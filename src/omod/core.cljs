(ns omod.core (:require
    [clojure.edn :refer [read-string]]
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [shadow.resource :as rc]
    ["js-markov" :as markov]
    ["seedrandom" :as seedrandom]
    ["react-twemoji$default" :as tw]
    ["rot-js" :refer [RNG]]))

(def job-names (js/JSON.parse (rc/inline "data/jobs.json")))
(def bank-names (-> (rc/inline "data/banks.txt") (.split "\n")))
(def start-month (* 17 12))
(def domains ["tech" "retail" "hospitality"])
(def mortality-rate-per-year (read-string (rc/inline "data/mortality-rate-per-year.edn")))
(def job-board-size 10)
(def job-order (shuffle (range 10)))

(defonce state (r/atom {:screen :title}))

; *** randomness *** ;

(defn generate-strings [src n & [longest include]]
  (let [m (markov.)
        lengths (.map src #(or (aget % "length") 0))
        longest (or longest (apply js/Math.max lengths))]
    (.addStates m src)
    (.train m)
    (loop [out []]
      (if (< (count out) n)
        (recur
          (let [item (.generateRandom m longest)]
            (if (and (= (.indexOf src item) -1) (or (nil? include) (>= (.indexOf item include) 0))) 
              (conj out item)
              out)))
        out))))

(defn make-jobs [n]
  (let [names (generate-strings job-names n)]
    (map
      (fn [n]
        (let [experience (int (js/Math.max 0 (.getNormal RNG 10 15)))
              multiplier (js/Math.max 1 (.getNormal RNG 3 10))]
          {:name n
           :salary (int (* (inc experience) multiplier))
           :experience experience
           :uuid (random-uuid)}))
      names)))

(defn make-banks [n]
  (let [names (generate-strings bank-names n 20 "Bank")]
    (map
      (fn [n]
        {:type :bank
         :name n
         :icon (.getItem RNG #js ["💰" "🤑" "💳" "🧧 " "💵" "💱" "💴" "🏦" "🪙" "💶" "💷" "💸" "🎖️" "🍁" "🌿" "🐉" "🌲" "🌳"])
         :multiplier (* 0.25 (js/Math.random))
         :uuid (random-uuid)})
      names)))

(defn rng-int [a b]
  (let [diff (- b a)]
    (js/Math.floor
      (+
       (* (js/Math.random) diff)
       a))))

; https://stackoverflow.com/questions/664014/what-integer-hash-function-are-good-that-accepts-an-integer-hash-key#12996028
(defn h [x]
  (let [x (* (bit-xor (bit-shift-right x 16) x) 0x45d9f3b)
        x (* (bit-xor (bit-shift-right x 16) x) 0x45d9f3b)
        x (bit-xor (bit-shift-right x 16) x)]
    x))

; *** functions *** ;

(defn get-age [state]
  (let [d (-> state :game :month)
        b (-> state :game :birth-month)]
    (int (/ (- d b) 12))))

(defn update-tax-food [old-state]
  old-state)

(defn add-salary [old-state]
  (let [age (get-age old-state)
        job (-> old-state :game :job)
        tax-rate (-> old-state :game :tax-rate)
        savings-rate (-> old-state :game :savings-rate)
        food-price (-> old-state :game :food-price)
        food-cost (if (> age 18)
                    (* (js/Math.max 0 (.getNormal RNG 0.5 1)) food-price -1)
                    0)]
    (update-in old-state [:game :net-worth] #(+ % food-cost
                                                (when job
                                                  (*
                                                   savings-rate
                                                   (-> job :salary (/ 12) (* (- 1 tax-rate)))))))))

(defn update-xp [old-state]
  (let [job (-> old-state :game :job)]
    (if job
      (update-in old-state [:game :experience] #(+ % (/ 1 12)))
      old-state)))

(defn update-aliveness [old-state]
  (let [age (get-age old-state)
        mortality-rate (nth mortality-rate-per-year (js/Math.min age (count mortality-rate-per-year)))
        mortality-rate-monthly (/ mortality-rate 12)
        chance (js/Math.random)]
    (if (< chance mortality-rate-monthly)
      (assoc-in old-state [:game :outcome] :dead)
      old-state)))

(defn update-has-won [old-state]
  (let [net-worth (-> old-state :game :net-worth)]
    (if (>= net-worth 1000)
      (assoc-in old-state [:game :outcome] :rich)
      old-state)))

(defn bank-rate [old-state bank]
  (let [base-rate (-> old-state :game :interest-rate)
        rate (- base-rate (* (:multiplier bank) base-rate))]
    rate))

(def entity-allocation-fns
  {:bank (fn [old-state bank amount]
           (-> amount (* (bank-rate old-state bank)) (/ 12)))})

(defn compute-entity-allocations [old-state]
  (try
    (let [net-worth (-> old-state :game :net-worth)
          allocations (-> old-state :game :allocations)
          earnings (map (fn [[_uuid item]]
                          (let [entity (:entity item)
                                entity-type (:type entity)
                                earning-function (entity-type entity-allocation-fns)
                                amount (* (:percent item) 0.01 net-worth)]
                            (earning-function
                              old-state
                              entity
                              amount)))
                        allocations)
          total-earned (apply + earnings)]
      (if (> net-worth 0)
        (assoc-in old-state [:game :net-worth] (+ net-worth total-earned))
        old-state))
    (catch :default e (js/console.error e) old-state)))

(defn update-game-state [state interval]
  (when (:game @state)
    ; apply all the state updates
    (when (not (aget js/window "pause"))
      (swap! state
             #(-> %
                  update-aliveness
                  update-tax-food
                  add-salary
                  compute-entity-allocations
                  update-xp
                  update-has-won
                  (update-in [:game :month] inc))))
    ; stop the ticker once there has been an outcome
    (when (not (-> @state :game :outcome))
      (js/setTimeout #(update-game-state state interval) interval))))

(defn go-screen [state which]
  (swap! state assoc :screen which))

(defn start-game [state]
  (swap! state
         #(-> %
              (assoc
                :game
                {:birth-year (rng-int 1800 2100)
                 :birth-month (rng-int 0 12)
                 :month start-month
                 :net-worth 0
                 :play true
                 :job nil
                 :experience 0
                 :tax-rate 0.1
                 :interest-rate 0.05
                 :outcome nil
                 :savings-rate 0.15
                 :food-price (-> (js/Math.random) (* 0.5))
                 :jobs (make-jobs (* 12 150))
                 :banks (make-banks 5)})
              (assoc :coin-positions (map (fn [_i] [(js/Math.random) (js/Math.random)]) (range 30)))
              (assoc :screen :game)))
  (update-game-state state 1000))

(defn apply-for-job [state job]
  (let [got-the-job (and (>= (* (js/Math.random) (-> @state :game :experience) 1.1) (:experience job))
                         (or (>= (get-age @state) 16)
                             (> (js/Math.random) 0.9)))]
    (swap! state
           (fn [old-state]
             (let [new-state old-state
                   new-state (if got-the-job (assoc-in new-state [:game :job] (assoc job :status :got)) new-state)
                   new-state (update-in new-state [:game :jobs]
                                        (fn [jobs]
                                          (map #(if (= (:uuid %) (:uuid job))
                                                  (assoc job :status (if got-the-job :got :denied))
                                                  %)
                                               jobs)))]
               new-state)))))

(defn rebalance-portfolio-to [entity new-percent]
  (swap! state update-in [:game :allocations]
        (fn [allocations]
          (let [uuid (:uuid entity)
                old-percent (or (-> allocations (get uuid) :percent) 0)
                difference (- new-percent old-percent)
                total-allocations-percent (+ (apply + (map (fn [[_uuid entry]] (:percent entry)) allocations)) difference)
                other-allocations (remove (fn [[check-uuid _entry]] (= check-uuid uuid)) allocations)
                ; if difference > 0 reduce the other allocations
                reduced-allocations (when
                                      (and
                                        (> difference 0)
                                        (> total-allocations-percent 100)
                                        (> (count other-allocations) 0))
                                      (reduce
                                        (fn [new-allocations i]
                                          (update-in (vec new-allocations) [(mod i (count new-allocations)) 1 :percent] #(js/Math.max 0 (dec (js/parseInt %)))))
                                        (vec (shuffle other-allocations))
                                        (range difference)))
                reduced-allocations (when (> (count reduced-allocations) 0)
                                      (remove (fn [[_uuid entry]] (js/console.log "entry" (clj->js entry)) (= (:percent entry) 0)) reduced-allocations))
                allocations (if reduced-allocations (into {} reduced-allocations) allocations)]
            (if (> new-percent 0)
              (assoc allocations uuid {:entity entity
                                       :percent new-percent})
              (dissoc allocations uuid))))))

(defn exit-game [state]
  (swap! state dissoc :game)
  (go-screen state :title))

;*** user interface ***;

(defn display-date [state]
  (let [d (-> state :game :month)
        ds (str "2010-" (inc (mod d 12)))
        date (js/Date. ds)
        month-name (.toLocaleString date "default" (clj->js {:month "short"}))]
    month-name))

(defn component-net-worth [state]
  [:div "Net worth $" (-> @state :game :net-worth (.toFixed 0)) "k"])

(defn component-age [state]
  [:div (display-date @state) ", age " (get-age @state)])

(defn component-stat [state k txt]
  (let [v (-> @state :game k)]
    [:div txt ": "
     (if (< v 1)
       (str (* v 100) "%")
       (js/Math.floor v))]))

(defn component-stats [state]
  [:nav#stats
   [component-net-worth state]
   [:div "XP: " (-> @state :game :experience js/Math.floor)]
   [component-age state]])

(defn component-game-state [state]
  [:div#game-state
   [component-stats state]
   [:button {:on-click #(go-screen state :game)} "home"]])

(defn component-job-board [state]
  (let [month (or (-> @state :game :month) 0)
        has-job (-> @state :game :job)
        jobs (subvec (-> @state :game :jobs vec) month (+ month job-board-size))
        seed (-> @state :seed)]
    [:section#jobs.screen
     [component-game-state state]
     [:div
      [component-stat state :tax-rate "Taxation rate"]
      [component-stat state :savings-rate "Savings rate"]]
     (when has-job
       [:div.card.fill.parallelogram {:class (:status has-job)}
        [:h3 "Job: " (:name has-job)]
        [:p "Salary: " (:salary has-job) "k"]])
     [:header
      [:h1 "Job market"]]
     [:div#job-market
      (for [j job-order]
        (let [seed (-> seed (* 17) (+ j) js/Math.abs)
              rng (-> RNG .clone (.setSeed seed))
              job (nth jobs (mod (- j month) job-board-size))]
          [:div.card.parallelogram {:class (:status job)
                                    :key (:uuid job)}
           [:h3 (:name job)]
           [:p "Salary: " (:salary job) "k"]
           [:div.application
            (case (:status job)
              :denied [:p "You didn't get the job."]
              :got [:p "You got this job!"]
              [:button {:on-click #(apply-for-job state job)
                        :class (.getItem rng #js ["" "alt-1" "alt-2"])}
               "apply"])]
           [:p "Experience: " (str (:experience job) " years")]]))]]))

(defn component-stonks-board [state]
  [:section#stonks.screen
   [component-game-state state]
   [:header
    [:h1 "Stonks"]]])

(defn component-banks-board [state]
  [:section#banks.screen
   [component-game-state state]
   [:header
    [:h1 "Banks"]]
   (for [bank (-> @state :game :banks)]
     (let [rate (bank-rate @state bank)
           uuid (:uuid bank)]
       [:div.card.bank.fill {:key uuid}
        [:h3 [:> tw (:icon bank)] (:name bank)]
        [:p "Rate: " (-> rate (* 100) (.toFixed 2))]
        [:p "Allocate: "
         [:input {:type "range"
                  :min 0
                  :max 100
                  :value (or (-> @state :game :allocations (get uuid) :percent) 0)
                  :on-change #(rebalance-portfolio-to bank (js/parseInt (-> % .-target .-value)))}]]]))])

(defn component-houses-board [state]
  [:section#houses.screen
   [component-game-state state]
   [:header
    [:h1 "Houses"]]])

(defn component-wealth [state]
  [:div#wealth
    (let [net-worth (js/Math.round (-> @state :game :net-worth))
          coin-positions (-> @state :coin-positions)]
      (for [c (range net-worth)]
        (let [[x y] (nth coin-positions (mod c (count coin-positions)))]
          [:> tw {:key c
                  ;:margin-top (str "-" (* c 10) "px")
                  :style {"--offset-x" (* (- x 0.5) 400)
                          "--offset-y" (+ (* (- y 0.5) 400)
                                          (*
                                           (int (/ c (count coin-positions)))
                                           20))}
                  :class "coin"}
           "🪙"])))])

(defn component-game [state]
  [:section#game.screen
   [component-age state]
   [component-wealth state]
   [:div
    [component-net-worth state]]
   [:div
    [:div "Job: " (or (-> @state :game :job :name) "None")]
    [:button {:on-click #(go-screen state :jobs)}
     [:> tw "⚒️ job board"]]
    [:button {:on-click #(go-screen state :banks)}
     [:> tw "🏦 banks"]]
    [:button {:on-click #(go-screen state :stonks)
              :class "alt-1"}
     [:> tw "📈 stonks"]]
    [:button {:on-click #(go-screen state :houses)
              :class "alt-2"}
     [:> tw "🏠 houses"]]]
   [:button {:on-click #(exit-game state)} "quit"]])

(defn component-title [state]
  [:section#title.screen
   [:img#title {:src "title.png"}]
   [:button {:on-click #(start-game state)} "Play"]])

(defn component-end-of-game [state]
  [:section#end.screen
   [:div
    (if (= (-> @state :game :outcome) :rich)
      [:> tw {:options {:folder "svg" :ext ".svg"}} "🧐"
       [:p "You made a million!"]]
      [:> tw {:options {:folder "svg" :ext ".svg"}} "🪦"])
    [:div
     [:div "Age " (get-age @state)]
     [:div "Net worth $" (-> @state :game :net-worth (.toFixed 0)) "k"]]
    [:button {:on-click #(exit-game state)} "restart"]]])

(defn component-main [state]
  [:main
   (if (-> @state :game :outcome)
     [component-end-of-game state]
     (case (:screen @state)
       :jobs [component-job-board state]
       :stonks [component-stonks-board state]
       :banks [component-banks-board state]
       :houses [component-houses-board state]
       :game [component-game state]
       [component-title state]))])

(defn start {:dev/after-load true} []
  (rdom/render [component-main state]
               (js/document.getElementById "app")))

(defn init []
  (let [seed-from-url (-> (aget js/document "location" "search") (.slice 1))
        seed (if (= seed-from-url "")
               (-> (js/Math.random) str (.split ".") second int)
               seed-from-url)]
    (print "seed" seed)
    (swap! state assoc :seed seed)
    (seedrandom seed #js {:global true}))
  (start))
