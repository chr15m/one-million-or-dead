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
(def start-month 156)
(def domains ["tech" "retail" "hospitality"])
(def mortality-rate-per-year (read-string (rc/inline "data/mortality-rate-per-year.edn")))

(defonce state (r/atom {:screen :title}))

; *** randomness *** ;

(defn generate-strings [src n]
  (let [m (markov.)
        lengths (.map job-names #(or (aget % "length") 0))
        longest (apply js/Math.max lengths)]
    (.addStates m src)
    (.train m)
    (loop [out []]
      (if (< (count out) n)
        (recur
          (let [item (.generateRandom m longest)]
            (if (= (.indexOf src item) -1)
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

(defn rng-int [a b]
  (let [diff (- b a)]
    (js/Math.floor
      (+
       (* (js/Math.random) diff)
       a))))

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
        food-price (-> old-state :game :food-price)
        food-cost (if (> age 18)
                    (* (js/Math.max 0 (.getNormal RNG 0.5 1)) food-price -1)
                    0)]
    (update-in old-state [:game :net-worth] #(+ % food-cost (when job (-> job :salary (/ 12) (* (- 1 tax-rate))))))))

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

(defn update-game-state [state]
  ;(js/console.log "game state:" (clj->js @state))
  ; apply all the state updates
  (swap! state
         #(-> %
              update-aliveness
              update-tax-food
              add-salary
              update-xp
              update-has-won
              (update-in [:game :month] inc)))
  ; stop the ticker once there has been an outcome
  (when (-> @state :game :outcome)
    (js/clearInterval (:ticker @state))))

(defn start-ticker [ticker state interval]
  (when ticker
    (js/clearInterval ticker))
  (js/setInterval
    #(update-game-state state)
    interval))

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
                 :outcome nil
                 :food-price (js/Math.random)
                 :jobs (make-jobs (* 12 150))})
              (assoc :screen :game)
              (update-in [:ticker] start-ticker state 1000))))

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

(defn component-nav [state]
  [:nav
   [:button {:on-click #(go-screen state :game)} "home"]
   [:button {:on-click #(go-screen state :jobs)} "jobs"]
   [:button {:on-click #(exit-game state)} "quit"]])

(defn component-stats [state]
  [:nav#stats
   [:div "Net worth $" (-> @state :game :net-worth (.toFixed 0)) "k"]
   [:div "XP: " (-> @state :game :experience js/Math.floor)]
   [:div (display-date @state) ", age " (get-age @state)]])

(defn component-game-state [state]
  [:div#game-state
   [component-stats state]
   [component-nav state]])

(defn component-job-board [state]
  (let [month (or (-> @state :game :month) 0)
        len 10
        has-job (-> @state :game :job)
        jobs (subvec (-> @state :game :jobs vec) month (+ month len))]
    [:section#jobs.screen
     [component-game-state state]
     (when has-job
       [:div.card.fill.parallelogram {:class (:status has-job)}
        [:h3 "Job: " (:name has-job)]
        [:p "Salary: " (:salary has-job) "k"]])
     [:header
      [:h1 "Job market"]]
     (for [job jobs]
       [:div.card.fill.parallelogram {:class (:status job)
                                      :key (:uuid job)}
        [:h3 (:name job)]
        [:p "Salary: " (:salary job) "k"]
        [:div.application
         (case (:status job)
           :denied [:p "You didn't get the job."]
           :got [:p "You got this job!"]
           [:button {:on-click #(apply-for-job state job)} "apply"])]
        [:p "Experience: " (str (:experience job) " years")]])]))

(defn component-game [state]
  [:section#game.screen
   [component-game-state state]
   [:div "Job: " (or (-> @state :game :job :name) "None")]
   [:> tw "🎃 🥚 "]
   [:div "This is a game."]])

(defn component-title [state]
  [:section#title.screen
   [:img#title {:src "title.png"}]
   [:button {:on-click #(start-game state)} "Play"]])

(defn component-end-of-game [state]
  [:section#end.screen
   [:div
    (if (= (-> @state :game :outcome) :rich)
      [:> tw "💸"]
      [:> tw "🪦"])
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
       :game [component-game state]
       [component-title state]))])

(defn start {:dev/after-load true} []
  (rdom/render [component-main state]
               (js/document.getElementById "app")))

(defn init []
  (let [seed-from-url (-> (aget js/document "location" "search") (.slice 1))
        seed (if (= seed-from-url "")
               (-> (js/Math.random) str (.split ".") second)
               seed-from-url)]
    (print "seed" seed)
    (seedrandom seed #js {:global true}))
  (start))
