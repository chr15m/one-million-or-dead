(ns omod.core
  (:require
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [shadow.resource :as rc]
    ["js-markov" :as markov]))

(def job-names (js/JSON.parse (rc/inline "data/jobs.json")))
(js/console.log job-names)

(defonce state (r/atom {:screen :title}))

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
        {:name n
         :salary (str (int (* (js/Math.random) 200)) "k")
         :experience (str (int (* (js/Math.random) 10)) " years")})
      names)))

(defn start-ticker [ticker state interval]
  (when ticker
    (js/clearInterval ticker))
  (js/setInterval
    #(swap! state update-in [:game :month] inc)
    interval))

(defn go-screen [state which]
  (swap! state assoc :screen which))

(defn rng-int [a b]
  (let [diff (- b a)]
    (js/Math.floor
      (+
       (* (js/Math.random) diff)
       a))))

(defn start-game [state]
  (swap! state
         #(-> %
              (assoc
                :game
                {:birth-year (rng-int 1800 2100)
                 :birth-month (rng-int 0 12)
                 :month 156
                 :net-worth 0
                 :play true
                 :jobs (make-jobs 1000)})
              (assoc :screen :game)
              (update-in [:ticker] start-ticker state 1000))))

;*** user interface ***;

(defn display-date [state]
  (let [d (-> state :game :month)
        ds (str "2010-" (inc (mod d 12)))
        date (js/Date. ds)
        month-name (.toLocaleString date "default" (clj->js {:month "short"}))]
    month-name))

(defn display-age [state]
  (let [d (-> state :game :month)
        b (-> state :game :birth-month)]
    (int (/ (- d b) 12))))

(defn component-nav [state]
  [:nav
   [:button {:on-click #(go-screen state :game)} "home"]
   [:button {:on-click #(go-screen state :jobs)} "jobs"]
   [:button {:on-click #(go-screen state :title)} "quit"]])

(defn component-stats [state]
  [:nav#stats
   [:div "Net worth $" (-> @state :game :net-worth)]
   [:div "Job: " (or (:job @state) "None")]
   [:div (display-date @state) ", age " (display-age @state)]])

(defn component-game-state [state]
  [:div#game-state
   [component-stats state]
   [component-nav state]])

(defn component-job-board [state]
  [:section#jobs.screen
   [component-game-state state]
   [:header
    [:h1 "Job market"]]
   (for [job (-> @state :game :jobs)]
     [:div.card.fill
      [:h3 (:name job)]
      [:p "Salary: " (:salary job)]
      [:button "apply"]    
      [:p "Experience: " (:experience job)]])])

(defn component-game [state]
  [:section#game.screen
   [component-game-state state]
   [:div "This is a game."]])

(defn component-title [state]
  [:section#title.screen
   [:img#title {:src "title.png"}]
   [:button {:on-click #(start-game state)} "Play"]])

(defn component-main [state]
  [:main
   (case (:screen @state)
     :jobs [component-job-board state]
     :game [component-game state]
     [component-title state])])

(defn start {:dev/after-load true} []
  (rdom/render [component-main state]
               (js/document.getElementById "app")))

(defn init []
  (start))
