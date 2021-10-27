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
    (js/console.log "longest" lengths)
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

(js/console.log (clj->js (generate-strings job-names 50)))

(defn component-job-board [state]
  [:section
   [:header
    [:h1 "Procedurally generated jobs"]
    [:h3 "You've come to the right place my friend"]]
   (for [job (:jobs @state)]
     [:div.card
      [:h3 (:name job)]
      [:p "Salary: " (:salary job)]
      [:button "apply"]    
      [:p "Experience: " (:experience job)]])])

(defn component-game [state]
  [:section#game.screen
   "This is a game."
   ]
  )

(defn component-title [state]
  [:section#title.screen
   [:img#title {:src "title.png"}]
   [:button {:on-click #(swap! state assoc :screen :game)} "Play"]])

(defn component-main [state]
  [:main
   (case (:screen @state)
     :jobs [component-job-board state]
     :game [component-game state]
     [component-title state])])

(defn start {:dev/after-load true} []
  (swap! state assoc :jobs (make-jobs 50))
  (rdom/render [component-main state]
               (js/document.getElementById "app")))

(defn init []
  (start))
