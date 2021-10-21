(ns omod.core
  (:require
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [shadow.resource :as rc]
    ["js-markov" :as markov]))

(def job-names (js/JSON.parse (rc/inline "data/jobs.json")))
(js/console.log job-names)


(defonce state (r/atom {}))

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
  [:main
   [:header
    [:h1 "Procedurally generated jobs"]
    [:h3 "You've come to the right place my friend"]]
   (for [job (:jobs @state)]
     [:div.card
      [:h3 (:name job)]
      [:p "Salary: " (:salary job)]
      [:button "apply"]    
      [:p "Experience: " (:experience job)]])])

(defn component-main [state]
  [:div
   [:h1 "omod"]
   [:p "Welcome to the app!"]
   [:button {:on-click #(js/alert "Hello world!")} "click me"]])

(defn start {:dev/after-load true} []
  (swap! state assoc :jobs (make-jobs 50))
  (rdom/render [component-job-board state]
               (js/document.getElementById "app")))

(defn init []
  (start))
