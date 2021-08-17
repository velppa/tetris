(ns main
  (:require [rum.core :as rum]
            [odoyle.rules :as o]
            [odoyle.rum :as orum]
            [taoensso.timbre :as log]))


(defn click [state]
  (-> state
      (o/insert ::global ::event ::click)
      o/fire-rules))


(def rules
  (o/ruleset
    {::on-click
     [:what
      [::global ::event ::click]
      [::global ::clicks clicks {:then false}]
      :then
      (o/insert! ::global ::clicks (inc clicks))]
     ::get-clicks
     [:what
      [::global ::clicks clicks]]}))


(def components
  (orum/ruleset
    {click-counter
     [:what
      [::global ::clicks clicks]
      :then
      (let [*session (orum/prop)
            *local (orum/atom 10)]
        [:div
         [:button {:on-click (fn [_]
                               (swap! *session click)
                               (swap! *local inc))}
          (str "Clicked " clicks " " (if (= 1 clicks) "time" "times"))]
         [:p (str "Local: " @*local)]])]}))


(def *session
  (-> (reduce o/add-rule (o/->session) (concat rules components))
      (o/insert ::global ::clicks 0)
      o/fire-rules
      atom))


(defn mount []
  (rum/mount (click-counter *session) (js/document.querySelector "#app")))

(o/query-all @*session ::on-click)

(defn main! []
  (mount))

(defn reload! []
  (mount))
