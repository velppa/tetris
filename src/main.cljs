(ns main
  (:require [rum.core :as rum]
            [odoyle.rules :as o]
            [odoyle.rum :as orum]
            [taoensso.timbre :as log])
  (:require-macros [odoyle.rum :as orum]))


(defn fire-event [event state]
  (-> state
      (o/insert ::global ::event event)
      o/fire-rules))


(def shapes
  #{{:width 4 :fields [1 1 1 1]}
    {:width 2 :fields [1 1
                       1 1]}
    {:width 3 :fields [1 1 0
                       0 1 1]}
    {:width 3 :fields [0 1 1
                       1 1 0]}
    {:width 3 :fields [1 1 1
                       0 1 0]}
    {:width 3 :fields [0 1 0
                       1 1 1]}
    {:width 3 :fields [1 1 1
                       0 0 1]}
    {:width 3 :fields [1 1 1
                       1 0 0]}})


(def css
  {:button #{"justify-center" "p-3"
             "m-4" "border"
             "border-transparent" "text-base"
             "font-medium" "rounded-md"
             "text-indigo-700" "bg-indigo-100"
             "hover:bg-indigo-200" "md:py-4"
             "md:text-lg" "md:px-10"}
   :cell #{"h-12" "rounded-md"
           ;; "w-4" "h-4"
           "flex" "items-center"
           "justify-center" "text-2xl"
           "font-extrabold"}
   :colors #{"bg-red-800" "bg-green-800" "bg-blue-800" "bg-pink-800"}})


(defn rand-color [] (rand-nth (vec (:colors css))))


(def grid-columns 10)


(def grid-rows 9)


(defn row-col [idx width]
  (let [row (int (/ idx width))
        col (- idx (* row width))]
    [row col]))


(defn initial-grid []
  (vec (repeat (* grid-rows grid-columns) nil)))


(defn transpose [{:keys [fields width] :as shape}]
  (into
   shape
   (let [height (/ (count fields) width)]
     (condp = width
       1 {:width height
          :fields fields}
       2 (condp = height
           2 shape
           3 (let [[a b
                    c d
                    e f] fields]
               {:width 3
                :fields [e c a
                         f d b]}))
       3 (let [[a b c
                d e f] fields]
           {:width 2
            :fields [d a
                     e b
                     f c]})
       4 {:width 1
          :fields fields}))))


(defn random-shape []
  (->> shapes vec rand-nth
       (iterate transpose)
       (take (inc (rand-int 10)))
       vec
       last))


(defn grid-with-new-shape [grid]
  (let [new-color (rand-color)
        {:keys [fields width]} (random-shape)
        x-offset (rand-int (- grid-columns width 1))]
    {:grid (reduce
            (fn [acc [idx bit]]
              (let [[row col] (row-col idx width)
                    cell-index (+ x-offset (* row grid-columns) col)
                    current (get grid cell-index)
                    _ (prn "cell-index: " cell-index "current:" current)]
                (when (and (nil? current) acc)
                  (if (= bit 0)
                    acc
                    (assoc acc cell-index {:color new-color})))))
            grid (map-indexed (fn [i v] [i v]) fields))
     :offset {:row 0
              :col x-offset}}))



(def rules
  (o/ruleset
   {::on-down
    [:what
     [::global ::event ::down]
     [::global ::grid grid {:then false}]
     :then
     (let [{new-grid :grid offset :offset} (grid-with-new-shape grid)]
       (if new-grid
         (o/insert! ::global {::grid new-grid
                              ::offset offset
                              ::message "Game over"})))]

    ::on-restart
    [:what
     [::global ::event ::restart]
     :then
     (let [{grid :grid offset :offset} (grid-with-new-shape (initial-grid))]
       (o/insert! ::global {::grid grid
                            ::offset offset
                            ::message "New game"}))]

    ::get-offset
    [:what
     [::global ::offset offset]]

    ::get-last-event
    [:what
     [::global ::event event]]}))


(def components
  (orum/ruleset
   {app-root
    [:then
     (let [*session (orum/prop)]
       [:div {:class "p-10"}
        (heading)
        (message-box {:*session *session})
        (buttons-box {:*session *session})
        (grid-comp {:*session *session})])]

    message-box
    [:what
     [::global ::message message]
     :then
     [:p message]]

    buttons-box
    [:then
     (let [{:keys [*session]} (orum/prop)
           handler (fn [event] #(swap! *session (partial fire-event event)))]
       [:div
        [:button {:class (:button css) :on-click (handler ::restart)} "Restart"]
        [:button {:class (:button css) :on-click (handler ::down)} "Down"]
        [:button {:class (:button css) :on-click (handler ::rotate)} "Rotate"]
        [:button {:class (:button css) :on-click (handler ::left)} "Left"]
        [:button {:class (:button css) :on-click (handler ::right)} "Right"]])]

    heading
    [:then
     [:h1 {:class "text-2xl font-bold"} "Tetris"]]

    grid-comp
    [:what
     [::global ::grid grid]
     :then
     [:div {:class "rounded-t-xl bg-gradient-to-r from-green-50 to-red-50 bg-white p-8"}
      [:div {:class (str "grid gap-2 grid-cols-" grid-columns)}
       (map-indexed
        (fn [i cell]
          [:div {:class (conj (:cell css) (or (:color cell) "bg-gray-100"))}
           " "])
        grid)]]]}))


(def initial-session
  {::message "Welcome!"
   ::grid (initial-grid)
   ::offset {:row 0 :col 0}})


(def *session
  (-> (reduce o/add-rule (o/->session) (concat rules components))
      (o/insert ::global initial-session)
      o/fire-rules
      atom))

(defn mount []
  (rum/mount (app-root *session) (js/document.querySelector "#app")))


(comment
  (o/query-all @*session ::get-last-event)

  (o/query-all @*session)

  (map (fn [{:keys [fields width]}]
         (/ (count fields) width)) shapes)

  (map (comp transpose transpose) shapes)

  (click-counter *session))


(defn main! [] (mount))
(defn reload! [] (mount))
