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


(defn random-color [] (rand-nth (vec (:colors css))))


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
  (into {:color (random-color)}
        (->> shapes vec rand-nth
             (iterate transpose)
             (take (inc (rand-int 10)))
             vec
             last)))


(defn with-index [coll]
  (map-indexed (fn [i v] [i v]) coll))


(defn random-offset [{:keys [width]}]
  (rand-int (- grid-columns width 1)))


(defn grid-with-new-shape [grid {:keys [fields width color]} {:keys [row col]}]
  (reduce
   (fn [acc [idx bit]]
     (let [[r c]  (row-col idx width)
           cell-index (+ c col (* (+ r row) grid-columns))
           current    (get grid cell-index)]
       (when (and (nil? current) acc)
         (if (= bit 0)
           acc
           (assoc acc cell-index {:color color})))))
   grid (with-index fields)))


(defn cell-swap [grid idx1 idx2]
  (let [v1 (get grid idx1)
        v2 (get grid idx2)]
    (-> grid
        (assoc idx1 v2)
        (assoc idx2 v1))))


(defn move-shape-down [grid {:keys [width fields]} {:keys [row col]}]
  (let [new-grid
        (reduce
         (fn [acc [idx bit]]
           ;; (prn acc idx bit)
           (when acc
             (let [[r c] (row-col idx width)
                   old-index (+ col c (* (+ r row) grid-columns))
                   new-index (+ old-index grid-columns)
                   candidate (get acc new-index :out-of-grid)]
               (when (or (nil? candidate) (= bit 0))
                 (if (= bit 1)
                   (cell-swap acc new-index old-index)
                   acc)))))
         grid (reverse (with-index fields)))]
    {:grid new-grid
     :offset {:row ((if new-grid inc identity) row)
              :col col}}))


(defn move-shape-left [grid {:keys [width fields]} {:keys [row col]}]
  (let [new-grid
        (reduce
         (fn [acc [idx bit]]
           (when acc
             (let [[r c] (row-col idx width)
                   old-index (+ col c (* (+ r row) grid-columns))
                   new-col (dec (+ col c))
                   new-index (+ new-col (* (+ r row) grid-columns))
                   candidate (get acc new-index :out-of-grid)]
               (when (and (<= 0 new-col) (or (nil? candidate) (= bit 0)))
                 (if (= bit 1)
                   (cell-swap acc new-index old-index)
                   acc)))))
         grid (with-index fields))]
    {:grid (or new-grid grid)
     :offset {:row row
              :col ((if new-grid dec identity) col)}}))


(defn move-shape-right [grid {:keys [width fields]} {:keys [row col]}]
  (let [new-grid
        (reduce
         (fn [acc [idx bit]]
           (when acc
             (let [[r c] (row-col idx width)
                   old-index (+ col c (* (+ r row) grid-columns))
                   new-col (inc (+ col c))
                   new-index (+ new-col (* (+ r row) grid-columns))
                   candidate (get acc new-index :out-of-grid)]
               (when (and (< new-col grid-columns) (or (nil? candidate) (= bit 0)))
                 (if (= bit 1)
                   (cell-swap acc new-index old-index)
                   acc)))))
         grid (reverse (with-index fields)))]
    {:grid (or new-grid grid)
     :offset {:row row
              :col ((if new-grid inc identity) col)}}))


(defn grid-without-shape [grid {:keys [width fields]} {:keys [row col]}]
  (reduce
   (fn [acc [idx _]]
     (let [[r c] (row-col idx width)
           index (+ c col (* (+ r row) grid-columns))]
       (assoc acc index nil)))
   grid (with-index fields)))


(defn rotate-shape [old-grid shape offset]
  (let [new-shape (transpose shape)
        grid (grid-without-shape old-grid shape offset)
        new-grid (grid-with-new-shape grid new-shape offset)]
    {:grid  (or new-grid old-grid)
     :shape (if new-grid new-shape shape)}))


(comment
  (let [shape (into {:color (random-color)} (first shapes))
        offset {:row 0 :col 0}
        grid (grid-with-new-shape (initial-grid) shape offset)]
    (rotate-shape grid shape offset)))


(def rules
  (o/ruleset
   {::on-down
    [:what
     [::global ::event ::down]
     [::global ::grid grid {:then false}]
     [::global ::offset offset {:then false}]
     [::global ::shape shape {:then false}]
     [::global ::status status {:then false}]
     :when
     (= status :active)
     :then
     (let [{new-grid :grid offset :offset} (move-shape-down grid shape offset)]
       (if new-grid
         (o/insert! ::global {::grid new-grid
                              ::offset offset
                              ::message "Down"})
         (o/insert! ::global ::event ::add)))]

    ::on-left
    [:what
     [::global ::event ::left]
     [::global ::grid grid {:then false}]
     [::global ::offset offset {:then false}]
     [::global ::shape shape {:then false}]
     [::global ::status status {:then false}]
     :when
     (= status :active)
     :then
     (let [{new-grid :grid offset :offset} (move-shape-left grid shape offset)]
       (o/insert! ::global {::grid    new-grid
                            ::offset  offset
                            ::message "Left"}))]

    ::on-right
    [:what
     [::global ::event ::right]
     [::global ::grid grid {:then false}]
     [::global ::offset offset {:then false}]
     [::global ::shape shape {:then false}]
     [::global ::status status {:then false}]
     :when
     (= status :active)
     :then
     (let [{new-grid :grid offset :offset} (move-shape-right grid shape offset)]
       (o/insert! ::global {::grid    new-grid
                            ::offset  offset
                            ::message "Right"}))]

    ::on-rotate
    [:what
     [::global ::event ::rotate]
     [::global ::grid grid {:then false}]
     [::global ::offset offset {:then false}]
     [::global ::shape shape {:then false}]
     [::global ::status status {:then false}]
     :when
     (= status :active)
     :then
     (let [{new-grid :grid new-shape :shape} (rotate-shape grid shape offset)]
       (o/insert! ::global {::grid    new-grid
                            ::shape   new-shape
                            ::message "Rotate"}))]

    ::on-restart
    [:what
     [::global ::event ::restart]
     :then
     (let [shape (random-shape)
           offset {:row 0 :col (random-offset shape)}
           grid (grid-with-new-shape (initial-grid) shape offset)]
       (o/insert! ::global {::grid grid
                            ::offset offset
                            ::shape shape
                            ::message "New game"
                            ::status :active}))]

    ::on-add
    [:what
     [::global ::event ::add]
     [::global ::grid grid {:then false}]
     [::global ::status status {:then false}]
     :when
     (= status :active)
     :then
     (let [shape (random-shape)
           offset {:row 0 :col (random-offset shape)}
           new-grid (grid-with-new-shape grid shape offset)]
       (o/insert! ::global
                  (if new-grid {::grid    new-grid
                                ::offset  offset
                                ::shape   shape
                                ::message "Added"}
                      {::message "Game over"
                       ::status :finished})))]

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
        (grid-comp)])]

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
        [:button {:class (:button css) :on-click (handler ::right)} "Right"]
        [:button {:class (:button css) :on-click (handler ::add)} "Add"]])]

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
          [:div {:key (str "cell-" i)
                 :class (conj (:cell css) (or (:color cell) "bg-gray-100"))}
           i])
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
