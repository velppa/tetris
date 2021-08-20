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
  {:button #{"justify-center" "p-4"
             "m-4" "border"
             "border-transparent" "text-base"
             "font-medium" "rounded-md"
             "text-indigo-700" "bg-indigo-100"
             "hover:bg-indigo-200"
             "md:text-lg" "md:p-2"}
   :cell #{"w-8" "h-8"
           "flex"}
   :colors #{"bg-red-800" "bg-green-800" "bg-blue-800" "bg-pink-800"}})


(defn random-color [] (rand-nth (vec (:colors css))))


(def grid-columns 12)


(def grid-rows 19)


(defn shape-height [{:keys [fields width]}]
  (/ (count fields) width))


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


(defn grid-with-shape
  "Inserts SHAPE into GRID in the top-left corner at OFFSET.

  If any of cells is alredy occupied or shape goes beyound the grid,
  returns nil."
  [grid {:keys [fields width color] :as shape} {:keys [row col] :as offset}]
  (when (and (<= 0 row)
             (<= 0 col)
             (<= (+ col width) grid-columns)
             (<= (+ row (shape-height shape)) grid-rows))
      (reduce
       (fn [acc [idx bit]]
         (let [[r c]      (row-col idx width)
               cell-index (+ c col (* (+ r row) grid-columns))
               current    (get grid cell-index)]
           (when (and (or (nil? current) (= bit 0)) acc)
             (if (= bit 0)
               acc
               (assoc acc cell-index {:color color})))))
       grid (with-index fields))))


(defn grid-without-shape [grid {:keys [width fields]} {:keys [row col]}]
  (reduce
   (fn [acc [idx bit]]
     (let [[r c] (row-col idx width)
           index (+ c col (* (+ r row) grid-columns))]
       (if (= bit 0) acc (assoc acc index nil))))
   grid (with-index fields)))


(defn inc-row [{:keys [row col]}] {:row (inc row) :col col})
(defn inc-col [{:keys [row col]}] {:row row :col (inc col)})
(defn dec-col [{:keys [row col]}] {:row row :col (dec col)})


(defn move-shape [mover grid shape offset]
  (let [new-grid (-> grid
                     (grid-without-shape shape offset)
                     (grid-with-shape shape (mover offset)))]
    {:grid (or new-grid grid)
     :offset ((if new-grid mover identity) offset)}))


(def move-shape-right (partial move-shape inc-col))
(def move-shape-left (partial move-shape dec-col))
(def move-shape-down (partial move-shape inc-row))



(defn rotate-shape [old-grid shape offset]
  (let [new-shape (transpose shape)
        grid (grid-without-shape old-grid shape offset)
        new-grid (grid-with-shape grid new-shape offset)]
    {:grid  (or new-grid old-grid)
     :shape (if new-grid new-shape shape)}))


(comment
  (let [shape (into {:color (random-color)} (first shapes))
        offset {:row 0 :col 0}
        grid (grid-with-shape (initial-grid) shape offset)]
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
       (if (not= new-grid grid)
         (o/insert! ::global {::grid new-grid
                              ::offset offset
                              ::message "Down"})
         (o/insert! ::global ::event ::full-row)))]

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

    ::on-full-row
    [:what
     [::global ::event ::full-row]
     [::global ::grid grid {:then false}]
     :then
     (when (->> grid
                (partition grid-columns)
                (map #(every? some? %))
                (some true?))
       (let [rows       (partition grid-columns grid)
             with-nils  (filter #(some nil? %) rows)
             empty-row  (repeat grid-columns nil)
             empty-rows (repeat (- grid-rows (count with-nils)) empty-row)
             new-grid   (vec (flatten (concat empty-rows with-nils)))]
         (o/insert! ::global {::grid new-grid
                              ::message (str "Removed " (count empty-rows) " rows")})))
     (o/insert! ::global ::event ::add)]

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
           grid (grid-with-shape (initial-grid) shape offset)]
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
           new-grid (grid-with-shape grid shape offset)]
       (o/insert! ::global
                  (if new-grid
                    {::grid    new-grid
                     ::offset  offset
                     ::shape   shape
                     ::message "Added"}
                    {::message "Game over"
                     ::status :finished})))]

    ::get-offset
    [:what
     [::global ::offset offset]]

    ::get-grid
    [:what
     [::global ::grid grid]]

    ::get-event
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
        [:button {:class (:button css) :on-click (handler ::right)} "Right"]])]

    heading
    [:then
     [:h1 {:class "text-2xl font-bold"} "Tetris"]]

    grid-comp
    [:what
     [::global ::grid grid]
     :then
     [:div {:class "rounded-t-xl bg-gradient-to-r from-green-50 to-red-50 bg-white p-8"}
      [:div {:class (str "grid gap-0 grid-cols-" grid-columns)}
       (map-indexed
        (fn [i cell]
          [:div {:key (str "cell-" i)
                 :class (conj (:cell css) (or (:color cell) "bg-gray-100"))}
           ""])
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

  (o/query-all @*session ::get-grid)

  (->> (o/query-all @*session ::get-grid)
       first :grid
       (partition grid-columns)
       (map #(every? some? %))
       (some true?))

  (map (fn [{:keys [fields width]}]
         (/ (count fields) width)) shapes)

  (map (comp transpose transpose) shapes)

  (click-counter *session))


(defn main! [] (mount))
(defn reload! [] (mount))
