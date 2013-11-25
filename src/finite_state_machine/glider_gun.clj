(ns finite-state-machine.glider-gun
  (:require [seesaw.core :refer :all]
            [seesaw.graphics :refer :all]
            [seesaw.color :refer :all]
            [finite-state-machine.core :refer :all]))


(defn- make-grid [rows cols]
  (vec (repeat rows (vec (repeat cols false)))))

(def grid (make-grid 36 9))

(defn game-of-life-cell-transition [neighbours]
  (let [cell (get-in neighbours [1 1])
        filtered-neighbours (assoc-in neighbours [1 1] nil)
        num-neighbours (reduce #(if %2 (inc %) %) 0 (reduce concat filtered-neighbours))]
    (condp < num-neighbours
      3 nil
      2 true
      1 cell
      nil)))

(def seed-vec
  [[- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -]
   [- - - - - - - - - - - - - - - - - - - - - - - - - 0 - - - - - - - - - - - -]
   [- - - - - - - - - - - - - - - - - - - - - - - 0 - 0 - - - - - - - - - - - -]
   [- - - - - - - - - - - - - 0 0 - - - - - - 0 0 - - - - - - - - - - - - 0 0 -]
   [- - - - - - - - - - - - 0 - - - 0 - - - - 0 0 - - - - - - - - - - - - 0 0 -]
   [- 0 0 - - - - - - - - 0 - - - - - 0 - - - 0 0 - - - - - - - - - - - - - - -]
   [- 0 0 - - - - - - - - 0 - - - 0 - 0 0 - - - - 0 - 0 - - - - - - - - - - - -]
   [- - - - - - - - - - - 0 - - - - - 0 - - - - - - - 0 - - - - - - - - - - - -]
   [- - - - - - - - - - - - 0 - - - 0 - - - - - - - - - - - - - - - - - - - - -]
   [- - - - - - - - - - - - - 0 0 - - - - - - - - - - - - - - - - - - - - - - -]
   [- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -]
   [- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -]
   [- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -]
   [- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -]
   [- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -]
   [- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -]
   [- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -]
   [- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -]
   [- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -]
   [- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -]])

(def seed (mapv #(mapv {- nil
                        0 true} %) seed-vec))

(defn cell-neighbours [row col configuration]
  (let [deltas (for [r [-1 0 1] c [-1 0 1]] [r c])
        inds (mapv #(map + %1 %2) deltas (repeat [row col]))]
    (vec (map vec (partition 3 (mapv #(get-in configuration %) inds))))))

(defn- gol-tr-fn* [current-state]
  (letfn [(tr-row [row-nr row] (vec (map-indexed
                                      (fn [col-nr _]
                                        (cell-neighbours row-nr col-nr current-state))
                                      row)))]
    (let [neighbours (vec (map-indexed tr-row current-state))
          new-state (mapv #(mapv game-of-life-cell-transition %) neighbours)]
      new-state)))

(defn- gol-tr-fn [current-state-atom _]
  (swap! current-state-atom gol-tr-fn*)
  current-state-atom)

(def glider-gun (finite-state-machine (atom seed) gol-tr-fn))

(defn- get-state [] @(:state glider-gun))

;;; Graphics

(def cell-size 20)

(defn cell-style [alive?] (if alive?
                            (style :background "#cc0000"
                                   :stroke (stroke :width 2 :cap :round))
                            (style :background "#000000"
                                   :stroke (stroke :width 2 :cap :round))))

(defn- paint-cells [c g]
  (try
    (let [state (deref (:state glider-gun))
          w (.getWidth c)
          h (.getHeight c)]
      (doseq [row state]
        (translate g 0 cell-size)
        (push g
              (doseq [alive? row]
                (translate g cell-size 0)
                (draw g (rect 0 0 cell-size) (cell-style alive?))))))
    (catch Exception e
      (println e))))



(defn -main [& args]
  (let [cvs (canvas :id :canvas :background "#000000" :paint paint-cells)
        t (timer (fn [e] (repaint! cvs)) :delay 100)]
    (init glider-gun (atom seed))
    (timer (fn [e] (transition glider-gun :next-gen)) :delay 100)
    (frame
      :visible? true
      :title "Game of Life: Glider Gun"
      :width 880 :height 600
      :on-close :exit
      :content cvs)))





