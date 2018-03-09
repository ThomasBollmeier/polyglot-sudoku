(ns sudoku.board
  (:require [sudoku.layout :as layout]))

(defn make-board
  [layout]
  (let [size (layout/size layout)]
    {
      :layout layout
      :cells (reduce
               (fn [cells [row col]]
                 (assoc cells [row col] (set (range 1 (inc size)))))
               {}
               (layout/cells layout))}))

(defn find-free-cell
  [{:keys [cells]}]
  (let [free-cells (filter (fn [[pos values]] (> (count values) 1)) cells)]
    (-> (reduce
          (fn [{:keys [min-cell min-cnt] :as acc} [_ values :as cell]]
            (if (or (nil? min-cell) (< (count values) min-cnt))
              {:min-cell cell
               :min-cnt (count values)}
              acc))
          {:min-cell nil
           :min-cnt 0}
        free-cells)
        :min-cell)))


(def b (make-board [2 2]))

(find-free-cell b)
