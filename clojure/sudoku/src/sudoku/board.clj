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

(defn remove-value
  [board cell value]
  (let [vals (get-in board [:cells cell])]
    (if (contains? vals value)
      (let [next-vals (disj vals value)]
        {:board (assoc-in board [:cells cell] next-vals)
         :single-val (when (= 1 (count next-vals)) (first next-vals))})
      {:board board
       :single-val nil})))

(defn set-value
  [board cell value]
  (defn set-val-int
    [board cell-vals]
    (if (nil? (seq cell-vals))
      board
      (let [[c v] (first cell-vals)
            siblings (layout/siblings (:layout board) c)
            {:keys [board cell-vals]} (reduce
                                        (fn [acc sibling]
                                          (let [{:keys [board single-val]} (remove-value (:board acc) sibling v)]
                                            (if (nil? single-val)
                                              (assoc acc :board board)
                                              (assoc acc :board board :cell-vals (conj (:cell-vals acc) [sibling single-val])))))
                                        {:board (assoc-in board [:cells c] #{v})
                                         :cell-vals (rest cell-vals)}
                                        siblings)]
        (recur board cell-vals))))
  (set-val-int board [[cell value]]))



(-> (make-board [2 2])
    (set-value [0 0] 1)
    (set-value [0 1] 2)
    (set-value [1 0] 3))



(find-free-cell b)
