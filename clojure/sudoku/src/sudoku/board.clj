(ns sudoku.board
  (:require [clojure.set :as cs])
  (:require [sudoku.layout :as layout]))

(defn make-board
  [layout]
  (let [size (layout/size layout)]
    {:layout layout
     :fixed {}}))

(defn find-free-cell
  [{:keys [layout fixed]}]
  (let [fixed-cells (set (keys fixed))
        free-cells (->> (layout/cells layout)
                        (remove fixed-cells))]
    (when-not (empty? free-cells)
      (first free-cells))))

(defn candidates
  [{:keys [layout fixed]} cell]
  (let [fixed-cells (set (keys fixed))]
    (if (contains? fixed-cells cell)
      #{(get fixed cell)}
      (let [siblings (layout/siblings layout cell)]
        (reduce (fn [acc sibling]
                  (if (contains? fixed-cells sibling)
                    (disj acc (get fixed sibling))
                    acc))
                (set (range 1 (inc (layout/size layout))))
                siblings)))))

(defn set-value
  [{:keys [fixed] :as board} cell value]
  (assoc board :fixed (assoc fixed cell value)))

(defn solve
  [board]
  (defn group-valid?
    [{:keys [layout fixed] :as board} grp]
    (let [exp-num-distinct (layout/size layout)
          vals (reduce (fn [acc cell]
                         (if (contains? fixed cell)
                           (conj acc (get fixed cell))
                           acc))
                       #{}
                      grp)]
      (= (count vals) exp-num-distinct)))
  (defn valid?
    [{:keys [layout] :as board}]
    (let [grps (layout/groups layout)]
      (every? (partial group-valid? board) grps)))
  (defn solve-int
    [placements]
    (let [[board cell values] (first placements)
          value (first values)
          remaining (rest values)
          plcmts (if (empty? remaining)
                   (rest placements)
                   (cons [board cell remaining] (rest placements)))
          next-board (set-value board cell value)
          next-cell (find-free-cell next-board)]
      (if (nil? next-cell)
        (if (valid? next-board)
          next-board ; => solution found
          (when-not (empty? plcmts)
            (recur plcmts)))
        (let [next-values (candidates next-board next-cell)]
          (if (empty? next-values)
            (when-not (empty? plcmts)
              (recur plcmts))
            (recur (cons [next-board next-cell next-values] plcmts)))))))
  (if-let [cell (find-free-cell board)]
    (let [values (candidates board cell)]
      (if (empty? values)
        (when (valid? board) board)
        (solve-int [[board cell values]])))
    (when (valid? board) board)))
