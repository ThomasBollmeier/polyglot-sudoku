(ns sudoku.layout)

(defn cells
  [[nrows-area ncols-area]]
  (let [size (* nrows-area ncols-area)]
    (for [row (range size)
          col (range size)]
      [row col])))

(defn groups
  [layout]
  (let [[nrows-area ncols-area] layout
        size (* nrows-area ncols-area)
        rows (map (partial row-cells layout) (range size))
        columns (map (partial column-cells layout) (range size))
        ;; TODO: ...areas...
        ]
    (->> rows
         (concat columns))))

(defn row-cells
  [[nrows-area ncols-area] row]
  (let [size (* nrows-area ncols-area)]
    (for [col (range size)]
      [row col])))

(defn column-cells
  [[nrows-area ncols-area] col]
  (let [size (* nrows-area ncols-area)]
    (for [row (range size)]
      [row col])))

(defn area-cells
  [[nrows-area ncols-area] x y]
  (let [r (* y nrows-area)
        c (* x ncols-area)]
    (for [dc (range ncols-area)
          dr (range nrows-area)]
      [(+ r dr) (+ c dc)])))
