(ns sudoku.layout)

(defn size
  [[nrows-area ncols-area]]
  (* nrows-area ncols-area))

(defn cells
  [[nrows-area ncols-area]]
  (let [size (* nrows-area ncols-area)]
    (for [row (range size)
          col (range size)]
      [row col])))

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

(defn groups
  [[nrows-area ncols-area :as layout]]
  (let [size (* nrows-area ncols-area)
        rows (map (partial row-cells layout) (range size))
        columns (map (partial column-cells layout) (range size))
        areas (for [x (range nrows-area)
                    y (range ncols-area)]
                (area-cells layout x y))]
    (-> rows
        (concat columns)
        (concat areas))))

(defn siblings
  [[nrows-area ncols-area :as layout] [row col]]
  (let [x (int (/ col ncols-area))
        y (int (/ row nrows-area))
        rcells (row-cells layout row)
        ccells (column-cells layout col)
        acells (area-cells layout x y)
        cells (concat rcells ccells acells)]
    (-> (set cells)
        (disj cells [row col]))))
