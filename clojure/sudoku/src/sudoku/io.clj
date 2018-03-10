(ns sudoku.io
  (:require [clojure.string :as str])
  (:require [sudoku.layout :as layout])
  (:require [sudoku.board :as board]))

(defn read-board
  [file-path]
  (defn rowstr->intlist
    [rowstr]
    (->> (str/split rowstr #"")
         (map #(Integer/parseInt %))))
  (defn values->cell-data
    [[row colvals]]
    (-> (reduce (fn [{:keys [col data] :as acc} colval]
                  (if (> colval 0)
                    (assoc acc :col (inc col)
                               :data (conj data [[row col] colval]))
                  (assoc acc :col (inc col))))
                {:col 0
                 :data []}
                colvals)
        :data))
  (let [content (slurp file-path)
        lines (->> (str/split content #"\n")
                   (map str/trimr))
        header (first lines)
        layout_ (->> (str/split header #" ")
                     (map #(Integer/parseInt %)))
        board_ (board/make-board layout_)
        values-to-set (->> (rest lines)
                        (map rowstr->intlist)
                        (map (fn [row cvals] [row cvals])
                             (range))
                        (map values->cell-data)
                        (apply concat))]
    (reduce (fn [b [cell value]]
              (board/set-value b cell value))
            board_
            values-to-set)))

(defn print-board
  [{:keys [layout fixed] :as board}]
  (defn print-row
    [row]
    (let [positions (layout/row-cells layout row)]
      (-> (apply str
                 (map #(get fixed % \.)
                      positions))
          println)))
  (let [size (layout/size layout)]
    (dotimes [row size]
      (print-row row))))

