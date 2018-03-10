(ns sudoku.core
  (:require [sudoku.io :as sio])
  (:require [sudoku.board :as sb])
  (:gen-class))

(defn -main
  [file-path & args]
  (let [board (sio/read-board file-path)]
    (do
      (sio/print-board board)
      (println)
      (let [solved-board (sb/solve board)]
        (if (nil? solved-board)
          (println "No solution")
          (sio/print-board solved-board))))))
