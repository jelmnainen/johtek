(ns ristinolla.core
  (:gen-class))

(def game-board
  [
    ["O", "X", ""]
    ["X", "", "X"]
    ["", "O", ""]])


(def test-board-1
  [
    ["O", "X", "X"]
    ["X", "", "X"]
    ["", "O", ""]])


(def O-wins-board
  [
    ["O", "X", "O"]
    ["O", "O", "X"]
    ["X", "X", "O"]])

(def X-wins-board
  [
    ["O", "X", "O"]
    ["O", "X", "X"]
    ["X", "X", "O"]])

(def tie-board
  [
    ["O", "X", "O"]
    ["X", "O", "X"]
    ["X", "O", "X"]])

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn winning-row?
  "score a row"
  [board-row]
  (let [m (first board-row)]
    (loop [row board-row
           mark m]
       (cond
         (empty? row) true
         (empty? (first row)) false
         (= (first row) mark) (recur (rest row) mark)
         :else false))))

(winning-row? (first test-board-2))

(defn matrice->nth-column
  "Takes a matrice and retuns x:th column as a vector"
  [x matrice]
  (loop [n x
         a-mat matrice
         result []]
    (cond
      (empty? a-mat) result
      :else (recur n (rest a-mat) (conj result (get (first a-mat) n))))))

(defn three-wide-matrice->diagonals
  "Takes a 3x3 matrice and returns the diagonals as a 2x3 matrice"
  [matrice]
  (let [first-row (get matrice 0)
        second-row (get matrice 1)
        third-row (get matrice 2)]
    [
      [(get first-row 0), (get second-row 1), (get third-row 2)],
      [(get first-row 2), (get second-row 1), (get third-row 0)]]))

(defn get-potential-winning-rows
  "Takes a 3x3 matrice and returns a new matrice with potential winning rows as vectors"
  [matrice]
  (let [diagonals (three-wide-matrice->diagonals matrice)]
    (conj matrice
      (matrice->nth-column 0 matrice)
      (matrice->nth-column 1 matrice)
      (matrice->nth-column 2 matrice)
      (get diagonals 0)
      (get diagonals 1))))

(defn declare-winner
  "Returns 1 if X wins, -1 if O wins"
  [winning-sign]
  (cond
    (= "X" winning-sign) 1
    (= "O" winning-sign) -1
    (= "N" winning-sign) 0
    :else false))

(defn get-winner-if-exists
  "Declares winner or returns false if neither wins"
  [board]
  (let [possible-wins (get-potential-winning-rows board)]
    (loop [rows possible-wins]
      (let [current-row (first rows)]
        (cond
          (empty? rows) false
          (winning-row? current-row) (declare-winner (first current-row))
          :else (recur (rest rows)))))))

(defn row-has-empty-spot?
  "Returns false if a vector has an empty string in it"
  [a-vec]
  (cond
    (empty? a-vec) false
    (empty? (first a-vec)) true
    :else (recur (rest a-vec))))

(defn has-empty-spots?
  "Returns true if there is still a place to play, false otherwise"
  [board]
  (let [current-row (row-has-empty-spot? (first board))]
    (cond
      (empty? board) false
      (identity current-row) current-row
      :else (recur (rest board)))))

(defn game-over?
  "Returns false if game is not over or declares winner"
  [board]
  (let [maybe-winner (get-winner-if-exists board)]
    (cond
      (identity maybe-winner) maybe-winner
      (has-empty-spots? board) false
      :else (declare-winner "N"))))

(defn play
  "Plays mark on [x, y] on board"
  [mark [x, y] board]
  (let [row (get board x)]
    (cond
      (empty? (get row y)) (assoc board x (assoc row y mark))
      :else board)))

(defn find-playable-y
  "Returns index of empty string in a vector or false"
  [vector]
  (loop [a-vec vector
         x 0]
    (cond
      (= "" (first a-vec)) x
      (empty? (first a-vec)) false
      :else (recur (rest a-vec) (inc x)))))

(defn next-playable-spot
  "Returns [x, y] of next playable spot or false if board is full"
  [board]
  (loop [a-board board, x 0]
    (cond
      (empty? board) false
      (row-has-empty-spot? (first a-board)) [x (find-playable-y (first a-board))]
      :else (recur (rest a-board) (inc x)))))

(defn alpha-beta-pruning
  "Use alpha-beta-pruning to compute a game of tic-tac-toe"
  [board]
  (let [current-row (first board)]
    (cond
      (empty? board) "The board is not valid"
      (min-value board))))


(defn min-value
  "Calculate minimum value"
  [the-board, alpha-value, beta-value]
  (let [maybe-winner (game-over? the-board)
        v-cand -2]
    (if
      (identity maybe-winner) maybe-winner
      (loop [board the-board
             alpha alpha-value
             beta beta-value
             v (min (max-value))]
        (cond
          (>= beta v) v)))))
