(ns autot.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn generate-node
  "Generates a new probability node"
  [& parent-probabilities]
  (if
    (> (rand) (reduce * parent-probabilities))
    0
    1))

(defn generate-car
  "Generates a new car"
  []
  (let [A (generate-node 0.9)
        R (generate-node A 0.9)
        S (generate-node A 0.95)
        B (generate-node 0.95)
        K (generate-node S B 0.99)
        L (generate-node K 0.99)]
    {:A A :R R :S S :B B :K K :L L}))

(defn generate-items
  "Generates x items"
  [generator x]
  (loop [gen generator
         n x
         item-list []]
     (if
       (>= 0 n) item-list
       (recur gen (dec n) (conj item-list (gen))))))

(defn a?
  "Returns true if a-map has :A value of 1"
  [a-map]
  (= 1 (:A a-map)))

(defn k?
  "returns true if a-map has :K value of 1"
  [a-map]
  (= 1 (:K a-map)))

(defn r-b-not-k?
  "Returns true if a-map has :R and :B values of 1 and :K value of 0"
  [a-map]
  (= 1 (:R a-map) (:B a-map) (inc (:K a-map))))

(defn r-s-b?
  "Returns true if a-map has :R, :S and :B values of 1"
  [a-map]
  (= 1 (:R a-map) (:S a-map) (:B a-map)))

(defn not-r-s-b?
  "Returns true if a-map has :R value of 0 but :S and :B values of 1"
  [a-map]
  (= 1 (inc (:R a-map)) (:S a-map) (:B a-map)))

(defn approximate-events-with-condition
  "Generate n events and with generator and determine how predicate condition
  contributes to predicate event"
  [generator n event condition]
  (let [generated-events (generator n)
        generated-events-with-event (count (filter event generated-events))
        events-with-condition (filter condition generated-events)
        events-with-condition-and-event (count (filter event events-with-condition))]
    [
      (/ generated-events-with-event (count generated-events))
      (/ events-with-condition-and-event (count events-with-condition))]))

(defn assignment-1-part-b
  "Helper for assignment 1 part b"
  [event condition]
  (let [generate-cars (partial generate-items generate-car)]
    (approximate-events-with-condition generate-cars 100000 event condition)))

;Tehtävä 1 b i).  Tulkinta: jos R ja B ovat tapahtuneet mutta K ei,
;                 niin A tapahtuu aina. Muutoin A:lla on noin 9/10 tn.
;                 Tämä johtuu siitä, että R ei tapahdu koskaan jos A ei tapahdu
(assignment-1-part-b a? r-b-not-k?)

;Tehtävä 1 B ii). Tulkinta: Käynnistyy hyvin usein kun on Sytytys tapahtuu ja
;                 Bensaa on.
(assignment-1-part-b k? r-s-b?)

;Tehtävä 1 B iii) Tulkinta: radion käynnistymisellä ei ole vaikutusta moottorin
;                 käynnistymiseen  (vrt. kohta iii))
(assignment-1-part-b k? not-r-s-b?)

(defn get-alarm-p
  "Calculates probability of alarm going off"
  [parent-probabilities]
  (if
    (= 1 (:M parent-probabilities))
    (if
      (= 1 (:V parent-probabilities))
      0.97
      0.81)
    (if
      (= 1 (:V parent-probabilities))
      0.92
      0.0095)))

(defn generate-house
  "Generates a new house"
  []
  (let [M (generate-node 0.009)
        V (generate-node 0.0032)
        H (generate-node (get-alarm-p {:M M :V V}))]
    {:M M :V V :H H}))

(defn h?
  "Returns true if a-map :H equals 1"
  [a-map]
  (= 1 (:H a-map)))

(defn m?
  "Returns true if a-map :H equals 1"
  [a-map]
  (= 1 (:M a-map)))

(defn m-and-h?
  "Returns true if a-map :M and :H equal 1"
  [a-map]
  (= 1 (:M a-map) (:H a-map)))

(defn v?
  "Returns true if a-map :V equals 1"
  [a-map]
  (= 1 (:V a-map)))

(defn assignment-1-part-c
  "A helper for assignment 1 part c"
  [event condition]
  (let [generate-houses (partial generate-items generate-house)]
    (approximate-events-with-condition generate-houses 100000 event condition)))

; Tehtävä 1 C: eka osa näyttää nyt vähän pieneltä luvulta, mut en keksiny miä
; siinä varsinaisesti olis väärässäkään
(assignment-1-part-c v? h?)
(assignment-1-part-c v? m-and-h?)
