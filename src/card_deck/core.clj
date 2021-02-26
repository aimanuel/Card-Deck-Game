;===========================================================
;NS & requirement declarations
;===========================================================

;Define the ns
(ns card-deck.core
  (:gen-class))

;API to export as CSV
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])


;===========================================================
;Card Deck & Helper-deck
;===========================================================

;Define the database of your card Deck
(def dummy [{:sn "Acecl" :ln "Ace Club"}
            {:sn "1cl" :ln "1 Club"}
            {:sn "2cl" :ln "2 Club"}
            {:sn "3cl" :ln "3 Club"}
            {:sn "4cl" :ln "4 Club"}
            {:sn "5cl" :ln "5 Club"}
            {:sn "6cl" :ln "6 Club"}
            {:sn "7cl" :ln "7 Club"}
            {:sn "8cl" :ln "8 Club"}
            {:sn "9cl" :ln "9 Club"}
            {:sn "10cl" :ln "10 Club"}
            {:sn "Jcl" :ln "Jack Club"}
            {:sn "Qcl" :ln "Queen Club"}
            {:sn "Kcl" :ln "King Club"}
            {:sn "Acehr" :ln "Ace Heart"}
            {:sn "1hr" :ln "1 Heart"}
            {:sn "2hr" :ln "2 Heart"}
            {:sn "3hr" :ln "3 Heart"}
            {:sn "4hr" :ln "4 Heart"}
            {:sn "5hr" :ln "5 Heart"}
            {:sn "6hr" :ln "6 Heart"}
            {:sn "7hr" :ln "7 Heart"}
            {:sn "8hr" :ln "8 Heart"}
            {:sn "9hr" :ln "9 Heart"}
            {:sn "10hr" :ln "10 Heart"}
            {:sn "Jhr" :ln "Jack Heart"}
            {:sn "Qhr" :ln "Queen Heart"}
            {:sn "Khr" :ln "King Heart"}
            {:sn "Acedm" :ln "Ace Diamond"}
            {:sn "1dm" :ln "1 Diamond"}
            {:sn "2dm" :ln "2 Diamond"}
            {:sn "3dm" :ln "3 Diamond"}
            {:sn "4dm" :ln "4 Diamond"}
            {:sn "5dm" :ln "5 Diamond"}
            {:sn "6dm" :ln "6 Diamond"}
            {:sn "7dm" :ln "7 Diamond"}
            {:sn "8dm" :ln "8 Diamond"}
            {:sn "9dm" :ln "9 Diamond"}
            {:sn "10dm" :ln "10 Diamond"}
            {:sn "Jdm" :ln "Jack Diamond"}
            {:sn "Qdm" :ln "Queen Diamond"}
            {:sn "Kdm" :ln "King Diamond"}
            {:sn "Acesp" :ln "Ace Spade"}
            {:sn "1sp" :ln "1 Spade"}
            {:sn "2sp" :ln "2 Spade"}
            {:sn "3sp" :ln "3 Spade"}
            {:sn "4sp" :ln "4 Spade"}
            {:sn "5sp" :ln "5 Spade"}
            {:sn "6sp" :ln "6 Spade"}
            {:sn "7sp" :ln "7 Spade"}
            {:sn "8sp" :ln "8 Spade"}
            {:sn "9sp" :ln "9 Spade"}
            {:sn "10sp" :ln "10 Spade"}
            {:sn "Jsp" :ln "Jack Spade"}
            {:sn "Qsp" :ln "Queen Spade"}
            {:sn "Ksp" :ln "King Spade"}])


;Function for getting a vector of range
(defn get-range
  "this is to create a vector range i.e [0 1 2 3 4 5]"
  [n]
  (into [] (range n)))

;Define helper database to help shuflling the deck
(def helper-deck (get-range 56))


(println (range 56))

;=========================================================
;in memory db using atom & swap functions
;=========================================================
(def in-memory-db (atom {:temp 0}))



;=========================================================
;CSV Export Import related functions
;=========================================================

;This func is to write csv into folder resources
;it'll overwrite if the list changed
(defn write-csv [path row-data]
  (let [columns [:pick :sn :seq]
        headers (map name columns)
        rows (mapv #(mapv % columns) row-data)]
    (with-open [file (io/writer path)]
      (csv/write-csv file (cons headers rows)))))


(write-csv "resources/result.csv" dummy)



;this variable is to open & read csv file
(defn csvopener
  []
  (with-open [reader (io/reader "resources/shuffled.csv")]
    (doall
     (csv/read-csv reader))))


;this function is to convert plain text csv file into a maps
;ACTUALLY IN THIS CASE WE DON'T NEED THIS YET
(defn csv-data->maps
  [csv-data]
  (map zipmap
       (->> (first csv-data)
            (map keyword)
            repeat)
        (rest csv-data)))


;this function is combined open & mapping it
;this is only testing, we don't need this
(into [](csv-data->maps (csvopener)))




;=========================================================
;Print & Export Shuffled deck Functions
;=========================================================
(defn shuffled-deck
  []
  (def z (shuffle helper-deck)))


;structure of exported data will be like this
[{:pick 3 :seq ["shuffled-deck"] :sn ["short named"]}]


;Functions to prepare the exported shuffled deck
;the result of this function will be like above vector
(defn exported-text-csv
  "this is to prepare the exported csv text"
  [y z]
  (conj [](hash-map
           :pick y
           :seq (into [] z)
           :sn (into []
                     (reduce
                      (fn [convert numz]
                        (into convert [(get (get dummy numz) :sn)]))
                      []
                      z))
           )))


(println (str "picked card = " 10))

(defn print-shuffled-deck
  "this is to print the shuffled deck"
  [y z]
  (println "====================")
  (println (str "PICKED CARD = " y))
  (println "====================")
  (dorun (for [x (take y z)]
        (println
         (get (get dummy x) :ln))))
  (println "====================")
  (println (str "REMAINING CARD = " (- 56 y)))
  (println "====================")
  (dorun (for [x (drop y z)]
        (println
         (get (get dummy x) :ln)))))



(defn print-export-shuffled-deck
  "this is to print & export the shuffled deck"
  [y]
  (shuffled-deck)
  (print-shuffled-deck y z)
  (println (exported-text-csv y z))
  (swap! in-memory-db assoc :temp (exported-text-csv y z))
  (write-csv "resources/shuffled.csv" (exported-text-csv y z)))


;this 2 code below is for testing purpose
(print-export-shuffled-deck 15)

(deref in-memory-db)





;========================================================
;Summon the last picked card from csv
;========================================================

;define last pick card
(defn last-pick
  []
  (Integer/parseInt
   (get
    (get
     (into [](csv-data->maps (csvopener)))
     0)
    :pick)))

(last-pick)

;define last random seq of cards
(defn last-seq
  []
  (clojure.edn/read-string
   (get
    (get
     (into [](csv-data->maps (csvopener)))
     0)
    :seq)))

(last-seq)

;print out the last seq of cards
(print-shuffled-deck (last-pick) (last-seq))




;========================================================
;Summon the last picked card from in-memory db
;========================================================
(defn in-mem-last-pick
  []
  (get (get (get (deref in-memory-db) :temp) 0) :pick))

(defn in-mem-last-seq
  []
  (get (get (get (deref in-memory-db) :temp) 0) :seq))

;print out the last seq of cards
(print-shuffled-deck (in-mem-last-pick) (in-mem-last-seq))




;******************************************************
;PLAY GAME
;******************************************************

;print & export the shuffled deck, number is your picked card
;export both : in-memory db & csv as resource/shuffled.csv
(print-export-shuffled-deck 14)

;call out the saved last picked from csv
(print-shuffled-deck (in-mem-last-pick) (in-mem-last-seq))

;call out the saved last picked from in-memory db
(print-shuffled-deck (last-pick) (last-seq))
