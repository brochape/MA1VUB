(ns input-simple)

(def flights
  [{:id 0
    :from "BRU" :to "ATL"
    :pricing [[600 150 0] ; price; # seats available at that price; # seats taken at that price
              [650  50 0]
              [700  50 0]
              [800  50 0]]}
   {:id 1
    :from "BRU" :to "LON"
    :pricing [[300 150 0]
              [350  50 0]
              [370  20 0]
              [380  30 0]]}
   {:id 2
    :from "BRU" :to "LON"
    :pricing [[250 100 0]
              [300  50 0]]}
   {:id 3
    :from "BRU" :to "MAD"
    :pricing [[200 150 0]
              [250  50 0]
              [300 100 0]]}
   {:id 4
    :from "BRU" :to "MAD"
    :pricing [[250 150 0]
              [300  50 0]]}])

(def customers
  [{:id  0 :from "BRU" :to "ATL" :seats 5 :budget 700}
   {:id  1 :from "BRU" :to "ATL" :seats 5 :budget 550}
   {:id  2 :from "BRU" :to "LON" :seats 6 :budget 270}
   {:id  3 :from "BRU" :to "ATL" :seats 4 :budget 600}
   {:id  4 :from "BRU" :to "LON" :seats 3 :budget 270}
   {:id  5 :from "BRU" :to "LON" :seats 9 :budget 250}
   {:id  6 :from "BRU" :to "MAD" :seats 5 :budget 200}
   {:id  7 :from "BRU" :to "MAD" :seats 9 :budget 150}
   {:id  8 :from "BRU" :to "LON" :seats 5 :budget 250}
   {:id  9 :from "BRU" :to "ATL" :seats 4 :budget 500}
   {:id 10 :from "BRU" :to "MAD" :seats 1 :budget 180}
   {:id 11 :from "BRU" :to "LON" :seats 2 :budget 320}
   {:id 12 :from "BRU" :to "ATL" :seats 3 :budget 850}
   {:id 13 :from "BRU" :to "ATL" :seats 4 :budget 200}])

(def TIME_BETWEEN_SALES 50) ; milliseconds
(def TIME_OF_SALES 10)
