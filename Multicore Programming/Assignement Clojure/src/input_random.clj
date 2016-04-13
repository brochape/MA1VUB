(ns input-random)

(def flights
  [{:id 0
    :from "BRU" :to "LON"
    :pricing [[100 150 0] ; price; # seats available at that price; # seats taken at that price
              [150  50 0]
              [200  50 0]
              [300  50 0]]}
   {:id 1
    :from "BRU" :to "LON"
    :pricing [[100  50 0]
              [200 150 0]
              [320  20 0]
              [240  30 0]]}
   {:id 2
    :from "BRU" :to "LON"
    :pricing [[250 100 0]
              [300  50 0]]}
   {:id 3
    :from "BRU" :to "LON"
    :pricing [[250 100 0]
              [300  50 0]]}
   {:id 4
    :from "BRU" :to "LON"
    :pricing [[250 100 0]
              [350  50 0]]}
   {:id 5
    :from "BRU" :to "LON"
    :pricing [[150 100 0]
              [300 100 0]]}
   {:id 6
    :from "BRU" :to "MAD"
    :pricing [[200 150 0]
              [250  50 0]
              [300 100 0]]}
   {:id 7
    :from "BRU" :to "MAD"
    :pricing [[200 150 0]
              [250  50 0]
              [300  80 0]
              [350  20 0]]}
   {:id 8
    :from "BRU" :to "MAD"
    :pricing [[200 150 0]
              [250  50 0]
              [300  80 0]
              [350  20 0]]}
   {:id 9
    :from "BRU" :to "MAD"
    :pricing [[250 150 0]
              [300  50 0]]}
   {:id 10
    :from "BRU" :to "MAD"
    :pricing [[250 150 0]
              [300  50 0]]}
   {:id 11
    :from "BRU" :to "MAD"
    :pricing [[150 150 0]
              [300  50 0]]}])

(def customers
  (for [id (range 1000)
        :let [{from :from to :to} (rand-nth flights)]]
    {:id     id
     :from   from
     :to     to
     :seats  (+ (rand-int 4) 1)        ; 1-4
     :budget (+ (rand-int 600) 200)})) ; 200-799

(def TIME_BETWEEN_SALES 50) ; milliseconds
(def TIME_OF_SALES 10)
