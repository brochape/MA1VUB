(ns flight-reservation
  (:require [clojure.string]
            [clojure.pprint]
            ;[input-simple :as input]))
            [input-random :as input]))

(def flights
  "All flights are encapsulated in a single atom in this implementation.
  You are free to change this to a more appropriate mechanism."
  (atom []))

(defn initialize-flights [initial-flights]
  "Set `flights` atom to the `initial-flights`."
  (reset! flights initial-flights))

(defn print-flights [flights]
  "Print `flights`."
  (letfn [(pricing->str [pricing]
            (->> pricing
              (map (fn [[p a t]] (clojure.pprint/cl-format nil "$~3d: ~3d ~3d" p a t)))
              (clojure.string/join ", ")))]
    (doseq [{:keys [id from to pricing]} flights]
      (println (clojure.pprint/cl-format nil "Flight ~3d from ~a to ~a: ~a"
        id from to (pricing->str pricing))))))

(defn- update-pricing [flight factor]
  "Updated pricing of `flight` with `factor`."
  (update flight :pricing
    (fn [pricing]
      (map (fn [[p a t]] [(* p factor) a t]) pricing))))

(defn start-sale []
  "Sale: all flights -20%."
  (println "Start sale!")
  (swap! flights (fn [fs] (vec (map (fn [f] (update-pricing f 0.80)) fs)))))

(defn end-sale []
  "End sale: all flights +25% (inverse of -20%)."
  (println "End sale!")
  (swap! flights (fn [fs] (vec (map (fn [f] (update-pricing f 1.25)) fs)))))

(defn sort-pricing [pricing]
  "Sort `pricing` from lowest to highest price."
  (sort-by first pricing))

(defn filter-pricing-with-n-seats [pricing seats]
  "Get `pricing` for which there are at least `seats` empty seats available."
  (filter #(>= (second %) seats) pricing))

(defn lowest-available-price [flight seats]
  "Returns the lowest price in `flight` for which at least `seats` empty seats
  are available, or nil if none found."
  (-> (:pricing flight)                 ; [[price available taken]]
    (filter-pricing-with-n-seats seats)
    (sort-pricing)
    (first)                             ; [price available taken]
    (first)))                           ; price

(defn- find-flight [flights customer]
  "Find a flight in `flights` that is on the route and within the budget of
  `customer`. If a flight was found it is returned, else returns nil."
  (let [{:keys [id from to seats budget]}
          customer
        flights-on-route
          (filter #(and (= (:from %) from) (= (:to %) to)) flights)
        flights-in-budget
          (filter
            (fn [f]
              (let [lowest-price (lowest-available-price f seats)]
                (and (some? lowest-price) (<= lowest-price budget))))
          flights-on-route)]
    (first flights-in-budget)))

(defn- book [flight customer]
  "Updates `flight` to book `customer`'s seats, returning
  {:flight updated-flight :price price}."
  (let [seats        (:seats customer)
        lowest-price (lowest-available-price flight seats)
        new-pricing  (for [[p a t] (:pricing flight)]
                       (if (= p lowest-price)
                         [p (- a seats) (+ t seats)]
                         [p a t]))]
    {:flight (assoc flight :pricing new-pricing)
     :price  lowest-price}))

(defn- process-customer [flights customer]
  "Try to book a flight from `flights` for `customer`, returning the updated
  flight if found, or nil if no suitable flight was found."
  (if-let [flight (find-flight flights customer)]
    (let [{updated-flight :flight price :price} (book flight customer)]
      (println "Customer" (:id customer) "booked" (:seats customer)
        "seats on flight" (:id updated-flight) "at $" price " (< budget of $"
        (:budget customer) ").")
      updated-flight)
    (do
      (println "Customer" (:id customer) "did not find a flight.")
      nil)))

(def finished-processing?
  "Set to true once all customers have been processed, so that sales process
  can end."
  (atom false))

(defn process-customers [customers]
  "Process `customers` one by one."
  (doseq [customer customers]
    (swap! flights
      (fn [flights]
        (let [updated-flight (process-customer flights customer)]
          (if (some? updated-flight)
            (assoc flights (:id updated-flight) updated-flight)
            flights)))))
  (reset! finished-processing? true))

(defn sales-process []
  "The sales process starts and ends sales periods, until `finished-processing?`
  is true."
  (loop []
    (Thread/sleep input/TIME_BETWEEN_SALES)
    (start-sale)
    (Thread/sleep input/TIME_OF_SALES)
    (end-sale)
    (if (not @finished-processing?)
      (recur))))

(defn main []
  (initialize-flights input/flights)
  (let [f1 (future (process-customers input/customers))
        f2 (future (sales-process))]
    @f1
    @f2)
  (println "Flights:")
  (print-flights @flights))

(main)
(shutdown-agents)
