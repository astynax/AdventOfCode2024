(ns advent-of-code-2024.day-09
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :as m]
            [advent-of-code-2024.dll :as dll]))

(defn decode [text]
  (for [[ch free id] (map vector (s/trim text)
                          (iterate not false)
                          (map #(m/floor-div % 2)
                               (iterate inc 0)))]
    [(parse-long (s/join [ch]))
     (if free nil id)]))

(def input (decode (slurp (io/resource "day-09.input"))))

(def example (decode "2333133121414131402"))

(defn dump [data]
  (->> data
       (map #(or % "."))
       s/join))

(defn next-such [pred step pos data]
  (let [np (step pos)]
    (assert (<= 0 np (dec (count data))))
    (if (pred (get data np))
      np
      (recur pred step np data))))

(defn as-blocks [data]
  (->> data
       (map (fn [[n i]] (repeat n i)))
       (apply concat)))

(defn optimize [initial-data]
  (loop [data (vec (as-blocks initial-data))
         i 0
         j (count data)]
    (let [ni (next-such nil? inc i data)
          nj (next-such some? dec j data)]
      (if (>= ni nj) data
          (recur
           (-> data
               (assoc ni (get data nj))
               (assoc nj nil))
           ni nj)))))

(defn next-file [dll k]
  (loop [k (:prev (dll/at dll k))]
    (when (some? k)
      (let [p (dll/at dll k)]
        (if (some? (get-in p [:value 1]))
          k
          (recur (:prev p)))))))

(defn optimize-no-frag [initial-data]
  (loop [dll (dll/seq-> initial-data)
         file (:last dll)]
    (if (nil? file)
      (-> dll
          dll/->seq
          as-blocks)
      (let [{[s v] :value} (dll/at dll file)]
        (if-let [[k ps] (loop [k (:first dll)]
                          (when (not= k file)
                            (let [{[ps pv] :value
                                   n :next} (dll/at dll k)]
                              (cond
                                (and (nil? pv) (<= s ps)) [k ps]
                                (nil? n) nil
                                :else (recur n)))))]
          (let [new (-> dll
                        (dll/edit k (constantly [s v]))
                        (dll/edit file (constantly [s nil])))
                new (if (< s ps)
                      (dll/insert-after new k [(- ps s) nil])
                      new)
                new-file (next-file new file)]
            (recur new new-file))
          (recur dll (next-file dll file)))))))

(defn checksum [data]
  (->> data
       (map #(or % 0))
       (map * (iterate inc 0))
       (reduce +)))

(def solution1 #(checksum (optimize input)))

(def solution2 #(checksum (optimize-no-frag input)))
