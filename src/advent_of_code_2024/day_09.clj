(ns advent-of-code-2024.day-09
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :as m]
            [advent-of-code-2024.ddl :as ddl]))

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

(defn next-file [ddl k]
  (loop [k (:prev (ddl/at ddl k))]
    (when (some? k)
      (let [p (ddl/at ddl k)]
        (if (some? (get-in p [:value 1]))
          k
          (recur (:prev p)))))))

(defn optimize-no-frag [initial-data]
  (loop [ddl (ddl/seq-> initial-data)
         file (:last ddl)]
    (if (nil? file)
      (-> ddl
          ddl/->seq
          as-blocks)
      (let [{[s v] :value} (ddl/at ddl file)]
        (if-let [[k ps] (loop [k (:first ddl)]
                          (when (not= k file)
                            (let [{[ps pv] :value
                                   n :next} (ddl/at ddl k)]
                              (cond
                                (and (nil? pv) (<= s ps)) [k ps]
                                (nil? n) nil
                                :else (recur n)))))]
          (let [new (-> ddl
                        (ddl/edit k (constantly [s v]))
                        (ddl/edit file (constantly [s nil])))
                new (if (< s ps)
                      (ddl/insert-after new k [(- ps s) nil])
                      new)
                new-file (next-file new file)]
            (recur new new-file))
          (recur ddl (next-file ddl file)))))))

(defn checksum [data]
  (->> data
       (map #(or % 0))
       (map * (iterate inc 0))
       (reduce +)))

(def solution1 #(checksum (optimize input)))

(def solution2 #(checksum (optimize-no-frag input)))
