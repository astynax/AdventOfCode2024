(ns advent-of-code-2024.day-09
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.math :as m]
            [clojure.zip :as z]))

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

(defn find-place [z s]
  (loop [z z]
    (let [[n i] (z/node z)]
      (if (and (nil? i)
               (>= n s))
        z
        (when-let [nz (z/right z)]
          (recur nz))))))

(defn insert [l [n i]]
  (let [z (z/down (z/vector-zip l))]
    (when-let [z (find-place z n)]
      (let [[s _] (z/node z)
            z (z/replace z [n i])]
        (-> (if (> s n)
              (z/insert-right z [(- s n) nil])
              z)
            z/root)))))

(defn next-file [z]
  (loop [z (z/left z)]
    (when (some? z)
      (if (-> z z/node (get 1) nil?)
        (recur (z/left z))
        z))))

(defn optimize-no-frag [initial-data]
  (let [start (-> initial-data vec
                  z/vector-zip
                  z/down
                  z/rightmost)
        end (loop [z start]
              (let [ls (get-in z [1 :l])]
                (if (empty? ls)
                  z
                  (let [file (z/node z)
                        z (if-let [nl (insert ls file)]
                            (-> z
                                (assoc-in [1 :l] nl)
                                (z/replace (assoc file 1 nil)))
                            z)]
                    (if-let [nz (next-file z)]
                      (recur nz)
                      z)))))]
    (as-blocks (z/root end))))

(defn checksum [data]
  (->> data
       (map #(or % 0))
       (map * (iterate inc 0))
       (reduce +)))

(def solution1 #(checksum (optimize input)))

(def solution2 #(checksum (optimize-no-frag input)))
