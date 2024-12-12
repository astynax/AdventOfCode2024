(ns advent-of-code-2024.day-12
  (:require [clojure.java.io :as io]
            [advent-of-code-2024.utils :refer :all]))

(defn decode [text]
  (topo-map-bound text))

(def input (decode (slurp (io/resource "day-12.input"))))

(def example1 (decode "
AAAA
BBCD
BBCC
EEEC
"))

(def example2 (decode "
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
"))

(def example3 (decode "
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
"))

(defn neibs [valid? [x y]]
  (->> [[(dec x) y]
        [(inc x) y]
        [x (dec y)]
        [x (inc y)]]
       (filter valid?)))

(defn mark-regions [{m :map
                     :keys [in-bounds?]
                     :as area}]
  (loop [{:keys [qs v i rs vs] :as state}
         {:qs (->> (keys m)
                   (map #(vector % nil))
                   (cons [[0 0] 0]))
          :v [(m [0 0]) 0]
          :i 0
          :rs {}
          :vs #{}}]
    (if (empty? qs)
      rs
      (let [[[pos id] & qs] qs]
        (if (vs pos)
          (recur (assoc state :qs qs))
          (let [nv (m pos)]
            (if (not= [nv id] v)
              (recur (assoc state
                            :qs (cons [pos (inc i)] qs)
                            :v [nv (inc i)]
                            :i (inc i)))
              (let [ns (->> pos
                            (neibs
                             (fn [n]
                               (and (in-bounds? n)
                                    (not (vs n))
                                    (= (m n) (first v)))))
                            (map #(vector % i)))]
                (recur (assoc state
                              :qs (into qs ns)
                              :rs (->> pos
                                       (conj (get rs i #{}))
                                       (assoc rs i))
                              :vs (conj vs pos)))))))))))

(defn perimeters [regions]
  (->> regions
       (map (fn [[id ps]]
              [id (->> ps
                       (flatmap (partial neibs (complement ps)))
                       count)]))
       (into {})))

(defn sections [pairs]
  (->> pairs
       (map (fn [[p x]] (and (not p) x)))
       (partition-by identity)
       (filter first)
       count))

(defn steps [[from to]]
  (if (< from to)
    (range from (inc to))
    (range from (dec to) -1)))

(defn scan-for-sides [region axis1 axis2 mk-pos mk-neib]
  (->> (for [step (steps axis1)]
         (sections (for [pos (steps axis2)]
                     [(contains? region (mk-neib pos step))
                      (contains? region (mk-pos pos step))])))
       (reduce +)))

(defn measure-by [f ps]
  (let [vs (map f ps)]
    [(reduce min vs)
     (reduce max vs)]))

(defn sides [region]
  (let [[nx mx] (measure-by first region)
        [ny my] (measure-by second region)]
    (->> [[[ny my] [nx mx] (fn [x y] [x y]) (fn [x y] [x (dec y)])]  ; v
          [[my ny] [nx mx] (fn [x y] [x y]) (fn [x y] [x (inc y)])]  ; ^
          [[nx mx] [ny my] (fn [y x] [x y]) (fn [y x] [(dec x) y])]  ; >
          [[mx nx] [ny my] (fn [y x] [x y]) (fn [y x] [(inc x) y])]] ; <
         (map (partial apply scan-for-sides region))
         (reduce +))))

(defn prices [area]
  (let [rs (mark-regions area)
        ps (perimeters rs)]
    [(->> (for [[k r] rs]
            (* (count r) (ps k)))
          (reduce +))
     (->> (for [[_ r] rs]
            (* (count r) (sides r)))
          (reduce +))]))

(def input-prices (prices input))

(def solution1 (first input-prices))

(def solution2 (second input-prices))
