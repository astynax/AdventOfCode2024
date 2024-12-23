(ns advent-of-code-2024.day-23
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]
            [loom.graph :as g]
            [loom.alg :as a]))

(defn decode [text]
  (->> (for [line (s/split-lines (s/trim text))
             :let [[f t] (s/split line #"-" 2)]]
         [f t])
       (into [])))

(def input (decode (slurp (io/resource "day-23.input"))))

(def example
  [["kh" "tc"] ["qp" "kh"] ["de" "cg"] ["ka" "co"] ["yn" "aq"]
   ["qp" "ub"] ["cg" "tb"] ["vc" "aq"] ["tb" "ka"] ["wh" "tc"]
   ["yn" "cg"] ["kh" "ub"] ["ta" "co"] ["de" "co"] ["tc" "td"]
   ["tb" "wq"] ["wh" "td"] ["ta" "ka"] ["td" "qp"] ["aq" "cg"]
   ["wq" "ub"] ["ub" "vc"] ["de" "ta"] ["wq" "aq"] ["wq" "vc"]
   ["wh" "yn"] ["ka" "de"] ["kh" "ta"] ["co" "tc"] ["wh" "qp"]
   ["tb" "vc"] ["td" "yn"]])

(defn subgraphs [edges]
  (let [fg (apply g/graph edges)
        ns (g/nodes fg)]
    (->> (for [a ns
               :let [scs (g/successors fg a)]
               b scs
               c scs
               :when (and (not= b c)
                          (g/has-edge? fg b c))]
           (set [a b c]))
         (into #{}))))

(defn all-with-t [edges]
  (->> (subgraphs edges)
       (filter (fn [sg] (some #(s/starts-with? % "t") sg)))
       count))

(defn clique [g n]
  (loop [clique #{n}
         ns (g/successors g n)]
    (reduce
     (fn [clique x]
       (if (every? #(g/has-edge? g % x) clique)
         (conj clique x)
         clique))
     #{n}
     ns)))

(defn all-cliques [g]
  (loop [cs ()
         seen #{}
         ns (set (g/nodes g))]
    (if (empty? ns)
      cs
      (let [c (clique g (first ns))]
        (recur (cons c cs)
               (set/union seen c)
               (set/difference ns c))))))

(defn largest-clique-pass [edges]
  (->> edges
       (apply g/graph)
       all-cliques
       (sort-by count >)
       first
       sort
       (s/join ",")))

(def solution1 (all-with-t input))

(def solution2 (largest-clique-pass input))
