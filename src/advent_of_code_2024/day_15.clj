(ns advent-of-code-2024.day-15
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [advent-of-code-2024.utils :refer :all]))

(defn decode [text]
  (let [[p1 p2 & _] (s/split text #"\n\n" 2)
        {m :map :keys [w h]} (topo-map-bound p1 (fn [_ c] (when (not= \. c) c)))
        [pos] (keys-for \@ m)
        m (dissoc m pos)
        steps (filter #(not (java.lang.Character/isSpace %)) p2)]
    {:m m
     :w w
     :h h
     :pos pos
     :steps steps}))

(def input (decode (slurp (io/resource "day-15.input"))))

(def example (decode "
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
"))

(def example2 (decode "
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
"))

(defn move [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn try-to-push [m pos d]
  (case (m pos)
    nil m
    \# nil
    (let [np (move pos d)]
      (when-let [mm (try-to-push m np d)]
        (-> mm
            (assoc np (mm pos))
            (dissoc pos))))))

(def dir->off {\v [0 1]
               \^ [0 -1]
               \< [-1 0]
               \> [1 0]})

(defn step [{:keys [m pos] :as state} d]
  (let [off (dir->off d)
        np (move pos off)]
    (if (contains? m np)
      (when-let [m (try-to-push m np off)]
        (assoc state
               :m m
               :pos np))
      (assoc state :pos np))))

(defn simulate-using [step-fn {:keys [steps] :as state}]
  (loop [state state
         steps steps]
    (if (empty? steps)
      state
      (let [[s & nss] steps]
        (recur (or (step-fn state s) state) nss)))))

(defn widen [state]
  (let [m (->>
           (for [[[x y] c] (:m state)
                 :let [nx (* x 2)]
                 p (map vector
                        [[nx y] [(inc nx) y]]
                        (case c
                          \# "##"
                          \O "[]"))]
             p)
           (into {}))]
    (-> state
        (assoc :m m)
        (update-in [:pos 0] * 2)
        (update :w * 2))))

(defn try-to-push-wide [m x y dy]
  (let [ny (+ y dy)
        nl [x ny]
        nr [(inc x) ny]
        move-self #(-> %
                       (assoc nl \[ nr \])
                       (dissoc [x y]
                               [(inc x) y]))
        vl (m nl)
        vr (m nr)]
    (when (and (not= vl \#) (not= vr \#))
      (if (and (= vl \[) (= vr \]))
        (when-let [m (try-to-push-wide m x ny dy)]
          (move-self m))
        (when-let [m (if (= vl \])
                       (try-to-push-wide m (dec x) ny dy)
                       m)]
          (when-let [m (if (= vr \[)
                         (try-to-push-wide m (inc x) ny dy)
                         m)]
            (move-self m)))))))

(defn wide-step [{m :m
                  [x y] :pos
                  :as state} d]
  (if (#{\< \>} d)
    (step state d)
    (let [dy ({\^ -1
               \v 1} d)
          np [x (+ y dy)]
          vp (m np)]
      (case vp
        \# nil
        nil (assoc state :pos np)
        (when-let [m (try-to-push-wide
                      m
                      (if (= vp \[) x (dec x))
                      (+ y dy)
                      dy)]
          (assoc state
                 :m m
                 :pos np))))))

(defn all-gps-for [c m]
  (->> (for [[x y] (keys-for c m)]
         (+ x (* y 100)))
       (reduce +)))

(def solution1
  (->> input
       (simulate-using step)
       :m
       (all-gps-for \O)))

(def solution2
  (->> input
       widen
       (simulate-using wide-step)
       :m
       (all-gps-for \[)))
