(ns advent-of-code-2024.dll)

(defn empty-dll []
  {::id-seq 0
   :first nil
   :last nil
   ::store {}})

(defn append [{s ::id-seq
               l :last
               :as dll}
              v]
  (let [m (-> dll
              (update ::id-seq inc)
              (assoc-in [::store s] {:prev l
                                     :next nil
                                     :value v})
              (assoc :last s))]
    (if (some? l)
      (assoc-in m [::store l :next] s)
      (assoc m :first s))))

(defn prepend [{s ::id-seq
                f :first
                :as dll}
               v]
  (let [m (-> dll
              (update ::id-seq inc)
              (assoc-in [::store s] {:prev nil
                                     :next f
                                     :value v})
              (assoc :first s))]
    (if (some? f)
      (assoc-in m [::store f :prev] s)
      (assoc m :last s))))

(defn ->seq [{f :first
              s ::store}]
  (letfn [(step [k]
            (when k
              (when-let [v (s k)]
                (cons (:value v)
                      (lazy-seq (step (:next v)))))))]
    (lazy-seq (step f))))

(defn seq-> [xs]
  (reduce append (empty-dll) xs))

(defn at [dll k]
  (assert (contains? (::store dll) k)
          (str "Key not found: " k))
  (get-in dll [::store k]))

(defn insert-after [{s ::id-seq
                     d ::store
                     :as dll} k v]
  (if-let [n (:next (at dll k))]
    (-> dll
        (update ::id-seq inc)
        (assoc-in [::store k :next] s)
        (assoc-in [::store n :prev] s)
        (assoc-in [::store s] {:prev k
                               :next n
                               :value v}))
    (append dll v)))

(defn insert-before [{d ::store
                      :as dll} k v]
  (if-let [p (:prev (at dll k))]
    (insert-after dll p v)
    (prepend dll v)))

(defn edit [dll k f & args]
  (->> k
       (at dll)
       :value
       (#(apply f % args))
       (assoc-in dll [::store k :value])))
