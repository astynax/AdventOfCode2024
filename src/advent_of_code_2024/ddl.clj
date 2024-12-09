(ns advent-of-code-2024.ddl)

(defn empty-ddl []
  {::id-seq 0
   :first nil
   :last nil
   ::store {}})

(defn append [{s ::id-seq
               l :last
               :as ddl}
              v]
  (let [m (-> ddl
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
                :as ddl}
               v]
  (let [m (-> ddl
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
  (reduce append (empty-ddl) xs))

(defn at [ddl k]
  (assert (contains? (::store ddl) k)
          (str "Key not found: " k))
  (get-in ddl [::store k]))

(defn insert-after [{s ::id-seq
                     d ::store
                     :as ddl} k v]
  (if-let [n (:next (at ddl k))]
    (-> ddl
        (update ::id-seq inc)
        (assoc-in [::store k :next] s)
        (assoc-in [::store n :prev] s)
        (assoc-in [::store s] {:prev k
                               :next n
                               :value v}))
    (append ddl v)))

(defn insert-before [{d ::store
                      :as ddl} k v]
  (if-let [p (:prev (at ddl k))]
    (insert-after ddl p v)
    (prepend ddl v)))

(defn edit [ddl k f & args]
  (->> k
       (at ddl)
       :value
       (#(apply f % args))
       (assoc-in ddl [::store k :value])))
