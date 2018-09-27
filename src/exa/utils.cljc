(ns exa.utils)

(defn subl [form x]
  (let [[fst & rst] form]
    (list* fst x rst)))

(defn subr [form x]
  (concat form [x]))

(defn update-nth [col n f & args]
  (map-indexed (fn [i x] (if (= i n)
                          (apply f x args)
                          x))
               col))

(defn fail [& args]
  (let [msg (->> args (interpose " ") (apply str))]
    (throw
     #?(:clj  (Exception. msg)
        :cljs (js/Error.  msg)))))
