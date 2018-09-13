(ns exa.utils)

(defn fail [& args]
  (let [msg (->> args (interpose " ") (apply str))]
    (throw #?(:cljs (js/Error. msg)
              :clj  (Exception. msg)))))

(defn insert-left [form x]
  (let [[fst & rst] form]
    (list* fst x rst)))

(defn insert-right [form x]
  (concat form [x]))

(defn update-nth [form n f & args]
  (map-indexed (fn [i x] (if (= n i)
                           (apply f x args)
                           x))
               form))
