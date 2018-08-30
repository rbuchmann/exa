(ns exa.core
  (:require [clojure.string :as str]
            [exa.assembly   :as a]))

(defn genlabel
  ([]  (-> (gensym)   str (str/replace #"_*" "")))
  ([s] (-> (gensym s) str (str/replace #"_*" ""))))

(defn block [& args]
  (->> (map a/eval-exacode args)
       (interpose \newline)
       (apply str)))

(defmethod a/eval-form 'do [[_ & args]]
  (apply block args))
