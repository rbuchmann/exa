(ns exa.csp
  (:require [exa.assembly :as a]
            [exa.utils :as u]
            [clojure.string :as str])
  #?(:cljs
     (:require-macros [exa.csp :refer [bind-exa-forms]])))

(defn genlabel
  ([]    (-> (gensym)     str (str/replace #"_*" "")))
  ([sym] (-> (gensym sym) str (str/replace #"_*" ""))))

#?(:clj
   (defmacro bind-exa-forms [bindings]
     `(do
        ~@(for [[f-sym f] bindings]
            `(defmethod a/eval-form '~f-sym [[_# & args#]]
               (apply ~f (map a/eval-exacode args#)))))))

(defn do* [& args]
  (->> args
       (map a/eval-exacode)
       (interpose \newline)
       (apply str)))

(defn if* [pred then else]
  (let [false-label (genlabel "false")
        end-label   (genlabel "end")]
    (do*
     (a/test pred)
     (a/fjmp false-label)
     then
     (a/jump end-label)
     (a/mark false-label)
     else
     (a/mark end-label))))

(defn when* [pred then]
  (let [end-label (genlabel "end")]
    (do*
     (a/test pred)
     (a/fjmp end-label)
     then
     (a/mark end-label))))

(defn while* [pred body]
  (let [looplabel (genlabel "while")]
    (do*
     (a/mark looplabel)
     body
     (a/test pred)
     (a/tjmp looplabel))))

(defn dotimes* [n body]
  (let [looplabel (genlabel "dotimes")]
    (do*
     (a/copy n :x)
     (a/mark looplabel)
     body
     (a/test (a/> x 0))
     (a/tjmp looplabel))))


(def channels
  '{f  "F"
    m "M"
    })

(bind-exa-forms
 {do      do*
  if      if*
  when    when*
  while   while*
  dotimes *dotimes})
