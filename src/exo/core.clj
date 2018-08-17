(ns exo.core
  (:require [clojure.string :as str])
  (:refer-clojure :rename {+ c+
                           - c-
                           * c*
                           / cdiv
                           if cif}))

(def x "X")
(def t "T")
(def f "F")
(def m "M")

(defn as-asm [x]
  (str/upper-case
   (if (or (keyword? x)
           (symbol?  x))
     (name x)
     x)))

(defn fn-call [f & args]
  (let [f-name (-> f name as-asm (str " "))
        args* (->> args (map as-asm) (interpose " "))]
    (apply str (list* f-name args*))))

(defn op [op-key]
  (partial fn-call op-key))

(defn block [args]
  (->> block
       (interpose " ")
       (apply str)))

;; Manipulate values

(def + (op :addi))
(def - (op :subi))
(def * (op :muli))
(def / (op :divi))
(def % (op :modi))

(defn swiz [a b]
  (fn-call :swiz a b))

(defn copy [from to]
  (fn-call :copy from to))

;; Branching

(defn mark [l]
  (fn-call :mark l))

(defn jump [l]
  (fn-call :jump l))

(defn fjump [l])

(defn if [pred then else]
  )



(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
