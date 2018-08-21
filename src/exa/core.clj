(ns exa.core
  (:require [clojure.string :as str])
  (:refer-clojure :rename {+ c+
                           - c-
                           * c*
                           / cdiv
                           < c<
                           > c>
                           = c=
                           if cif}))

(def x "X")
(def t "T")
(def f "F")
(def m "M")

(def eof "EOF")

(defn as-asm [x]
  (str/upper-case
   (if (or (keyword? x)
           (symbol?  x))
     (name x)
     x)))

(defn fn-call [f & args]
  (let [f-name (-> f name as-asm (str (when args " ")))
        args* (->> args (map as-asm) (interpose " "))]
    (apply str (list* f-name args*))))

(defn block [& args]
  (->> args
       (interpose "\n")
       (apply str)))

(defn genlabel
  ([]
   (-> (gensym)   str (str/replace #"_*" "")))
  ([s]
   (-> (gensym s) str (str/replace #"_*" ""))))

;; Manipulate values

(defn op [op-key]
  (partial fn-call op-key))

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

(defn infix-op [op-key]
  (fn [a b]
    (str a " " (symbol op-key) " " b)))

(def < (infix-op :<))
(def > (infix-op :>))
(def = (infix-op :=))

(defn test [p]
  (fn-call :test p))

(defn mark [l]
  (fn-call :mark l))

(defn jump [l]
  (fn-call :jump l))

(defn fjump [l]
  (fn-call :fjmp l))

(defn tjmp [l]
  (fn-call :tjmp l))

;; Navigation

(defn link [r]
  (fn-call :link r))

(defn host []
  (fn-call :host))

;; Files

(defn grab [f]
  (fn-call :grab f))

(defn drop []
  (fn-call :drop))

(defn make []
  (fn-call :make))

(defn seek [n]
  (fn-call :seek n))

;; Higher level

(defn if [pred then else]
  (let [false-label (genlabel "false")
        end-label (genlabel "end")]
    (block
     (test pred)
     (fjmp false-label)
     then
     (jump end-label)
     (mark false-label)
     else
     (mark end-label))))

(defn when [pred then]
  (let [end-label (genlabel "end")]
    (block
     (test pred)
     (fjmp end-label)
     then
     (mark end-label))))

(defn while [pred body]
  (let [loop-label (genlabel "while")]
    (block
     (mark loop-label)
     body
     (test pred)
     (tjmp loop-label))))

(defn dotimes [n body]
  (block
   (copy n x)
   (while (> x 0)
     body)))
