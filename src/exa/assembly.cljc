(ns exa.assembly
  (:require [clojure.string :as str])
  (:refer-clojure :exclude [+ - * / < > = drop test])
  #?(:cljs
     (:require-macros [exa.assembly :refer [defexas]])) )

(defn fn-call [f & args]
  (let [f-name (-> f name str/upper-case)]
    (->> (cons f-name args)
         (interpose " ")
         (apply str))))

(defmulti eval-form first)

(defn eval-exacode [c]
  (cond
    (seq? c) (eval-form c)
    (keyword? c) (-> c name str/upper-case)
    :default c))

#?(:clj
   (defmacro defexas [& fns]
     `(do
        ~@(for [[f-sym arg-syms] fns]
            `(defn ~f-sym ~arg-syms
               (apply fn-call '~f-sym (map eval-exacode ~arg-syms))))
        ~@(for [[f-sym] fns]
            `(defmethod eval-form '~f-sym [[_# & args#]]
               (apply ~f-sym args#))))))

(defexas
  ;; Manipulate values
  (addi [a b to])
  (subi [a b to])
  (muli [a b to])
  (divi [a b to])
  (modi [a b to])
  (swiz [n pattern to])
  (copy [from to])

  ;; Branching
  (test [p])
  (mark [l])
  (jump [l])
  (fjmp [l])
  (tjmp [l])

  ;; Navigation
  (link [r])
  (host [])

  ;; Files
  (drop [])
  (make [])
  (seek [n])
  (grab [f])
  (file [r])
  (wipe [])

  ;; Lifecycle
  (halt [])
  (kill [])
  (repl [l])

  ;;communication
  (mode [])
  (void [r])
  (test [r]))

(defn infix-op [op-key a b]
  (str (eval-exacode a) " " (name op-key) " " (eval-exacode b)))

(defmethod eval-form '< [[_ a b]] (infix-op :< a b))
(defmethod eval-form '> [[_ a b]] (infix-op :> a b))
(defmethod eval-form '= [[_ a b]] (infix-op := a b))
