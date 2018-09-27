(ns exa.assembly
  (:require [clojure.string :as str]
            [exa.utils      :as u])
  (:refer-clojure :exclude [+ - * / < > = drop test])
  #?(:cljs
     (:require-macros [exa.assembly :refer [defexas]])))

(def ^:dynamic core-symbols
  '{eof "EOF"
    x   "X"})

(defn fn-call [f & args]
  (let [f-name (-> f name str/upper-case)]
    (->> (cons f-name args)
         (interpose " ")
         (apply str))))

(defmulti eval-form first)


(defn eval-exacode [c]
  (cond
    (seq? c) (eval-form c)
    (symbol? c) (or (core-symbols c)
                    (u/fail "Unknown symbol:" c))
    (number? c) (str c)
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
  (void [r]))

(defn infix-op [op-key a b]
  (str (eval-exacode a) " " (name op-key) " " (eval-exacode b)))

(defmethod eval-form '< [[_ a b]] (infix-op :< a b))
(defmethod eval-form '> [[_ a b]] (infix-op :> a b))
(defmethod eval-form '= [[_ a b]] (infix-op := a b))
