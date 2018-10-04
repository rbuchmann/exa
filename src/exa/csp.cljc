(ns exa.csp
  (:require [exa.assembly   :as a]
            [exa.utils      :as u]
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

(defn fork [& args]
  (let [labels (repeatedly (count args) (partial genlabel "fork"))]
    (apply do*
     (apply do* (map a/repl labels))
     (a/halt)
     (for [[label body] (map list labels args)]
       (do*
        (a/mark label)
        body
        (a/halt))))))

(defmethod a/eval-form '+ [[_ a b]] (a/addi a b 'x))
(defmethod a/eval-form '- [[_ a b]] (a/subi a b 'x))
(defmethod a/eval-form '/ [[_ a b]] (a/divi a b 'x))
(defmethod a/eval-form '* [[_ a b]] (a/muli a b 'x))
(defmethod a/eval-form '% [[_ a b]] (a/modi a b 'x))

(def valid-channels '[m f])

(def valid-channel? (set valid-channels))

(defn sub-form [[f & args :as form] x sub]
  (if ('#{do if when while} f)
    (u/update-nth form 1 sub x)
    (sub form x)))

(defn var->exa [s]
  (-> s name str/upper-case))

(defn receive-message [from rhs left?]
  (if (valid-channel? from)
    (let [sub     (if left?
                    u/subl
                    u/subr)
          to-eval (cond
                    (symbol? rhs) (sub (list rhs) (var->exa from))
                    (list?   rhs) (sub-form rhs (var->exa from) sub))]
      (a/eval-exacode to-eval))
    (apply u/fail from "is not a valid channel, should be one of:"
           valid-channels)))

(defn send-message [to from]
  (if (valid-channel? to)
    (a/copy from (var->exa to))
    (apply u/fail to "is not a valid channel, should be one of:"
           valid-channels)))

(defmethod a/eval-form '?> [[_ from rhs]]
  (receive-message from rhs true))

(defmethod a/eval-form '?>> [[_ from rhs]]
  (receive-message from rhs false))

(defmethod a/eval-form '! [[_ to from]]
  (send-message to from))

(bind-exa-forms
 {do    do*
  if    if*
  when  when*
  while while*
  ||    fork})
