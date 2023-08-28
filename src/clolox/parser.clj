(ns clolox.parser
  (:require [clolox.expr :as expr]
            [clolox.logger :as logger]
            [clolox.token :as t]))

(defn parser
  [tokens]
  #:parser{:tokens (vec tokens)
           :current 0})

(defn nth-token
  [psr n]
  (nth (:parser/tokens psr) n))

(defn peek-token
  [psr]
  (nth-token psr (:parser/current psr)))

(defn at-end?
  [psr]
  (= (:token/type (peek-token psr)) ::t/eof))

(defn advance
  [psr]
  (when-not (at-end? psr)
    (update psr :parser/current inc)))

(defn previous
  [psr]
  (nth-token psr (dec (:parser/current psr))))

(defn check
  [psr token-type]
  (and (not (at-end? psr)) (= (:token/type (peek-token psr)) token-type)))

(defn match
  [psr & token-types]
  (some
   (fn [token-type] (= (:token/type (peek-token psr)) token-type))
   token-types))

(defn error
  [token msg]
  (logger/error! token msg)
  (ex-info
   "ParseError"
   {:type ::parse-error
    :token token
    :msg msg}))

(defn consume
  [psr token-type msg]
  (if (check psr token-type)
    (advance psr)
    (throw (error (peek-token psr) msg))))

(declare expression)

(defn primary
  [psr]
  (cond
    (match psr ::t/false) [(expr/literal false) (advance psr)]
    (match psr ::t/true) [(expr/literal true) (advance psr)]
    (match psr ::t/nil) [(expr/literal nil) (advance psr)]

    (match psr ::t/number ::t/string)
    [(expr/literal (:token/literal (peek-token psr))) (advance psr)]

    (match psr ::t/left-paren)
    (let [[expr p] (expression (advance psr))]
      [(expr/grouping expr)
       (consume p ::t/right-paren "Expect ')' after expression.")])
    :else (throw (error (peek-token psr) "Expect expression."))))

(defn unary
  [psr]
  (if (match psr ::t/bang ::t/minus)
    (let [[right p1] (unary (advance psr))]
      [(expr/unary (peek-token psr) right) p1])
    (primary psr)))

(defn factor
  [psr]
  (let [[left p] (unary psr)]
    (loop [expr left
           p p]
      (if (match p  ::t/slash ::t/star)
        (let [operator (peek-token p)
              [right p1] (unary (advance p))]
          (recur (expr/binary expr operator right) p1))
        [expr p]))))

(defn term
  [psr]
  (let [[left p] (factor psr)]
    (loop [expr left
           p p]
      (if (match p ::t/plus ::t/minus)
        (let [operator (peek-token p)
              [right p1] (factor (advance p))]
          (recur (expr/binary expr operator right) p1))
        [expr p]))))

(defn comparison
  [psr]
  (let [[left p] (term psr)]
    (loop [expr left
           p p]
      (if (match p ::t/greater ::t/greater-equal ::t/less ::t/less-equal)
        (let [operator (peek-token p)
              [right p1] (term (advance p))]
          (recur (expr/binary expr operator right) p1))
        [expr p]))))

(defn equality
  [psr]
  (let [[left p] (comparison psr)]
    (loop [expr left
           p p]
      (if (match p ::t/bang-equal ::t/equal-equal)
        (let [operator (peek-token p)
              [right p1] (comparison (advance p))]
          (recur (expr/binary expr operator right) p1))
        [expr p]))))

(defn expression
  [psr]
  (equality psr))

(defn synchronize-point?
  [psr]
  (or (at-end? psr)
      (= ::t/semicolon (:token/type (previous psr)))
      (#{::t/class ::t/fun ::t/var ::t/for
         ::t/if ::t/while ::t/print ::t/return}
       (peek psr))))

(defn synchronize
  [psr]
  (loop [p (advance psr)]
    (if (synchronize-point? p)
      p
      (recur (advance p)))))

(defn parse
  [psr]
  (try (first (expression psr))
       (catch Exception _
         nil)))

(defn parse-tokens
  [tokens]
  (-> tokens parser parse))
