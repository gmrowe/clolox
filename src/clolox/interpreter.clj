(ns clolox.interpreter
  (:require [clojure.string :as str]
            [clolox.expr :as expr]
            [clolox.token :as token]))

(defmulti visit ::expr/type)

(defn evaluate
  [expr]
  (visit expr))

(defmethod visit :literal
  [expr]
  (::expr/value expr))

(defmethod visit :grouping
  [expr]
  (evaluate (::expr/expr expr)))

(defn truthy?
  [obj]
  (cond
    (nil? obj) false
    (boolean? obj) obj
    :else true))

(defmethod visit :unary
  [expr]
  (let [right (evaluate (::expr/right-expr expr))]
    (case (-> expr ::expr/operator-token :token/type)
      ::token/minus (- (double right))
      ::token/bang (not (truthy? right)))))

(defn add-or-concat
  [left right]
  (cond
    (and (number? left) (number? right))
    (+ (double left) (double right))

    (and (string? left) (string? right))
    (str left right)

    :else nil))

(defmethod visit :binary
  [expr]
  (let [left (evaluate (::expr/left-expr expr))
        right (evaluate (::expr/right-expr expr))]
    (case (-> expr ::expr/operator-token :token/type)
      ::token/minus (- (double left) (double right))
      ::token/slash (/ (double left) (double right))
      ::token/star (* (double left) (double right))
      ::token/plus (add-or-concat left right)
      ::token/greater (> (double left) (double right))
      ::token/greater-equal (>= (double left) (double right))
      ::token/less (< (double left) (double right))
      ::token/less-equal (<= (double left) (double right))
      ::token/equal-equal (= left right)
      ::token/bang-equal (not (= left right))
      ;; Fallthrough
      nil)))
