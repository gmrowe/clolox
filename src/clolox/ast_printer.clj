(ns clolox.ast-printer
  (:require [clojure.string :as str]
            [clolox.expr :as expr]))

(defmulti print-expr ::expr/tag)

(defn parenthesize
  [fname & exprs]
  (let [f (fn [tokens e] (conj tokens " " (print-expr e)))]
    (str/join (conj (reduce f ["(" fname] exprs) ")"))))

(defmethod print-expr :binary
  [expr]
  (parenthesize (get-in expr [::expr/operator-token :token/lexeme])
                (::expr/left-expr expr)
                (::expr/right-expr expr)))

(defmethod print-expr :grouping
  [expr]
  (parenthesize "group" (::expr/expr expr)))

(defmethod print-expr :literal
  [expr]
  (if-let [v (::expr/value expr)]
    (str v)
    "nil"))

(defmethod print-expr :unary
  [expr]
  (parenthesize (get-in expr [::expr/operator-token :token/lexeme])
                (::expr/right-expr expr)))



