(ns clolox.ast-printer
  (:require [clojure.string :as str]
            [clolox.expr :as expr]
            [clolox.stmt :as stmt]))

(defmulti print-expr ::expr/type)

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
  (let [v (::expr/value expr)]
    (if (some? v)
      (str v)
      "nil")))

(defmethod print-expr :unary
  [expr]
  (parenthesize (get-in expr [::expr/operator-token :token/lexeme])
                (::expr/right-expr expr)))



(defmulti print-stmt ::stmt/type)

(defmethod print-stmt :lox-print
  [expr]
  (parenthesize "lox-print" (::stmt/expr expr)))
