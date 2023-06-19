(ns clolox.ast-printer
  (:require [clojure.string :as str]
            [clolox.expr :as expr]))

(declare parenthesize)

(def printing-strategy
  {:binary (fn [expr]
             (parenthesize (get-in expr [::expr/operator-token :token/lexeme])
                           (::expr/left-expr expr)
                           (::expr/right-expr expr)))

   :grouping (fn [expr]
               (parenthesize "group" (::expr/expr expr)))

   :literal (fn [expr]
              (if-let [v (::expr/value expr)]
                (str v)
                "nil"))

   :unary (fn [expr]
            (parenthesize (get-in expr [::expr/operator-token :token/lexeme])
                          (::expr/right-expr expr)))})

(defn print-expr
  [expr]
  (let [f (get printing-strategy (::expr/tag expr))]
    (f expr)))

(defn parenthesize
  [fname & exprs]
  (let [f (fn [tokens e] (conj tokens " " (print-expr e)))]
    (str/join (conj (reduce f ["(" fname] exprs) ")"))))


