(ns clolox.ast-printer
  (:require [clojure.string :as str]
            [clolox.expr :as expr]))

(declare parenthesize)

(def printing-strategy
  {:binary (fn [expr]
             (parenthesize (get-in expr [:operator-token :token/lexeme])
                           (:left-expr expr)
                           (:right-expr expr)))

   :grouping (fn [expr]
               (parenthesize "group" (get expr :expr)))

   :literal (fn [expr]
              (if-let [v (:value expr)]
                (str v)
                "nil"))

   :unary (fn [expr]
            (parenthesize (get-in expr [:operator-token :token/lexeme])
                          (:right-expr expr)))})

(defn print-expr
  [expr]
  (let [f (get printing-strategy (:tag expr))]
    (f expr)))

(defn parenthesize
  [fname & exprs]
  (let [f (fn [tokens e] (conj tokens " " (print-expr e)))]
    (str/join (conj (reduce f ["(" fname] exprs) ")"))))


(require '[clolox.expr :as expr])
(require '[clolox.token :as token])

(let [grouping (expr/grouping (expr/literal 123.45))]
  (print-expr grouping))
