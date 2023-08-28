(ns clolox.interpreter
  (:require [clojure.string :as str]
            [clolox.expr :as expr]
            [clolox.logger :as logger]
            [clolox.token :as token]))

(defn lox-eval-error
  [token msg]
  (ex-info
   msg
   {:token token}))

(defn check-number-operand*
  [token operand]
  (if (number? operand)
    operand
    (throw (lox-eval-error token "Operand must be a number."))))

(defn check-binary-numeric-operation*
  [token left bin-op right]
  (if (and (number? left) (number? right))
    (bin-op left right)
    (throw (lox-eval-error token "Operand must be a number."))))

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
  (let [right (evaluate (::expr/right-expr expr))
        token (::expr/operator-token expr)]
    (case (:token/type token)
      ::token/minus (- (check-number-operand* token right))
      ::token/bang (not (truthy? right)))))

(defn add-or-concat
  [token left right]
  (cond
    (and (number? left) (number? right))
    (+ (double left) (double right))

    (and (string? left) (string? right))
    (str left right)

    :else
    (throw
     (lox-eval-error token "Operands must be two numbers or two strings."))))

(defmethod visit :binary
  [expr]
  (let [left (evaluate (::expr/left-expr expr))
        right (evaluate (::expr/right-expr expr))
        token (::expr/operator-token expr)]
    (case (:token/type token)
      ::token/minus (check-binary-numeric-operation* token left - right)
      ::token/slash (check-binary-numeric-operation* token left / right)
      ::token/star (check-binary-numeric-operation* token left * right)
      ::token/plus (add-or-concat token left right)
      ::token/greater (check-binary-numeric-operation* token left > right)
      ::token/greater-equal (check-binary-numeric-operation* token left >= right)
      ::token/less (check-binary-numeric-operation* token left < right)
      ::token/less-equal (check-binary-numeric-operation* token left <= right)
      ::token/equal-equal (= left right)
      ::token/bang-equal (not (= left right))
      ;; Fallthrough
      nil)))

(defn stringify
  [value]
  (cond
    (nil? value) "nil"
    (number? value) (let [s (str value)]
                      (if (str/ends-with? s ".0")
                        (subs s 0 (- (count s) 2)) ; truncate the decimal
                        s))
    :else (str value)))

(defn interpret
  [expr]
  (try (let [value (evaluate expr)]
         (println (stringify value)))
       (catch Exception e
         (logger/runtime-error! e))))
