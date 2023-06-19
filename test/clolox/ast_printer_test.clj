(ns clolox.ast-printer-test
  (:require [clojure.test :refer :all]
            [clolox.ast-printer :refer :all]
            [clolox.expr :as expr]
            [clolox.token :as token]))

(deftest print-expr-test
  (testing "Should print a literal value"
    (is (= (print-expr (expr/literal 123.45)) "123.45")))
  (testing "Should print a unary expression"
    (let [unary (expr/unary
                 (token/token ::token/minus "-" nil 1)
                 (expr/literal 123.45))]
      (is (= (print-expr unary) "(- 123.45)"))))
  (testing "Should print a grouping expression"
    (let [grouping (expr/grouping (expr/literal 123.45))]
      (is (= (print-expr grouping) "(group 123.45)"))))
  (testing "Should print a binary expression"
    (let [left (expr/unary
                (token/token ::token/minus "-" nil 1)
                (expr/literal 123.45))
          op (token/token ::token/star "*" nil 1)
          right (expr/grouping (expr/literal 678.90))
          binary (expr/binary left op right)]
      (is (= (print-expr binary) "(* (- 123.45) (group 678.9))")))))
