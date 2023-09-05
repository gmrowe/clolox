(ns clolox.ast-printer-test
  (:require [clojure.test :refer [deftest is testing]]
            [clolox.ast-printer :refer :all]
            [clolox.expr :as expr]
            [clolox.stmt :as stmt]
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

(deftest print-stmt-test
  (testing "Should represent a printed lox expression"
    (let [unary (expr/unary
                 (token/token ::token/minus "-" nil 1)
                 (expr/literal 123.45))
          stmt (stmt/lox-print unary)]
      (is (= (print-stmt stmt) "(lox-print (- 123.45))")))))
