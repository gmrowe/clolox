(ns clolox.interpreter-test
  (:require [clojure.test :refer :all]
            [clolox.interpreter :refer :all]
            [clolox.parser :as parser]
            [clolox.scanner :as scanner]
            [clolox.token :as token]))

(defn literal-is
  [lit-expr-string expected-literal]
  (let [literal (-> lit-expr-string
                    scanner/tokenize
                    parser/parser
                    parser/parse
                    evaluate)]
    (is (= literal expected-literal))))


(deftest evaluate-test
  
  (testing "Can evaluate a literal"
    (testing "nil"
      (literal-is "nil" nil))
    (testing "boolean"
      (literal-is "true" true)
      (literal-is "false" false))
    (testing "number"
      (literal-is "12" 12.0))
    (testing "string"
      (literal-is "\"A string\"" "A string")))

  (testing "Can evaluate a unary"
    (testing "negation"
      (literal-is "-12" -12.0))
    (testing "bang (boolean inversion)"
      (literal-is "!true" false)
      (literal-is "!false" true)
      (literal-is "!nil" true)
      (literal-is "!1" false)
      (literal-is "!\"A string\"" false)))

  (testing "Can evaluate a grouping"
    (testing "literal"
      (literal-is "(12)" 12.0))
    (testing  "binary expression"
      (literal-is "16 - (8 + 4)" 4.0)))

  (testing "Can evaluate a binary"
    (testing "subtraction expression"
      (literal-is "42 - 16" 26.0)
      (literal-is "26 - -14" 40.0)
      (literal-is "-26.5 - 11.3" -37.8))
    (testing "division expression"
      (literal-is "48 / 4" 12.0))
    (testing "multiplication expression"
      (literal-is "12 * 9" 108.0))
    (testing  "numeric addition"
      (literal-is "26 + -13" 13.0))
    (testing "string concatination"
      (literal-is "\"A\" + \" string\"" "A string"))
    (testing "greater than comparison"
      (literal-is "4 > 2" true)
      (literal-is "(2 * 3) > (7 + 1)" false))
    (testing "greater than or equal comparison"
      (literal-is "6 >= 6" true)
      (literal-is "5 >= 1" true)
      (literal-is "-42 >= 69" false))
    (testing "less than comparison"
      (literal-is "32 < 108" true)
      (literal-is "(12 * 12) < (9 * 9)" false))
    (testing "less than or equal comparison"
      (literal-is "6 <= 6" true)
      (literal-is "5 <= 1" false)
      (literal-is "-42 <= 69" true))
    (testing "equality expression"
      (literal-is "(9 * 9) == (61 + 20)" true)
      (literal-is "nil == nil" true)
      (literal-is "13 == nil" false)
      (literal-is "(8 * 9) == (7 * 8)" false))
    (testing "inequality expression"
      (literal-is "(9 * 9) != (61 + 20)" false)
      (literal-is "nil != nil" false)
      (literal-is "13 != nil" true)
      (literal-is "(8 * 9) != (7 * 8)" true))))
