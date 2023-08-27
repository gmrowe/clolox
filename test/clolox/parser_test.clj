(ns clolox.parser-test
  (:require [clojure.test :refer :all]
            [clolox.ast-printer :as ast-printer]
            [clolox.parser :refer :all]
            [clolox.scanner :as scanner]))

(defn current-lexeme-matches
  [psr expected-lexeme]
  (is (= (:token/lexeme (peek-token psr)) expected-lexeme)))

(defn parsed-expr-matches
  [expr-string expected]
  (let [tokens (scanner/scan-tokens (scanner/scanner expr-string))
        parser (parser tokens)
        [expr _] (expression parser)]
    (is (= (ast-printer/print-expr expr) expected))))

(deftest parser-test
  (testing "Primary matches"
    (parsed-expr-matches "false" "false")
    (parsed-expr-matches "true" "true")
    (parsed-expr-matches "nil" "nil")
    (parsed-expr-matches "420" "420.0")
    (parsed-expr-matches "\"This is a string\"" "This is a string")
    (parsed-expr-matches "(420)" "(group 420.0)"))

  (testing "Unary matches"
    (parsed-expr-matches "!true" "(! true)")
    (parsed-expr-matches "-420" "(- 420.0)")
    (parsed-expr-matches "!!false" "(! (! false))")
    (parsed-expr-matches "-(-260)" "(- (group (- 260.0)))"))

  (testing "Factor matches"
    (parsed-expr-matches "2 * 3" "(* 2.0 3.0)")
    (parsed-expr-matches "4 / -0.69"  "(/ 4.0 (- 0.69))")
    (parsed-expr-matches "-(1.69 * 3)" "(- (group (* 1.69 3.0)))"))

  (testing "Term matches"
    (parsed-expr-matches "2 + 3" "(+ 2.0 3.0)")
    (parsed-expr-matches "2 * 3 + 5 / 6" "(+ (* 2.0 3.0) (/ 5.0 6.0))")
    (parsed-expr-matches
     "(2 * 2) + (4 * 5)"
     "(+ (group (* 2.0 2.0)) (group (* 4.0 5.0)))"))

  (testing "Comparison matches"
    (parsed-expr-matches "5 > 3 " "(> 5.0 3.0)")
    (parsed-expr-matches "2 <= (-4 + 6)" "(<= 2.0 (group (+ (- 4.0) 6.0)))"))

  (testing "Equality matches"
    (parsed-expr-matches "3 == 3 != 4" "(!= (== 3.0 3.0) 4.0)")))
