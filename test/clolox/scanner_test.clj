(ns clolox.scanner-test
  (:require [clojure.test :refer :all]
            [clolox.scanner :refer :all]
            [clolox.token :as tok]))

(deftest a-passing-test
  (testing "I pass"
    (is (= 1 1))))

(deftest at-end?-test
  (testing "If source is empty at-end always returns true"
    (let [s (scanner "")]
      (is (at-end? s)))))

(defn assert-first-token-matches
  [source expected-string]
  (let [s (scan-tokens (scanner source))]
    (is (= (tok/as-str (first (:scanner/tokens s)))
           expected-string))))

(deftest scan-tokens-test
  (testing "scan tokens always ends with :eof token"
    (assert-first-token-matches "" ":clolox.token/eof  null"))

  (testing "scan-token can handle single character tokens:"
    (testing "left-paren"
      (assert-first-token-matches "(" ":clolox.token/left-paren ( null"))
    (testing "right-paren"
      (assert-first-token-matches ")" ":clolox.token/right-paren ) null"))
    (testing "left-brace"
      (assert-first-token-matches "{" ":clolox.token/left-brace { null"))
    (testing "right-brace"
      (assert-first-token-matches "}" ":clolox.token/right-brace } null"))
    (testing "comma"
      (assert-first-token-matches "," ":clolox.token/comma , null"))
    (testing "dot"
      (assert-first-token-matches "." ":clolox.token/dot . null"))
    (testing "minus"
      (assert-first-token-matches "-" ":clolox.token/minus - null"))
    (testing "plus"
      (assert-first-token-matches "+" ":clolox.token/plus + null"))
    (testing "semicolon"
      (assert-first-token-matches ";" ":clolox.token/semicolon ; null"))
    (testing "star"
      (assert-first-token-matches "*" ":clolox.token/star * null")))

  (testing "scan-token can handle tokens that can either be one or two chars:"
    (testing "bang"
      (assert-first-token-matches "!" ":clolox.token/bang ! null"))
    (testing "bang-equal"
      (assert-first-token-matches "!=" ":clolox.token/bang-equal != null"))
    (testing "equal"
      (assert-first-token-matches "=" ":clolox.token/equal = null"))
    (testing "equal-equal"
      (assert-first-token-matches "==" ":clolox.token/equal-equal == null"))
    (testing "less"
      (assert-first-token-matches "<" ":clolox.token/less < null"))
    (testing "less-equal"
      (assert-first-token-matches "<=" ":clolox.token/less-equal <= null"))
    (testing "greater"
      (assert-first-token-matches ">" ":clolox.token/greater > null"))
    (testing "greater-equal"
      (assert-first-token-matches ">=" ":clolox.token/greater-equal >= null"))))
