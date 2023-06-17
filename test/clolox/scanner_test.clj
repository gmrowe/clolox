(ns clolox.scanner-test
  (:require [clojure.test :refer :all]
            [clolox.scanner :refer :all]
            [clolox.token :as tok]))

(deftest at-end?-test
  (testing "If source is empty at-end always returns true"
    (let [s (scanner "")]
      (is (at-end? s)))))

(defn assert-first-token-matches
  [source expected-string]
  (let [s (scan-tokens (scanner source))]
    (is (= (tok/as-str (first s))
           expected-string))))

(deftest scan-tokens-test
  (testing "scan tokens always ends with :eof token"
    (assert-first-token-matches "" ":clolox.token/eof  null 1"))
  
  ;; Single character tokens
  (testing "scan-token can handle single character tokens:"
    (testing "left-paren"
      (assert-first-token-matches "(" ":clolox.token/left-paren ( null 1"))
    (testing "right-paren"
      (assert-first-token-matches ")" ":clolox.token/right-paren ) null 1"))
    (testing "left-brace"
      (assert-first-token-matches "{" ":clolox.token/left-brace { null 1"))
    (testing "right-brace"
      (assert-first-token-matches "}" ":clolox.token/right-brace } null 1"))
    (testing "comma"
      (assert-first-token-matches "," ":clolox.token/comma , null 1"))
    (testing "dot"
      (assert-first-token-matches "." ":clolox.token/dot . null 1"))
    (testing "minus"
      (assert-first-token-matches "-" ":clolox.token/minus - null 1"))
    (testing "plus"
      (assert-first-token-matches "+" ":clolox.token/plus + null 1"))
    (testing "semicolon"
      (assert-first-token-matches ";" ":clolox.token/semicolon ; null 1"))
    (testing "star"
      (assert-first-token-matches "*" ":clolox.token/star * null 1")))

  ;; One or two character tokens
  (testing "scan-token can handle tokens that can either be one or two chars:"
    (testing "bang"
      (assert-first-token-matches "!" ":clolox.token/bang ! null 1"))
    (testing "bang-equal"
      (assert-first-token-matches "!=" ":clolox.token/bang-equal != null 1"))
    (testing "equal"
      (assert-first-token-matches "=" ":clolox.token/equal = null 1"))
    (testing "equal-equal"
      (assert-first-token-matches "==" ":clolox.token/equal-equal == null 1"))
    (testing "less"
      (assert-first-token-matches "<" ":clolox.token/less < null 1"))
    (testing "less-equal"
      (assert-first-token-matches "<=" ":clolox.token/less-equal <= null 1"))
    (testing "greater"
      (assert-first-token-matches ">" ":clolox.token/greater > null 1"))
    (testing "greater-equal"
      (assert-first-token-matches ">=" ":clolox.token/greater-equal >= null 1")))

  ;; One character or variable length token
  (testing "a slash can be division or start of a comment:"
    (testing "division"
      (assert-first-token-matches "/" ":clolox.token/slash / null 1"))
    (testing "comment"
      (testing "text of a comment is discarded by the scanner"
        (assert-first-token-matches "// This is a comment" ":clolox.token/eof  null 1"))))

  ;; Whitespace and newlines
  (testing "the line number is incremented when a newline is encountered"
    (assert-first-token-matches "\n\n\n" ":clolox.token/eof  null 4"))
  (testing "whitespace is skipped"
    (assert-first-token-matches "   " ":clolox.token/eof  null 1"))
  (testing "tabs are skipped"
    (assert-first-token-matches "\t\t\t" ":clolox.token/eof  null 1"))
  (testing "returns are skipped"
    (assert-first-token-matches "\r\r\r" ":clolox.token/eof  null 1"))

  ;; Strings
  (testing "A quote starts a string"
    (assert-first-token-matches
     "\"This is a string\""
     ":clolox.token/string \"This is a string\" This is a string 1")))

