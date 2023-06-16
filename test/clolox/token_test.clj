(ns clolox.token-test
  (:require [clojure.test :refer :all]
            [clolox.token :refer :all]))

(deftest a-passing-test
  (testing "as-str returns a human readable representation of a token"
    (let [token (token :clolox.token/left-paren "(" "" 1)]
      (is (= (as-str token) ":clolox.token/left-paren ( ")))))
