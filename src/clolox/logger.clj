(ns clolox.logger
  (:require [clolox.token :as t]))

;; REVIEW: Does this atom belong here or in core?
(def error? (atom false))

(defn report
  [line-number where message]
  ;; TODO: Implement logging using clojure.tools.logging
  ;; https://clojure.github.io/tools.logging/
  (let [err-msg (format "[line %s] Error%s: %s" line-number where message)]
    (.println *err* err-msg)
    (reset! error? true)))

(defn error
  [token message]
  (let [{:token/keys [type line lexeme]} token]
    (if (= ::t/eof type)
      (report line  " at end" message)
      (report line (format " at %s" lexeme) message))))
