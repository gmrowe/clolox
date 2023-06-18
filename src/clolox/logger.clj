(ns clolox.logger
  (:require [clojure.string :as str]))

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
  [line-number message]
  (report line-number "" message))
