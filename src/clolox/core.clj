(ns clolox.core
  (:require [clojure.string :as str])
  (:gen-class))

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

(defn tokenize
  [source]
  (str/split source #"\s+"))

(defn run
  [source]
  (let [tokens (tokenize source)]
    (println (str/join "\n" tokens))))

(defn run-file
  [path]
  ;; The empty Srting array here is necessary so that the
  ;; correct Paths/get method is resolved due to varagrs in
  ;; the Java method.0
  (let [p (java.nio.file.Paths/get path (make-array String 0))
        bytes (java.nio.file.Files/readAllBytes p)]
    (run (String. bytes (java.nio.charset.Charset/defaultCharset)))
    (when @error?
      (System/exit 65))))

(defn run-prompt
  []
  (do
    (print "> ")
    (flush)
    (when-let [line (read-line)]
      (run (str/trim line))
      (reset! error? false)
      (recur))))

(defn show-usage-and-exit
  []
  (do
    (println "Usage: clojure -M -m clolox [script]")
    (System/exit 64)))

(defn -main
  [& args]
  (cond
    (zero? (count args)) (run-prompt)
    (== (count args) 1) (run-file (first args))
    :else (show-usage-and-exit)))
