(ns clolox.core
  (:require [clojure.string :as str]
            [clolox.interpreter :as interpreter]
            [clolox.logger :as logger]
            [clolox.parser :as parser]
            [clolox.scanner :as scanner])
  (:gen-class))

(defn run
  [source]
  (let [tokens (scanner/tokenize source)
        psr (parser/parser tokens)
        expr (parser/parse psr)]
    (when (not @logger/error?)
      (interpreter/interpret expr))))

(defn run-file
  [path]
  ;; The empty Srting array here is necessary so that the
  ;; correct Paths/get method is resolved due to varagrs in
  ;; the Java method.
  (let [p (java.nio.file.Paths/get path (make-array String 0))
        bytes (java.nio.file.Files/readAllBytes p)]
    (run (String. bytes (java.nio.charset.Charset/defaultCharset)))
    (cond
      @logger/error? (System/exit 65)
      @logger/runtime-error? (System/exit 70))))

(defn run-prompt
  []
  (print "> ")
  (flush)
  (when-let [line (read-line)]
    (run (str/trim line))
    (reset! logger/error? false)
    (recur)))

(defn show-usage-and-exit
  []
  (println "Usage: clojure [script]")
  (System/exit 64))

(defn -main
  [& args]
  (cond
    (zero? (count args)) (run-prompt)
    (== (count args) 1) (run-file (first args))
    :else (show-usage-and-exit)))
