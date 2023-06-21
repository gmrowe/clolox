(ns clolox.core
  (:require [clojure.string :as str]
            [clolox.logger :as logger]
            [clolox.scanner :as scanner])
  (:gen-class))

(defn tokenize
  [source]
  (scanner/scan-tokens (scanner/scanner source)))

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
    (when @logger/error?
      (System/exit 65))))

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
