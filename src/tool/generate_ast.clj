(ns tool.generate-ast
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:gen-class))

(defn define-function
  [s]
  (let [[fname fields] (map str/trim (str/split s #":"))
        header [(format "(defn %s" fname)
                (format "[%s]" fields)
                (format "{" fname)]
        kvs (map (fn [field] (format "::%s %s" field field)) (str/split fields #"\s+"))
        tag [(format "::tag :%s" fname)]
        footer ["})"]]
    (str/join \newline (concat header kvs tag footer))))


(defn write-source-as-string
  [basename types]
  (let [ns [(format "(ns clolox.%s)" basename)]
        defs (map define-function types)]
    (str/join \newline (interpose "" (concat ns defs)))))

(defn gen-ast-source
  [path basename types]
  (let [filename (format "/%s.clj" basename)]
    (with-open [w (io/writer (str path filename) :encoding "UTF-8")]
      (.write w (write-source-as-string basename types))
      (.newLine w))))

(def ast-list
  ["binary : left-expr operator-token right-expr"
   "grouping: expr"
   "literal: value"
   "unary: operator-token right-expr"])

(defn print-usage-and-exit
  []
  (do
    (println "Usage: lein run-generate-ast <output-directory>")
    (System/exit 64)))

(defn -main
  [& args]
  (if (not= 1 (count args))
    (print-usage-and-exit)
    (gen-ast-source (first args) "expr" ast-list)))
