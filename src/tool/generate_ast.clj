(ns tool.generate-ast
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [clojure.string :as str])
  (:gen-class))

(defn ast-namespace
  [basename]
  (format "clolox.%s" basename))

(defn make-map
  [basename fname fields]
  (let [namespace (ast-namespace basename)
        add-map-element #(assoc %1 (keyword namespace %2) (symbol %2))]
    (assoc
     (reduce add-map-element {} fields)
     (keyword namespace "type") (keyword fname))))

(defn define-function
  [basename s]
  (let [[fname field-str] (map str/trim (str/split s #":"))
        fields (str/split field-str #"\s+")]
    `(defn ~(symbol fname)
       ~(vec (map symbol fields))
       ~(make-map basename fname fields))))

(defn write-source-as-string
  [basename types]
  (let [ns-header `(ns ~(symbol (ast-namespace basename)))
        defs (map #(define-function basename %) types)]
    (with-out-str
      (pp/pprint ns-header)
      (doseq [d defs]
        (println)
        (pp/pprint d)))))

(defn gen-ast-source
  [path basename types]
  (let [filename (format "/%s.clj" basename)]
    (with-open [w (io/writer (str path filename) :encoding "UTF-8")]
      (.write w (write-source-as-string basename types))
      (.newLine w))))

(def ast-expr-list
  {:basename "expr"
   :ast-list  ["binary : left-expr operator-token right-expr"
               "grouping: expr"
               "literal: value"
               "unary: operator-token right-expr"
               "error: message"]})

(def ast-stmt-list
  {:basename "stmt"
   :ast-list  ["expression: expr"
               "lox-print: expr"]})

(defn print-usage-and-exit
  []
  (println "Usage: lein run-generate-ast <output-directory>")
  (System/exit 64))

(defn -main
  [& args]
  (if (not= 1 (count args))
    (print-usage-and-exit)
    (doseq [{:keys [basename ast-list]} [ast-expr-list ast-stmt-list]]
      (gen-ast-source (first args) basename ast-list))))
