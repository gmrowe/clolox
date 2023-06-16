(ns clolox.scanner
  (:require [clolox.token :as token]
            [clolox.core :as clolox]))

(defn scanner
  [source]
  #:scanner{:source source
            :tokens []
            :start 0
            :current 0
            :line 1})

(defn at-end?
  [sc]
  (>= (:scanner/current sc) (count (:scanner/source sc))))

(defn add-token
  ([sc token-type] (add-token sc token-type nil))
  ([sc token-type literal]
   (let [{:scanner/keys [source start current line]} sc
         text (subs source start current)]
     (update sc :scanner/tokens conj (token/token token-type text literal line)))))

(defn current-char
  [sc]
  (.charAt (:scanner/source sc) (:scanner/current sc)))

(defn advance
  [sc]
  [(current-char sc) (update sc :scanner/current inc)])

(defn next-token-matches?
  [sc c]
  (and (not (at-end? sc))
       (= (current-char sc)) c))

;; REVIEW: can I come up with a better name for this function?
(defn advance-if-matches
  [s0 expected tok-a tok-b]
  (if (next-token-matches? s0 expected)
    (let [[_ s1] (advance s0)]
      (add-token s1 tok-a))
    (add-token s0 tok-b)))

(defn scan-token
  [sc]
  (let [[t s] (advance sc)]
    (case t
      \( (add-token s ::token/left-paren)
      \) (add-token s ::token/right-paren)
      \{ (add-token s ::token/left-brace)
      \} (add-token s ::token/right-brace)
      \, (add-token s ::token/comma)
      \. (add-token s ::token/dot)
      \- (add-token s ::token/minus)
      \+ (add-token s ::token/plus)
      \; (add-token s ::token/semicolon)
      \* (add-token s ::token/star)
      \! (advance-if-matches s \= ::token/bang-equal ::token/bang)
      \= (advance-if-matches s \= ::token/equal-equal ::token/equal)
      \< (advance-if-matches s \= ::token/less-equal ::token/less)
      \> (advance-if-matches s \= ::token/greater-equal ::token/greater)
      (clolox/error
       (:scanner/line s)
       (format "ERROR clolox.scanner/scan-token: unknown token \"%s\"" t)))))

(defn scan-tokens
  [sc]
  (loop [sc sc]
    (if (at-end? sc)
      (add-token sc ::token/eof)
      (recur (scan-token (assoc sc :scanner/start (:scanner/current sc)))))))

