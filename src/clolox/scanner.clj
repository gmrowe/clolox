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

(defn peek-char
  [sc]
  (if (at-end? sc)
    \u0000
    (.charAt (:scanner/source sc) (:scanner/current sc))))

(defn advance
  [sc]
  [(peek-char sc) (update sc :scanner/current inc)])

(defn next-token-matches?
  [sc c]
  (and (not (at-end? sc)) (= (peek-char sc) c)))

;; REVIEW: can I come up with a better name for this function?
(defn advance-if-matches
  [sc expected tok-a tok-b]
  (if (next-token-matches? sc expected)
    (add-token (second (advance sc)) tok-a)
    (add-token sc tok-b)))

(defn scan-comment
  [sc]
  (loop [s sc]
    (if (or (at-end? s) (= (peek-char s) \newline))
      s
      (recur (second (advance s))))))

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
      \/ (if (next-token-matches? s \/)
           (scan-comment s)
           (add-token s ::token/slash))
      (\tab \space \return) s
      \newline (update s :scanner/line inc)
      (clolox/error
       (:scanner/line s)
       (format "ERROR clolox.scanner/scan-token: unknown token \"%s\"" t)))))

(defn scan-tokens
  [sc]
  (loop [s0 sc]
    ;; Move the start index to the end of the last token scanned
    (let [s1 (assoc s0 :scanner/start (:scanner/current s0))]
      (if (at-end? s1)
        (:scanner/tokens (add-token s1 ::token/eof))
        (recur (scan-token s1))))))
