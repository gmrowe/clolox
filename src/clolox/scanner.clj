(ns clolox.scanner
  (:refer-clojure :exclude [peek])
  (:require [clolox.token :as token]
            [clolox.logger :as logger]))

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

(defn peek
  [sc]
  (if (at-end? sc)
    \u0000
    (.charAt (:scanner/source sc) (:scanner/current sc))))

(defn advance
  [sc]
  [(peek sc) (update sc :scanner/current inc)])

(defn qadvance
  [sc]
  (second (advance sc)))

(defn peek-next
  [sc]
  (if (at-end? sc)
    \u0000
    (peek (qadvance sc))))


(defn next-token-matches?
  [sc c]
  (and (not (at-end? sc)) (= (peek sc) c)))

;; REVIEW: can I come up with a better name for this function?
(defn advance-if-matches
  [sc expected tok-a tok-b]
  (if (next-token-matches? sc expected)
    (add-token (qadvance sc) tok-a)
    (add-token sc tok-b)))

(defn scan-comment
  [sc]
  (loop [s sc]
    (if (or (at-end? s) (= (peek s) \newline))
      s
      (recur (qadvance s)))))

(defn scan-string
  [sc]
  (loop [{:scanner/keys [line source start current] :as sc} sc]
    (cond
      ;; We reach the end of the file without terminating the string
      ;; report an error and continue
      (at-end? sc)
      (do (logger/error line "Unterminated string") sc)

      ;; We reach the closing quotation marks
      (= (peek sc) \")
      (add-token
       (qadvance sc)
       ::token/string
       (subs source (inc start) current))

      ;; We reach an internal newline in the string
      (= (peek sc) \newline)
      (recur (qadvance (update sc :scanner/line inc)))

      ;; Nothing... we keep on parsing
      :else (recur (qadvance sc)))))

(defn scan-number
  [sc]
  (loop [{:scanner/keys [source start current] :as sc} sc
         fractional? false]
    (cond
      ;; We found another digit.. continue parsing
      (Character/isDigit (peek sc))
      (recur (qadvance sc) fractional?)

      ;; We found a decimal point, if this is the first decimal we
      ;; have seen (not fractional?) then we consume it and continue to parse
      ;; otherwise we fall through and parse what we have thus far
      (and (= \. (peek sc))
           (not fractional?)
           (Character/isDigit (peek-next sc)))
      (recur (qadvance sc) true)

      ;; We are at neither a digit, nor a first decimal point. Lets try to parse
      ;; what we have so far
      :else (add-token
             sc
             ::token/number
             (Double/parseDouble (subs source start current))))))

(defn lox-identifier-start?
  [c]
  (or (= c \_) (Character/isLetter c)))

(defn lox-identifier-part?
  [c]
  (or (lox-identifier-start? c) (Character/isDigit c)))

(def keywords
  {"and" ::token/and
   "class" ::token/class
   "else" ::token/else
   "false" ::token/false
   "for" ::token/for
   "fun" ::token/fun
   "if" ::token/if
   "nil" ::token/nil
   "or" ::token/or
   "print" ::token/print
   "return" ::token/return
   "super" ::token/super
   "this" ::token/this
   "true" ::token/true
   "var" ::token/var
   "while" ::token/while})

(defn add-keyword-or-identifier
  [sc]
  (let [{:scanner/keys [source start current] :as sc} sc
        text (subs source start current)]
    (if-let [kw (get keywords text)]
      (add-token sc kw)
      (add-token sc ::token/identifier))))

(defn scan-identifier
  [sc]
  (loop [sc sc]
    (if (or (at-end? sc) (not (lox-identifier-part? (peek sc))))
      (add-keyword-or-identifier sc)
      (recur (qadvance sc)))))

(defn identifier-or-numeric-literal-or-error
  [t sc]
  (cond
    (Character/isDigit t) (scan-number sc)
    (lox-identifier-start? t) (scan-identifier sc)
    :else  (do (logger/error
                (:scanner/line sc)
                (format "ERROR clolox.scanner/scan-token: unknown token \"%s\"" t))
               sc)))

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
      \" (scan-string s)
      ;; Default (fall through)
      (identifier-or-numeric-literal-or-error t s))))

(defn scan-tokens
  [sc]
  (loop [s0 sc]
    ;; Move the start index to the end of the last token scanned
    (let [s1 (assoc s0 :scanner/start (:scanner/current s0))]
      (if (at-end? s1)
        (:scanner/tokens (add-token s1 ::token/eof))
        (recur (scan-token s1))))))

