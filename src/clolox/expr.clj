(ns clolox.expr)

(defn binary
  [left-expr operator-token right-expr]
  {::left-expr left-expr
   ::operator-token operator-token
   ::right-expr right-expr
   ::tag :binary})

(defn grouping
  [expr]
  {::expr expr
   ::tag :grouping})

(defn literal
  [value]
  {::value value
   ::tag :literal})

(defn unary
  [operator-token right-expr]
  {::operator-token operator-token
   ::right-expr right-expr
   ::tag :unary})
