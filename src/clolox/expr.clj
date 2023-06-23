(clojure.core/ns clolox.expr)

(clojure.core/defn
 binary
 [left-expr operator-token right-expr]
 #:clolox.expr{:left-expr left-expr,
               :operator-token operator-token,
               :right-expr right-expr,
               :type :binary})

(clojure.core/defn
 grouping
 [expr]
 #:clolox.expr{:expr expr, :type :grouping})

(clojure.core/defn
 literal
 [value]
 #:clolox.expr{:value value, :type :literal})

(clojure.core/defn
 unary
 [operator-token right-expr]
 #:clolox.expr{:operator-token operator-token,
               :right-expr right-expr,
               :type :unary})

