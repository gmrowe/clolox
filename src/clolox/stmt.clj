(clojure.core/ns clolox.stmt)

(clojure.core/defn
 expression
 [expr]
 #:clolox.stmt{:expr expr, :type :expression})

(clojure.core/defn
 lox-print
 [expr]
 #:clolox.stmt{:expr expr, :type :lox-print})

