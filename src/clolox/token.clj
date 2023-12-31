(ns clolox.token)

(def token-types
  #{; Single character tokens
    ::left-paren ::right-paren ::left-brace ::right-brace ::comma ::dot
    ::minus ::plus ::semicolon ::slash ::star

    ; One or two character tokens
    ::bang ::bang-equal
    ::equal ::equal-equal
    ::greater ::greater-equal
    ::less ::less-equal

    ; Literals
    ::identifier ::string ::number

    ; Keywords
    ::and ::class ::else ::false ::fun ::for ::if ::nil ::or
    ::print ::return ::super ::this ::true ::var ::while

    ::eof})

(defn token
  [token-type lexeme literal line]
  #:token{:type token-type
          :lexeme lexeme
          :literal literal
          :line line})

(defn as-str
  [tok]
  (let [{:token/keys [type lexeme literal line]} tok]
    (format "%s %s %s %s" type lexeme literal line)))
