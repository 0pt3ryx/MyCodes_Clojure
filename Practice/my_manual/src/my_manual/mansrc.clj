(ns my-manual.mansrc)

(comment
  ;; Collection
  ;; #collection #list #vector #map #set

  ;list
  '(1 :keyword 3 "a string")
  (list 1 :keyword 3 "a string")

  ;vector
  [1 :keyword 3 "a string"]

  ;map
  {:key0 "value0" :key1 "value1"}

  ;set
  #{1 :keyword 3 "a string"}
  (set [1 :keyword :keyword 3 "a string" 3])                ; converting a vector into a set

  ;collection functions
  (first '(1 2 3 4))
  (rest '(1 2 3 4))
  (cons 1 '(2 3 4))
  (nth [1 2 3 4] 2)
  (last '(1 2 3 4))
  (count '(1 2 3 4))
  (conj '(2 3 4) 1)                                         ; conjunction for list
  (conj [1 2 3] 4)                                          ; conjunction for vector

  (get {:key0 "val0" :key1 "val1"} :key1)                   ; args: (map key)
  (keys {:key0 "val0" :key1 "val1"})                        ; args: map
  (vals {:key0 "val0" :key1 "val1"})                        ; args: map
  (assoc {:key0 "val0" :key1 "val1"} :key2 "val12")         ; args: (map key value)
  (dissoc {:key0 "val0" :key1 "val1"} :key0)                ; args: (map key)
  (merge {:key0 "val0" :key1 "val1"}                        ; args: (map map ...)
         {:key0 "val12" :key2 "val2"}
         {:key3 "val3"})

  (clojure.set/union #{:r :b :w} #{:w :p :y})               ; args: (set set ...)
  (clojure.set/difference #{:r :b :w} #{:w :p :y})          ; args: (set set ...)
  (clojure.set/intersection #{:r :b :w} #{:w :p :y})        ; args: (set set ...)
  (get #{:r :b :w} :r)                                      ; == (#{:r :b :w} :r)
                                                            ; Set is like map whose values dare identical to their corresponding keys.
  (contains? #{:r :b :w} :r)
  (contains? #{1 nil} nil)

  (seq [1 2 3])                                             ; 'seq' converts a collection into a sequence.
  (seq [])                                                  ; If collection is empty, 'seq' returns 'nil'.
)

(comment
  ;; Binding symbols and defining functions
  ;; #def #let #defn #fn #function #namespace

  ; To define a symbol (global)
  (def nickname "0pt3ryx")                                  ; 'namespace'/nickname

  ; To bind a symbol (local)
  (let [nickname "0pt3ryx"] nickname)                       ; The scope of the symbol is parenthesis of 'let'.

  ; To define a function
  (defn what-you-said [arg] (str "You said: " arg))         ; (defn Function-Name [arg0 arg1 ...] (Function-Body))

  ; To define an anonymous function and to call it
  ((fn [arg] (str "You said: " arg)) "Hello")               ; Long version
                                                            ; (def what-you-said (fn [arg] (str "You said: " arg)))
                                                            ; == (defn what-you-said [arg] (str "You said: " arg))
  (#(str "You said: " %) "Hello")                           ; Short version with one argument ('%' is an argument)
  (#(str "You said: " %1 " and " %2) "Hello" "World!")      ; Short version with multiple arguments ('%n' is the nth argument.)

  ; To create a namespace or/and to switch to the namespace
  (ns opteryx.world0)

  ; To load a namespace
  (require 'clojure.set)

  ; To alias a namespace
  (require '[opteryx.world0 :as ow0])                       ; You can use symbols in the namespace whose name is 'opteryx.world0'
                                                            ;   like ow0/'symbol-name'
  (ns opteryx.world1
    (:require [opteryx.world0 :as ow0]))

  ; To load all symbols in a namespace
  (ns opteryx.world2                                        ; You can use symbols in the designated namespace without its name.
    (:require [opteryx.world0 :refer :all]                  ; There is a likelihood of collision of symbols
              [opteryx.world1 :refer :all]))                ;   whose names are identical to each other.
)

(comment
  ;; Logic Check
  ;; #true #false #nil #negation #equal #collection #empty #every #not-any #some

  ; To check if true/false
  (true? true)
  (false? false)

  ; To check if nil
  (nil? nil)

  ; To negate
  (not true)
  (not nil)                                                 ; 'nil' is logically false. (This is important.)

  ; To check if equal
  (= 1 1)
  (= '(:red :blue) [:red :blue])                            ; Equality of collection is a little bit different.

  ; To check if not equal
  (not (= 1 1))                                             ; Long version
  (not= 1 1)                                                ; Short version

  ;; Logic check in collection
  ; To check if a collection is empty
  (empty? '())                                              ; (defn empty? [coll] (not (seq coll)))
  (not (empty? []))                                         ; It is idiomatic to use 'seq' to check a collection is not empty
  (seq [])                                                  ;   instead of 'not' + 'empty?', because 'nil' is logically false.

  ; To check if every elements in a collection meets a condition
  (every? odd? [1 3 5])                                     ; (every? Predicate-function coll)
                                                            ; Predicate-function returns the result of logical check.
  ; To define and use a predicate function
  (defn at-least-5? [x] (>= x 5))                           ; Functions which return a boolean value
                                                            ;   usually has '?'(question mark) at the end of its name.
  (every? at-least-5? [5 6 7])
  (every? (fn [x] (>= x 5)) [5 6 7])
  (every? #(>= % 5) [5 6 7])

  ; To check if every elements in a collection doesn't meet a condition
  (not-any? at-least-5? [1 2 3])

  ; To check if some elements in a collection meets a condition
  (some at-least-5? [1 2 5])                                ; It returns the first element, which meets the condition.
                                                            ; It returns 'nil' if every elements doesn't meet the condition.
  (some #{1 2} [3 4 5 1 2])                                 ; Set can be used as a predicate function.
  (some #{nil} [nil nil nil])                               ; Be careful in case
  (some #{false} [false false false])                       ;   you deal with a logically false value.
)

(comment
  ;; Flow control
  ;; #if #if-let #when #when-let #cond #case

  ; if
  (if false "it is true" "it is false")                     ; (if condition true-expr false-expr)
  (if nil "it is true" "it is false")

  ; if-let
  (let [is-greater (> 10 3)]
    (if is-greater
      "Yes, it is greater."
      "No, it is not greater."))
  (if-let [is-greater (> 10 3)]                             ; (if-let [symbol-of-boolean logical-expr] true-expr false-expr)
    "Yes, it is greater."
    "No, it is not greater.")

  ; when
  (when true "it is true")                                  ; (when condition true-expr)
  (when false "it is true")                                 ; It returns 'nil' if the condition is false.

  ; when-let
  (let [is-greater (> 10 3)]
    (when is-greater
      "Yes, it is greater."))
  (when-let [is-greater (> 10 3)]                           ; (when-let [symbol-of-boolean logical-expr] true-expr)
    "Yes, it is greater.")

  ; cond
  (let [color "green"]                                      ; (let [symbol symbol-value]
    (cond                                                   ;    (cond
      (= color "red") "Red like roses."                     ;      condition1 expr1
      (= color "green") "Green like evergreens."            ;      condition2 expr2
      (= color "blue") "Blue like ocean."                   ;      ...
      :else "I don't know this color."))                    ;      [true-value default-expr]))
                                                            ; The default clause optional, and you can use any
                                                            ;   true value instead of ':else'.
  ; case
  (let [color "green"]                                      ; (let [symbol symbol-value]
    (case color                                             ;    (case symbol
      "red" "Red like roses."                               ;      value1 expr1
      "green" "Green like evergreens."                      ;      value2 expr2
      "blue" "Blue like ocean"                              ;      ...
      "I don't know this color."))                          ;      default-expr))
                                                            ; 'case' is used when only one symbol is repeatedly checked
                                                            ;   and value of the symbol can be compared by '='.
                                                            ; If 'case' is used and there is no matching clause,
                                                            ;    an exception will occur.
)

(comment
  ; ETC
  ; #class

  ; To check class
  (class true)
)

(defn -main [& args]
  (println "Hello"))
