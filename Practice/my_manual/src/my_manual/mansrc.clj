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
  ;; Currying
  ;; #partial #comp

  ; To create a function by using another function whose arguments are not fully given
  (defn target-func [name color]
    (if (= color :red)
      (str name " likes red.")
      (str name " doesn't like red but may other color.")))
  (partial target-func "0pt3ryx")                           ; It returns the new function.
  ((partial target-func "0pt3ryx") :red)

  ; To create a composite function
  (defn inner-func [color]
    (if (= color :red) "roses" "evergreens"))
  (defn outer-func [things]
    (str "You may like color like " things " , I guess"))
  (defn composite-func [color]
    ((comp outer-func inner-func) color))
)

(comment
  ;; Laziness, recursion and loop
  ;; #take #range #count #repeat #repeatedly #cycle #loop #recur

  ; To get an integer sequence
  ; (range)   *Warning code - can crash REPL*
  (range 10)
  (range 5 10)

  ; To take n elements from a lazy sequence
  (take 10 (range))

  ; To count the number of elements of a lazy sequence
  (count (take 10000 (range)))

  ; To repeat an element
  (repeat 3 "green")
  (count (take 10000 (repeat "green")))

  ; To create a random int sequence
  (repeat 5 (rand-int 10))                                  ; This fails to create a random int sequence.
  (repeatedly 5 #(rand-int 10))                             ; This succeeds to create one.
                                                            ;   One of arguments of 'repeatedly' is a function which has no argument.
  (take 10 (repeatedly #(rand-int 10)))

  ; To repeat elements in cycle
  (take 7 (cycle ["red" "blue" "green"]))

  ; Recursive call
  (defn rainbow-has [in out]
    (if (empty? in)
      out
      (rainbow-has (rest in) (conj out (str "Rainbow has " (first in))))))
  (rainbow-has ["red" "green" "blue" "yellow" "violet"] [])

  ; Loop
  (defn rainbow-has [input]
    (loop [in input
           out []]
      (if (empty? in)
        out
        (recur (rest in) (conj out (str "Rainbow has " (first in)))))))
  (rainbow-has ["red" "green" "blue" "yellow" "violet"])
)


(comment
  ;; Data conversion
  ;; #map #reduce #filter #remove #for

  ; To convert(generate) a collection into another collection
  (take 10 (map #(* % 2) (range)))                          ; (map function collection1 collection2 ...)

  ; Side effects
  (def color-print (map #(println %)
                        [:red :yellow :green :blue :violet])) ; Side effects are caused when it is called.
  (def color-print (doall (map #(println %)
                               [:red :yellow :green :blue :violet]))) ; Side effects are caused when it is defined.

  ; To generate a collection from two collections or more
  (defn concat-color-thing [color thing]
    (str color "-" thins))
  (map concat-color-thing                                  ; 'map' stops when it reaches end of the shortest collection.
       ["red" "blue" "orange" "green" "black"] (cycle ["cup" "table"]))

  ; To generate a collection or a value from another collection
  (reduce + [1 2 3 4 5])                                    ; 'reduce' can't deal with an infinite sequence.
  (reduce (fn [r x] (+ r (* x x))) [1 2 3])                 ; r is the accumulation and x is the operand.
  (reduce (fn [r x] (if (nil? x) r (conj r x))) [] [:red nil :green :blue nil nil :violet])

  ; To filter
  (filter (complement nil?) [:red nil :green nil nil])
  (filter keyword? [:red nil :green nil nil])
  (remove nil? [:red nil :green nil nil])

  ; To traverse a collection and to deal with each element
  (for [color [:red :green :blue]]
    (str (name color)))
  (for [color [:red :green :blue]
        thing [:cup :table]]                                ; If there are two collections or more, the collections
    (str (name color) "-" (name thing)))                    ;   are combined.

  ; To use modifiers in 'for'
  (for [color [:red :green :blue]                           ; Modifier - :let
        thing [:cup :table]
        :let [color-str (str "color-" (name color))
              thing-str (str "thing-" (name thing))
              display-str (str color-str "-" thing-str)]]
    display-str)

  (for [color [:red :green :blue]                           ; Modifier - :when
        thing [:cup :table]
        :let [color-str (str "color-" (name color))
              thing-str (str "thing-" (name thing))
              display-str (str color-str "-" thing-str)]
        :when (= thing :table)]
    display-str)

  ; To unnest a nested collection
  (flatten [[:yellow [:green] [[:blue]]]])

  ; To convert data structure
  (vec '(1 2 3))
  (into [1] '(2 3))
  (sorted-map :e 1 :a 2 :b 4 :c 3)
  (into (sorted-map) {:e 1 :a 2 :b 4 :c 3})
  (into {} [[:a 1] [:b 2] [:c 3]])
  (into [] {:a 1, :b 2, :c 3})
  (partition 3 [1 2 3 4 5 6 7 8 9])
  (partition 3 [1 2 3 4 5 6 7 8 9 10])                      ; The remainders are thrown.
  (partition-all 3 [1 2 3 4 5 6 7 8 9 10])                  ; The remainders are not thrown.
  (partition-by #(= 6 %) [1 2 3 4 5 6 7 8 9 10])
)

(comment
  ;; State and Concurrency
  ;; #atom #reset #swap #side-effects #ref #alter #dosync #transaction #commute #agent #send #send-off #error
  ;; #exception #restart-agent #set-error-mode! #set-error-handler

  ; To store state (atom)
  (def who-atom (atom :caterpillar))
  who-atom
  @who-atom                                                 ; To get value of an atom

  ; To modify one state synchronously
  (reset! who-atom :chrysalis)                              ; Function whose name has '!' at the end modifies state.
  @who-atom

  (def who-atom (atom :caterpillar))
  (defn change [state]
    (case state
      :caterpillar :chrysalis
      :chrysalis :butterfly
      :butterfly))
  (swap! who-atom change)                                   ; Keep in mind that when you use 'swap!', the passed
  @who-atom                                                 ;   function should not have side effects.
  (swap! who-atom change)
  @who-atom

  ; When you use function with no side effects
  (def counter (atom 0))
  @counter
  (let [n 5]
    (future (dotimes [_ n] (swap! counter inc)))
    (future (dotimes [_ n] (swap! counter inc)))
    (future (dotimes [_ n] (swap! counter inc))))
  @counter

  ; When you use function with side effects
  (def counter (atom 0))
  @counter
  (defn inc-print [val]
    (println val)
    (inc val))
  (let [n 2]
    (future (dotimes [_ n] (swap! counter inc-print)))
    (future (dotimes [_ n] (swap! counter inc-print)))
    (future (dotimes [_ n] (swap! counter inc-print))))
  @counter

  ; To modify two or more states in the coordinated way (transaction)
  ;   Behavior of 'ref' in a transaction
  ;     1. Atomic - Every modification in a transaction is invoked for every 'ref's. If there is an error,
  ;                any 'ref's are not modified.
  ;     2. Consistent - Verifier (function) can be optionally used before finish a transaction.
  ;     3. Isolated - A transaction can't know what happen to other transactions.
  (def balance (ref 1000))
  (def money-in-hand (ref 1200))
  (defn deposit-100-won []                                  ; Function which contains 'alter' should
    (when (pos? @money-in-hand)                             ;   have no side effects.
      (alter money-in-hand #(- % 100))
      (alter balance #(+ % 100))))
  (dosync (deposit-100-won))                                ; If 'dosync' is not used, it occurs error.
                                                            ;   'dosync' mediates all modifications in a transaction.

  (def balance (ref 1000))
  (def money-in-hand (ref 1200))
  (defn deposit-100-won []
    (dosync (when (pos? @money-in-hand)
              (alter money-in-hand #(- % 100))
              (alter balance #(+ % 100)))))
  (let [n 5]
    (future (dotimes [_ n] (deposit-100-won)))
    (future (dotimes [_ n] (deposit-100-won)))
    (future (dotimes [_ n] (deposit-100-won))))
  @balance
  @money-in-hand

  ;   'commute' doesn't retry to modify state in a transaction unlike 'ref'.
  (def balance (ref 1000))
  (def money-in-hand (ref 1200))
  (defn deposit-100-won []
    (dosync (when (pos? @money-in-hand)
              (commute money-in-hand #(- % 100))            ; This could be a dangerous example.
              (commute balance #(+ % 100)))))               ; Function which contains 'commute' should be commutative.
  (let [n 5]
    (future (dotimes [_ n] (deposit-100-won)))
    (future (dotimes [_ n] (deposit-100-won)))
    (future (dotimes [_ n] (deposit-100-won))))
  @balance
  @money-in-hand

  ; Deal with dependency
  (def x (ref 1))
  (def y (ref 1))                                           ; y = x+2
  (defn sync-values []
    (dosync
      (alter x inc)
      (ref-set y (+ 2 @x))))                                ; 'ref-set' is useful when one ref-value has
  (let [n 2]                                                ;    dependency on another ref-value.
    (future (dotimes [_ n] (sync-values)))
    (future (dotimes [_ n] (sync-values))))
  @x
  @y

  ; Asynchronous modification (to use agent)
  ; To store state (agent)
  (def who-agent (agent :caterpillar))
  @who-agent
  (defn change [state]
    (case state
      :caterpillar :chrysalis
      :chrysalis :butterfly
      :butterfly))
  (send who-agent change)                                   ; 'send' doesn't waiting for the given process.
  @who-agent

  (send-off who-agent change)                               ; 'send-off' is used when it is need to wait for I/O process.
  @who-agent

  ; Error or exception in agent
  (def who-agent (agent :caterpillar))
  (defn change [state]
    (case state
      :caterpillar :chrysalis
      :chrysalis :butterfly
      :butterfly))
  (defn change-error [state] (throw (Exception. "Boom!")))
  (send who-agent change-error)
  @who-agent
  (send-off who-agent change)                               ; You can't change the status when there is an error.
  (agent-error who-agent)                                   ; To check error
  (restart-agent who-agent :caterpillar)                    ; To remove error and reinitialize the agent
  (send-off who-agent change)                               ; You can change the status after 'restart-agent'.

  ; Error or exception handling in agent
  (def who-agent (agent :caterpillar))
  (set-error-mode! who-agent :continue)                     ; To switch error mode
  (defn err-handler-fn [a ex] (println "error " ex " value is " @a))
  (set-error-handler! who-agent err-handler-fn)             ; To set error handler
  (send who-agent change-error)
  (send who-agent change)                                   ; You don't need to restart the agent.
  @who-agent
)

(comment
  ;; Java and Polymorphism
  ;; #Java #import #doto #defmulti #defmethod #defprotocol #defrecord #deftype

  ; To use String methods
  (. "caterpillar" toUpperCase)                             ; String st = new String("caterpillar");
                                                            ; st.toUpperCase();

  (.indexOf "caterpillar" "pillar")                         ; String st1 = new String("caterpillar");
                                                            ; String st2 = new String("pillar");
                                                            ; st1.indexOf(st2);

  ; Create a class instance
  (new String "Hello World!")
  (String. "Hello World!")                                  ; Short version

  ; To import a library
  (ns opteryx.network (:import (java.net InetAddress)))     ; import java.net.InetAddress;
  (import 'java.net.InetAddress)

  ; To call a static method.
  (.getHostName (InetAddress/getByName "localhost"))        ; Use '/'.

  ; To call a static method without import
  (java.net.InetAddress/getByName "localhost")

  ; To call instance methods successively
  (def sb (doto (StringBuffer. "Who ")
            (.append "are ")
            (.append "you?")))
  (.toString sb)

  ; Multimethod (polymorphism for one function)
  (defn what-is-it [input]                                  ; Before using multimethod
    (cond
      (= java.lang.String (class input)) "This is a String."
      (= clojure.lang.Keyword (class input)) "This is a Keyword."
      (= java.lang.Long (class input)) "This is a number."))
  (what-is-it 12)

  (defmulti what-is-it class)                               ; Using multimethod
  (defmethod what-is-it java.lang.String [input]
    (str "This is a String. " input))
  (defmethod what-is-it clojure.lang.Keyword [input]
    (str "This is a Keyword. " input))
  (defmethod what-is-it java.lang.Long [input]
    (str "This is a number. " input))
  (defmethod what-is-it :default [input]                    ; Default method
    (str "I don't what it is. " input))
  (what-is-it 12)

  (defmulti is-negative (fn [input]
                          (if (< input 0)
                            :negative
                            :not-negative)))
  (defmethod is-negative :negative [_]
    "The number is negative.")
  (defmethod is-negative :not-negative [_]
    "The number is 0 or positive.")

  ; Protocol (polymorphism for 2 or more functions)
  (defprotocol Test-protocol
    (what-is-it [this]))

  (extend-protocol Test-protocol
    java.lang.String
    (what-is-it [this]
      (str (.toUpperCase this) " is a String."))

    clojure.lang.Keyword
    (what-is-it [this]
      (case this
        :negative "This is negative."
        :not-negative "This is not negative."))

    java.lang.Long
    (what-is-it [this]
      (if (< this 0)
        "The number is negative."
        "The number is 0 or positive.")))
  (what-is-it "Hello")
  (what-is-it :negative)
  (what-is-it -3)

  ; To define a new data type (defrecord)
  (defrecord Cup [color price])                             ; 'defrecord'
  (def my-cup (Cup. "green" 12000))                         ; To create an instance
  (class my-cup)
  (.color my-cup)                                           ; To get a field of an instance
  (.-price my-cup)                                          ; '.-' is usually used instead of '.'.

  (defprotocol Description-cup                              ; 'defprotocol' + 'defrecord'
    (describe-color [this])
    (describe-price [this]))
  (defrecord StandardCup [color price]
    Description-cup
    (describe-color [this]
      (str "This standard cup has " color " color."))
    (describe-price [this]
      (str "Its price is " price " and this is quite cheap.")))
  (defrecord DurableCup [color price]
    Description-cup
    (describe-color [this]
      (str "This durable cup has " color " color and so durable."))
    (describe-price [this]
      (str "Its price is " price " and this is more expensive.")))
  (def my-cup (StandardCup. "green" 11000))
  (def your-cup (DurableCup. "orange" 80500))
  (describe-color my-cup)
  (describe-price my-cup)
  (describe-color your-cup)
  (describe-price your-cup)

  ; To define a new data type (deftype)
  (deftype Cup [color price])                               ; 'deftype'
  (def my-cup (Cup. "green" 12000))                         ; To create an instance
  (class my-cup)
  (.color my-cup)                                           ; To get a field of an instance
  (.-price my-cup)                                          ; '.-' is usually used instead of '.'.

  ;; There are some differences between 'defrecord' and 'deftype'.
  ; https://lispcast.com/deftype-vs-defrecord/
  ; https://clojure.org/reference/datatypes
)

(comment
  ;; ETC
  ;; #class #destructuring #as #data-type #rand-int #complement #keyword? #name

  ; To check class
  (class true)

  ; To get a random int
  (rand-int 10)

  ; To destructure (vector)
  (let [[name color] ["0pt3ryx" "red"]]
    (str name " likes " color))

  ; To destructure (map)
  (let [{name :name color :color}
        {:name "0pt3ryx" :color "green"}]
    (str name " sees a " color " cube."))

  ; To destructure (map - simplified)
  (let [{:keys [name color]}
        {:name "0pt3ryx" :color "green"}]
    (str name " sees a " color " cube."))

  ; To destructure (map - in function)
  (defn flower-colors [colors]
    (str "The flowers are "
         (:flower1 colors)
         " and "
         (:flower2 colors)))
  (flower-colors {:flower1 "red" :flower2 "blue"})          ; Before destructuring

  (defn flower-colors [{:keys [flower1 flower2]}]
    (str "The flowers are " flower1 " and " flower2))
  (flower-colors {:flower1 "red" :flower2 "blue"})          ; After destructuring

  ; To get data type
  (let [[name [color] :as data-type] ["0pt3ryx" ["green"]]]
    {:color color :name name :data-type data-type})

  ; To create a function whose result is complement of another predicate-function
  ((complement nil?) nil)                                   ; (complement predicate-function)

  ; To check if a given value is a keyword or not
  (keyword? :red)

  ; To convert a keyword into a string
  (name :red)
)

(defn -main [& args]
  (println "Hello"))

; 1 1 2 3 5 8 13 21 34 55 89
(defn fibo [n]
  (loop [count n a1 1 a2 1]
    (if (<= count 1)
      a2
      (recur (dec count) a2 (+ a1 a2)))))
