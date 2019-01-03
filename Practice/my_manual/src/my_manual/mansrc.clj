(ns my-manual.mansrc)

(comment
  ; Collection
  ; #collection #list #vector #map #set

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
)



(defn -main [& args]
  (println "Hello"))
