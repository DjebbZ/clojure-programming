; Evalute these forms

(quote x)
(symbol? (quote x))
(namespace 'filter)
(def x 1)
(name 'x)
'(+ 1 1)
(list? '(+ 1 1))
(identical? '(+ 1 1) ('+ '1 '1))
(= '(+ 1 1) ('+ '1 '1))
'@x
'`x

; do
(do
  (println "hi")
  (println "ha"))

; IIFE and let
((fn [x]
   (+ 2 x)) 8)

(let [x 8]
  (+ 2 x))

; def + fn = defn
(def func (fn func-self-reference
            ([x] (func-self-reference x 1))
            ([x y] (+ x y))))

(func 10)
(func 10 50)

(defn func
  ([x] (func x 1))
  ([x y] (+ x y)))

(func 10)
(func 10 50)

; destructuring fn args
(defn concat-rest
  [x & rest]
  (apply str (butlast rest)))

(concat-rest 0 1 2 3 4)

(defn make-user
  [& [id1 id2]]
  {:id1 (or id1
                (str (java.util.UUID/randomUUID)))
   :id2 (or (when (not-any? nil? [id1 id2]) (str id1 "-" id2))
            (str (java.util.UUID/randomUUID)))})

(make-user)
(make-user 1 2 3)

; reduce looks like apply to me...
(reduce + [0 10 20 30])
(reduce + 10 [20 30 40])
(apply + [10 20 30 40])

(.getTime (java.util.Date.))

; reduce vs. apply : who's faster ? (A: reduce, lt 10% faster)
(defn current-time []
  (.getTime (java.util.Date.)))

(do
  (def t1 (current-time))
  (dotimes [n 20000000]
    (reduce + [0 1 2 3 4]))
  (- (current-time) t1))

(do
  (def t1 (current-time))
  (dotimes [n 20000000]
    (apply + [0 1 2 3 4]))
  (- (current-time) t1))


; comp
(require '[clojure.string :as str])

(def camel->keyword (comp keyword
                          str/join
                          (partial interpose \-)
                          (partial map str/lower-case)
                          #(str/split % #"(?<=[a-z])(?=[A-Z])")))

(camel->keyword "JustDoIt")

(#(str/split % #"(?<=[a-z])(?=[A-Z])") "JustDoIt")

; non-greedy regexps : http://qntm.org/files/re/re.html
(re-find #"'.*'" "'a'='a'")
(re-find #"'.*?'" "'a'='a'")

; comp <=> threading macro ?
(defn camel->>keyword
  [s]
  (->> (str/split s #"(?<=[a-z])(?=[A-Z])")
       (map str/lower-case)
       (interpose \-)
       str/join
       keyword))

(camel->>keyword "JustDoIt")

(def keyz #{"KeyOne" "KeyTwo" "KeyThree"})
(def valz [1 2 3])
(zipmap (map camel->keyword keyz)
        valz)

(map camel->keyword keyz)

(def kvlist '(("KeyOne" 1) ("KeyTwo" 2) ("KeyThree" 3)))
(map #(hash-map (first %) (second %)) kvlist)

(reduce (fn [m [k v]]
          (assoc m k v))
        {}
        kvlist)

(let [[k v] '("key" "val")]
  (println k)
  (println v))

