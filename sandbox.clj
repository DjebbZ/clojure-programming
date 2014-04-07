(ns sandbox.clj
  (:import (java.util HashMap UUID Date Formatter ArrayList)
           (java.io StringWriter BufferedWriter File)))

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
            (str (UUID/randomUUID)))
   :id2 (or (when (not-any? nil? [id1 id2]) (str id1 "-" id2))
            (str (UUID/randomUUID)))})

(make-user)
(make-user 1 2 3)

; reduce looks like apply to me...
(reduce + [0 10 20 30])
(reduce + 10 [20 30 40])
(apply + [10 20 30 40])

(.getTime (Date.))

; reduce vs. apply : who's faster ? (A: reduce, ~10% faster)
(defn current-time []
  (.getTime (Date.)))

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

(time (dotimes [n 20000000]
        (reduce + [0 1 2 3 4])))

(time (dotimes [n 20000000]
        (apply + [0 1 2 3 4])))

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

kvlist

(reduce (fn [m [k v]]
          (assoc m k v))
        {}
        kvlist)

(into {} (map vec kvlist))

(let [[k v] '("key" "val")]
  (println k)
  (println v))

; primitive logging system
(defn print-logger
  [writer]
  #(binding [*out* writer]
    (println %)))

(def *out*-logger (print-logger *out*))
(*out*-logger "hello")

(def writer (StringWriter.))
(def retained-logger (print-logger writer))
(retained-logger "hello")
(str writer)

(require 'clojure.java.io)
(defn file-logger
  [file]
  #(with-open [f (clojure.java.io/writer file :append true)]
    ((print-logger f) %)))

; file located here :
; /Applications/LightTable/LightTable.app/Contents/Resources/app.nw/plugins/clojure/runner/resources/messages.log
(def log->file (file-logger "messages.log"))
(log->file "hello")

(use 'clojure.java.javadoc)
(javadoc BufferedWriter)

(System/getProperty "user.dir")
(.getAbsolutePath (File. "."))

(defn multi-logger
  [& logger-fns]
  #(doseq [f logger-fns]
    (f %)))

(def log (multi-logger *out*-logger log->file))
(log "hello again")

(javadoc Formatter)
(defn timestamped-logger
  [logger]
  #(logger (format "[%1$tY-%1$tm-%1$te %1$tH:%1$tM:%1$tS] %2$s" (Date.) %)))

(def log-timestamped (timestamped-logger log))
(log-timestamped "goodbye, now")

; side-effects: I/O
(require '[clojure.xml :as xml])

; doesn't work anymore since Twitter 1.1 API requires authentication
(defn twitter-followers
  [username]
  (->> (str "https://api.twitter.com/1.1/users/show.xml?screen_name=" username)
       xml/parse
       :content
       (filter (comp #{:followers_count} :tag))
       first
       :content
       first
       Integer/parseInt))
(twitter-followers "Dj3bbZ") ; HTTP 401

; memoize only referentially transparent functions
(repeatedly 10 (partial rand-int 10))
(repeatedly 10 (partial (memoize rand-int) 10))

; collections and data structures
(keys {Math/PI "~3.14"})

(def v [1 2 3])
(conj v 4)
(conj v 4 5)
(seq v)

(def m {:a 1 :b 2})
(conj m [:c 3])
(conj m :c 3) ; IllegalArgumentException, arg must be a vector
(seq m)

(def s #{1 2 3})
(conj s 4 5)
(conj s 3)
(seq s)

(def lst '(1 2 3))
(conj lst 0 -1)
(seq lst)

; seq also works on Strings, but not conj
(seq "ABC")

(into v [4 5])
(into m [[:c 3] [:d 4]])
(into #{1 2} [2 3 4 3 2 5 3])
(into [1] m)

; collection abstraction
(conj [1] 2)
(seq [1 2])
(count '(1 2 3 4))
(empty #{1 2})
(= #{1 2} #{2 1})

; empty
(defn swap-pairs
  [sequential]
  (into (empty sequential)
        (interleave
          (take-nth 2 (drop 1 sequential))
          (take-nth 2 sequential))))

(swap-pairs [1 2 3 4 5 6])
(swap-pairs {:a 1 :b 2 :c 3})

(map (fn [[k v]] {k (inc v)}) {:a 1 :b 2})

(defn map-map
  [f m]
  (into (empty m)
        (for [[k v] m]
          [k (f v)])))
(map-map inc (hash-map :a 1 :b 2 :c 3))
(map-map inc (sorted-map :a 1 :b 2 :c 3))

;count
(count [1 2 3])
(count {:a 1 :b 2})
(count #{1 2 3 4})
(count '(1 2 3))
(count "ABC") ; works on Strings too

; sequences
(seq "Clojure")
(seq {:a 1 :b 5})
(seq (ArrayList. (range 10)))
(ArrayList. (range 10))
(seq (into-array ["Clojure" "Programming"]))
(seq [])
(seq nil)

(map str "Clojure")
(set "Programming")

(first "Clojure")
(rest "Clojure")
(next "Clojure")
(rest [1 2 3 4])

; rest vs next
(rest [1])
(next [1])
(rest nil)
(next nil)

(defn rest-equals-next
  "true for any x when it's a sequence or nil"
  [x]
  (= (next x)
     (seq (rest x))))

; sequences are not iterators
(doseq [x (range 3)]
  (println x))

(let [r (range 3)
      rst (rest r)]
  (prn (map str rst))
  (prn (map #(+ 100 %) r))
  (prn (conj r -1) (conj rst 42)))

; sequences are not lists
(let [s (range 1e6)]
  (time (count s))) ; must realize all values in s before counting
(let [s (apply list (range 1e6))]
  (time (count s))) ; lists track their own length, so it's just a property access

; creating seqs
(cons 0 [1 2 3])
(cons 0 (cons 1 (cons 2 [3 4])))
(list* 0 1 2 [3 4])
(class (cons 0 [1]))

; lazy seqs
(defn random-ints
  [limit]
  (lazy-seq
    (println "Realizing random number")
    (cons (rand-int limit)
          (random-ints limit))))

(take 10 (random-ints 50))
(random-ints 3) ; hopefully limited by the REPL, infinite recursion otherwise

(def rands (take 10 (random-ints 50)))
(first rands)
(nth rands 3)
(count rands)

(repeatedly 10 (partial rand-int 50))
(repeatedly 10 (fn []
                 (println "random number")
                 (rand-int 50)))

(def nxt (next (random-ints 50))) ; 2 realizations
(def rst (rest (random-ints 50))) ; only 1 realization

(split-with even? (range 10))
(split-with neg? (range -5 5))

(let [[t d] (split-with #(< % 12) (range 1e8))]
  [(count d) (count t)])

(let [[t d] (split-with #(< % 12) (range 1e8))]
  [(count t) (count d)])

; the associative abstraction
(def mapp {:a 1, :b 2, :c 3})
(get mapp :b)
(mapp :b)
(:b mapp)
(get mapp :d)
(mapp :d)
(:d mapp)
(get mapp :d "not-found")

(assoc mapp :d 4)
(dissoc mapp :c)

(assoc mapp
  :d 4
  :e 5
  :f 6)
(dissoc mapp :a :b)

; vectors are assocations of values with their indices
(def vect [1 2 3])
(get vect 0)
(get vect 10)
(get vect 10 "not-found")
(vect 0)
(0 vect) ; nope
(assoc vect
  1 4
  0 -12
  3 \c) ; assoc existing indices replace the values

; assoc on a indice not 'present' throws an IndexOutOfBoundsException
(assoc vect
  1 4
  0 -12
  4 \c) ; 4 is not an valid indice of vect
(conj vect \c) ; conj is better for vectors, no need to know an indice

; sets' associative abstraction associates values with themselves
(get #{1 2 3} 2)
(get #{1 2 3} 4)
(get #{1 2 3} 4 "not-found")
(when (get #{1 2 3} 1)
  (println "it contains 1 !"))
(assoc #{1 2 3} 4 4) ; assoc doesn't work on sets

(contains? [1 2 3] 0)
(contains? [] 0)
(contains? {:a 1 :b 2} :b)
(contains? {:a 1 :b 2} :c)
(contains? {:a 1 :b 2} 42)
(contains? #{1 2 3} 1)
(contains? #{1 2 3} 0)
(contains? [1 2 3] 3)
(contains? [1 2 3] 2)

(get "Clojure" 3)
(contains? (HashMap.) "not-there")
(get (into-array [1 2 3]) 0)

; beware of nil values
(get {:a nil} :c)
(get {:a nil} :a) ; nil is a valid value
(find {:a nil} :a)
(find {:a nil} :c)

(if-let [e (find {:a 1 :b 2} :a)]
  (format "found %s => %s" (key e) (val e))
  "not-found")

(if-let [e (find {:a 1 :b 2} :c)]
  (format "found %s => %s" (key e) (val e))
  "not-found")


