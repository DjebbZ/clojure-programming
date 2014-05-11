(ns sandbox.clj
  (:import (java.util HashMap UUID Date Formatter ArrayList)
           (java.io StringWriter BufferedWriter File)
           (javax.swing JFrame JPanel)
           (java.awt Graphics Graphics2D Dimension BasicStroke)
           (java.net URL))
  (:require [clojure.zip :as z]))

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

;{nil 1 nil 2}

(nth [:a :b :c] 2)
(get [:a :b :c] 2)
(nth [:a :b :c] 3) ; IndexOutOfBoundsException
(get [:a :b :c] 3) ; nil

(get 42 0)
(nth 42 0) ; UnsupportedOperationException

; stack abstraction
(conj '() 1)
(conj '(2 1) 3)
(peek '(3 2 1))
(pop '(3 2 1))
(pop '(2 1))
(pop '(1))
(pop '()) ; IllegalStateException

(conj [] 1)
(conj [1 2] 3)
(peek [1 2 3])
(pop [1 2 3])
(pop [1])
(pop []) ; IllegalStateException

; set
(get #{1 2 3} 1)
(get #{1 2 3} 4)
(get #{1 2 3} 4 "not-found")
(disj #{1 2 3} 1 3)

; sorted abstraction
(def sm (sorted-map :z 5 :x 4 :y 3 :b 0 :a 1 :c 9))
sm
(subseq sm <= :c)
(subseq sm > :b <= :y)
(rsubseq sm > :b <= :y)
(subseq sm > :b <= :y)

(compare 2 2)
(compare 2 3)
(compare 3 2)
(compare "cv" "abc")
(compare 1e8 nil)
(compare nil 1e8)
(compare ["z" 2] ["b" 2])

(sort < (repeatedly 10 #(rand-int 100)))
(sort-by first > (map-indexed vector "Clojure"))

((comparator <) 1 4)
((comparator >) 1 4)
((comparator >) 4 4)

(sorted-map-by compare :z 5 :x 4 :y 3 :b 0 :a 1 :c 9 :z 111)
(apply sorted-map-by compare [:z 5 :x 4 :y 3 :b 0 :a 1 :c 9 :z 111])
(sorted-map-by (comp - compare) :z 5 :x 4 :y 3 :b 0 :a 1 :c 9 :z 111)

(time (sorted-map-by (comp - compare) :z 5 :x 4 :y 3 :b 0 :a 1 :c 9 :z 111))
(time (into {} (->
                 (sorted-map-by compare :z 5 :x 4 :y 3 :b 0 :a 1 :c 9 :z 111)
                 reverse)))

(defn magnitude
  [x]
  (-> x Math/log10 Math/floor))

(defn compare-magnitude
  [a b]
  (println "a" (magnitude a) "b" (magnitude b))
  (- (magnitude a) (magnitude b)))

((comparator compare-magnitude) 10 10000)
((comparator compare-magnitude) 100 10)
((comparator compare-magnitude) 10 75)

; collections and keys and HOFs
(map :name [{:age 21 :name "David"}
            {:gender :f :name "Sarah"}
            {:name "Suzy" :location "NYC"}])

(some #{1 2 3} [0 4 5 6 7])
(some #{1 2 3} [0 1 4 5 6 7])

(filter :age [{:age 21 :name "David"}
              {:gender :f :name "Sarah"}
              {:name "Suzy" :location "NYC"}])

; beware, it doesn't mean what it reads "naturally"
; (partial <= 25) returns true when the final param is >= 25
(filter (comp (partial <= 25) :age) [{:age 21 :name "David"}
                                     {:gender :f :name "Sarah" :age 25}
                                     {:name "Suzy" :location "NYC" :age 34}])

(filter (partial <= 25) [1 2 3 44])
(<= 1 2)

(remove (comp (partial <= 25) :age) [{:age 21 :name "David"}
                                     {:gender :f :name "Sarah" :age 25}
                                     {:name "Suzy" :location "NYC" :age 34}])

(remove #{5 7} (cons false (range 10)))
(remove #{5 7 false} (cons false (range 10)))
(#{1 2 false} false)
(remove (partial contains? #{5 7 false}) (cons false (range 10)))
(contains? #{5 7 false} false)

; lists
()
'(1 2 3)
(1 2 3) ; ClassCastException
'(1 2 (+ 1 2)) ; subexpressions not evaluated
'(1 2 [1 2 3])
(list 1 2 (+ 1 2))
(list? 1)
(list? '(1 2 3))
(list? (seq [1 2 3])) ; false

; vectors
(vector 1 2 3)
(vec '(1 2 3))
(vector? [1 2])

; vectors as tuples
(defn euclidian-division
  [x y]
  [(quot x y) (rem x y)])

(euclidian-division 42 8)
((juxt quot rem) 42 8) ; juxt, OMG

(let [[q r] (euclidian-division 53 7)]
  (str "53/7 = " q " * 7 + " r))

(def point-3d [42 26 -7])
(def travel-legs [["LYS" "FRA"] ["FRA" "PHL"] ["PHL" "RDU"]])

; sets
#{1 2}
; #{1 2 2} IllegalArgumentException Duplicate key 2

(hash-set 1 2 3 4)
(set [1 2 3 3 4 8 8 8])

(apply str (remove (set "aeiouy") "vowels are useless"))
(defn numeric? [s] (every? (set "0123456789") s))
(numeric? "123")
(numeric? "12b")

; maps
{:a 1 :b 2}
;; {:a 1 :a 2}
(hash-map :a 1 :b 2)
(apply hash-map [:a 1 :b 2])

(keys m)
(vals m)
(map key m)
(map val m)

; maps as ad-hoc structs
(def playlist
  [{:title "Elephant", :artist "The White Stripes", :year 2003}
   {:title "Helioself", :artist "Papas Fritas", :year 1997}
   {:title "Stories from the City, Stories from the Sea", :artist "PJ Harvey", :year 2000}
   {:title "Buildings and Grounds", :artist "Papas Fritas", :year 2000}
   {:title "Zen Rodeo", :artist "Mardi Gras BB", :year 2002}])

(map :title playlist)
(defn summarize
  [{:keys [title artist year]}]
  (str title " / " artist " / " year))
(map summarize playlist)

; other usages of maps
(group-by #(rem % 3) (range 10))
(group-by :artist playlist)
(group-by (juxt :artist :year) playlist)

(into {} (for [[k v] (group-by :artist playlist)]
           [k (map summarize v)]))


(defn reduce-by
  [key-fn f init coll]
  (reduce (fn [summaries x]
            (let [k (key-fn x)]
              (assoc summaries k (f (summaries k init) x))))
          {} coll))

(def orders
  [{:product "Clock", :customer "Wile Coyote", :qty 6, :total 300}
   {:product "Dynamite", :customer "Wile Coyote", :qty 20, :total 5000}
   {:product "Shotgun", :customer "Elmer Fudd", :qty 2, :total 800}
   {:product "Shells", :customer "Elmer Fudd", :qty 4, :total 100}
   {:product "Hole", :customer "Wile Coyote", :qty 1, :total 1000}
   {:product "Anvil", :customer "Elmer Fudd", :qty 2, :total 300}
   {:product "Anvil", :customer "Elmer Fudd", :qty 2, :total 300}
   {:product "Anvil", :customer "Wile Coyote", :qty 6, :total 900}])

(reduce-by :product #(+ %1 (:total %2)) 0 orders)
(reduce-by :product #(conj %1 (:customer %2)) #{} orders)

(reduce-by (juxt :customer :product) #(+ %1 (:total %2)) 0 orders)

(defn reduce-by-in
  [keys-fn f init coll]
  (reduce (fn [summaries x]
            (let [ks (keys-fn x)]
              (assoc-in summaries ks (f (get-in summaries ks init) x))))
          {} coll))

(reduce-by-in (juxt :customer :product) #(+ %1 (:total %2)) 0 orders)
(map (juxt :customer :product) orders)

(def flat-breakup
  (reduce-by (juxt :customer :product) #(+ %1 (:total %2)) 0 orders))

(reduce #(apply assoc-in %1 %2) {} flat-breakup)
(assoc-in {} ["Anvil" "Wile Coyote"] 900)

(->> orders
     (reduce-by (juxt :customer :product) #(+ %1 (:total %2)) 0)
     (reduce #(apply assoc-in %1 %2) {}))

; transients

(def x (transient []))
(def y (conj! x 1))
(count y)
(count x)

(into #{} (range 5))

(defn naive-into
  [coll source]
  (reduce conj coll source))

(= (into #{} (range 500))
   (naive-into #{} (range 500)))

(time (do (into #{} (range 1e6))
          nil)) ; the nil only prevents the REPL to prints millions of numbers
(time (do (naive-into #{} (range 1e6))
          nil))

(defn faster-into
  [coll source]
  (persistent! (reduce conj! (transient coll) source)))

(time (do (faster-into #{} (range 1e6))
          nil))

(defn transient-capable?
  "Returns true if a transient can be obtained for the given collection.
  i.e. tests if `(transient coll)` will succeed."
  [coll]
  (instance? clojure.lang.IEditableCollection coll))

(def v [1 2])
(def tv (transient v))
(conj v 3)
(count tv)
(persistent! tv)
(get tv 0)

(nth (transient [1 2]) 1)
(get (transient {:a 1 :b 2}) :a)
((transient {:a 1 :b 2}) :a) ; transient are functions too
(:a (transient {:a 1 :b 2}))
(find (transient {:a 1 :b 2}) :a) ; ClassCastException : not all functions API for transients

(let [tm (transient {})]
  (doseq [x (range 100)]
    (assoc! tm x 0))
  (persistent! tm)) ; {0 0, 1 0, 2 0, 3 0, 4 0, 5 0, 6 0, 7 0}
; almost all of the values are lost, must the use the result of assoc! instead

(let [t (transient {})]
  @(future (get t :a))) ; IllegalAccessError, transient are mono-thread

; transients don't compose
(persistent! (transient [])) ; []
(persistent! (transient [(transient [])])) ; TransientVector

(= (transient [1 2]) (transient [1 2])) ; false

; metadata


(def a ^{:created (System/currentTimeMillis)}
   [1 2 3])
(meta a)

(meta ^:private ^:dynamic [1 2 3])

(def b (with-meta a (assoc (meta a)
               :modified (System/currentTimeMillis))))
(meta b)

(def b (vary-meta a assoc :modified (System/currentTimeMillis)))
(meta b)

(= a b)
(= ^{:a 5} 'any-value
   ^{:b 5} 'any-value)

(meta (conj a 500))

; game of life
(defn empty-board
  "Creates a rectangular empty board of the specified width and height"
  [w h]
  (vec (repeat w (vec (repeat h nil)))))

(defn populate
  "Turns :on each of the cells specified as [y, x] coordinates"
  [board living-cells]
  (reduce (fn [board coordinates]
            (assoc-in board coordinates :on))
          board
          living-cells))

(def glider (populate (empty-board 6 6) #{[2 0] [2 1] [2 2] [1 2] [0 1]}))

(pprint glider)

(defn neighbours
  [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= 0 dx dy)]
    [(+ dx x) (+ dy y)]))

(defn count-neighbours
  [board loc]
  (count (filter #(get-in board %) (neighbours loc))))

(defn indexed-step
  "Yields the next state of the board, using indices to determine neighbours,
  liveness, etc."
  [board]
  (let [w (count board)
        h (count (first board))]
    (loop [new-board board x 0 y 0]
      (cond
        (>= x w) new-board
        (>= y h) (recur new-board (inc x) 0)
        :else
        (let [new-liveness
              (case (count-neighbours board [x y])
                2 (get-in board [x y])
                3 :on
                nil)]
          (recur (assoc-in new-board [x y] new-liveness) x (inc y)))))))

(-> (iterate indexed-step glider) (nth 8) pprint)

(defn indexed-step2
  "More functional implementation of indexed-step"
  [board]
  (let [w (count board)
        h (count (first board))]
    (reduce
      (fn [new-board x]
        (reduce
          (fn [new-board y]
            (let [new-liveness
                  (case (count-neighbours board [x y])
                    2 (get-in board [x y])
                    3 :on
                    nil)]
              (assoc-in new-board [x y] new-liveness)))
          new-board (range h)))
      board (range w))))

(defn indexed-step3
  "indexed-step2 with nested reductions collapsed"
  [board]
  (let [w (count board)
        h (count (first board))]
    (reduce
      (fn [new-board [x y]]
        (let [new-liveness
              (case (count-neighbours board [x y])
                2 (get-in board [x y])
                3 :on
                nil)]
          (assoc-in new-board [x y] new-liveness)))
      board (for [x (range h) y (range w)] [x y]))))

(partition 3 1 (range 5))

(partition 3 1 (concat [nil] (range 5) [nil]))

(defn window
  "Returns a lazy sequence of 3-item windows centered around each item of coll,
  padded as necessary with pad or nil"
  ([coll] (window nil coll))
  ([pad coll]
   (partition 3 1 (concat [pad] coll [pad]))))

(defn cell-block
  "Creates a sequences of 3x3 windows from a triple of 3 sequences."
  [[left mid right]]
  (window (map vector left mid right)))

(defn liveness
  "Returns the liveness (nil or :on) of the center cell for the next step."
  [block]
  (let [[_ [_ center _] _] block]
    (case (- (count (filter #{:on} (apply concat block)))
             (if (= :on center) 1 0))
      2 center
      3 :on
      nil)))

(defn- step-row
  "Yields the next state of the center row."
  [rows-triple]
  (vec (map liveness (cell-block rows-triple))))

(defn index-free-step
  "Yieds the next state of the board"
  [board]
  (vec (map step-row (window (repeat nil) board))))

(= (nth (iterate indexed-step glider) 8)
   (nth (iterate index-free-step glider) 8))

; getting to the next level
(defn step
  "Yields the next state of the world"
  [cells]
  (set (for [[loc n] (frequencies (mapcat neighbours cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))

(->> (iterate step #{[2 0] [2 1] [2 2] [1 2] [0 1]})
     (drop 8)
     first
     (populate (empty-board 6 6))
     pprint)

(defn stepper
  "Returns a step function for Life-like cell automata.
  neightbours takes a location and return a sequential collection of location.
  survive? and birth? are predicates on the number of living neighbours."
  [neighbours birth? survive?]
  (fn [cells]
    (set (for [[loc n] (frequencies (mapcat neighbours cells))
               :when (if (cells loc) (survive? n) (birth? n))]
           loc))))

(def step2 (stepper neighbours #{3} #{2 3}))

(def living-cells #{[0 1][1 2][3 0][3 1][3 2]})
(step2 living-cells)

(= (nth (iterate step living-cells) 8)
   (nth (iterate step2 living-cells) 8))

; Life-like automaton H.B2/S34
; hexagonal grid, birth of 2, survive when 3 or 4
(defn hex-neighbours
  [[x y]]
  (for [dx [-1 0 1] dy (if (zero? dx) [-2 2] [-1 1])]
    [(+ dx x) (+ dy y)]))
(def hex-step (stepper hex-neighbours #{2} #{3 4}))

; this configuration is an oscillator of period 4
(hex-step #{[0 0] [1 1] [1 3] [0 4]})
(hex-step *1)
(hex-step *1)
(hex-step *1)

; maze generation
(defn maze
  "Returns a random maze carved out of walls;
  walls is a set of 2-item sets #{a b} where a and b are locations.
  The returned maze is a set of the remaining walls."
  [walls]
  (let [paths (reduce (fn [index [a b]]
                        (merge-with into index {a [b] b [a]}))
                      {} (map seq walls))
        start-loc (rand-nth (keys paths))]
    (loop [walls walls
           unvisited (disj (set (keys paths)) start-loc)]
      (if-let [loc (when-let [s (seq unvisited)] (rand-nth s))]
        (let [walk (iterate (comp rand-nth paths) loc)
              steps (zipmap (take-while unvisited walk) (next walk))]
          (recur (reduce disj walls (map set steps))
                 (reduce disj unvisited (keys steps))))
        walls))))

(defn grid
  [w h]
  (set (concat
         (for [i (range (dec w)) j (range h)] #{[i j] [(inc i) j]})
         (for [i (range w) j (range (dec h))] #{[i j] [i (inc j)]}))))

(defn draw
  [w h maze]
  (doto (JFrame "Maze")
    (.setContentPane
      (doto (proxy [JPanel] []
              (paintComponent [^Graphics g]
                (let [g (doto ^Graphics2D (.create g)
                          (.scale 10 10)
                          (.translate 1.5 1.5)
                          (.setStroke (BasicStroke. 0.4)))]
                  (.drawRect g -1 -1 w h)
                  (doseq [[[xa ya] [xb yb]] (map sort maze)]
                    (let [[xc yc] (if (= xa xb)
                                    [(dec xa) ya]
                                    [xa (dec ya)])]
                      (.drawLine g xa ya xc yc))))))
        (.setPreferredSize (Dimension
                             (* 10 (inc w)) (* 10 (inc h))))))
    .pack
    (.setVisible true)))

(draw 40 40 (maze (grid 40 40)))

; zippers

(require '[clojure.zip :as z])

(def v [[1 2 [3 4]] [5 6]])
(-> v z/vector-zip z/node)
(-> v z/vector-zip z/down z/node)
(-> v z/vector-zip z/down z/right z/node)

(-> v z/vector-zip z/down z/right (z/replace 56) z/node)
(-> v z/vector-zip z/down z/right (z/replace 56) z/root)
(-> v z/vector-zip z/down z/right z/remove z/node)
(-> v z/vector-zip z/down z/right z/remove z/root)
(-> v z/vector-zip z/down z/down z/right (z/edit * 42) z/root)

(def w [1 [2 5] [3 4]])
(-> w z/vector-zip z/down z/right z/right z/remove z/node)
(-> w z/vector-zip z/down z/right z/children)
(-> w z/vector-zip z/down z/rights)

; custom zippers
(defn html-zip [root]
  (z/zipper
    vector?
    (fn [[tagname & xs]]
      (if (map? (first xs)) (next xs) xs))
    (fn [[tagname & xs] children]
      (into (if (map? (first xs)) [tagname (first xs)] [tagname])
            children))
    root))

(defn wrap
  "Wraps the current node in the specified tag and attributes"
  ([loc tag]
   (z/edit loc #(vector tag %)))
  ([loc tag attrs]
   (z/edit loc #(vector tag attrs %))))

(def h [:body [:h1 "Clojure"]
              [:p "What a wonderful language!"]])

(-> h html-zip z/down z/right z/down (wrap :b) z/root)

; Delays

(def d (delay (println "Running...")
              :done!))

(deref d)
@d

(defn gh-user
  [name]
  (let [url (str "https://api.github.com/users/" name)]
    (delay (slurp url))))

(def d (gh-user "DjebbZ"))
d

(realized? d)
@d
(realized? d)

(def long-calculation (future (apply + (range 1e8))))
@long-calculation
@(future (Thread/sleep 5000) :done!)

(deref (future (Thread/sleep 5000) :done!)
       1000
       :impatient!)

(defn get-document
  []
  {:url "http://www.mozilla.org/about/manifesto.en.html"
   :title "The Mozilla manifesto"
   :content (future (slurp "http://www.mozilla.org/about/manifesto.en.html"))})

(get-document)
(:content (get-document))
(deref (:content (get-document)))

; promises
(def p (promise))
(realized? p)
(deliver p 42)
(realized? p)

(def a (promise))
(def b (promise))
(def c (promise))

(future
  (deliver c (+ @a @b))
  (println "Delivery complete!"))

(deliver a 15)
(deliver b 15)
@c

(defn call-service
  [arg1 arg2 callback-fn]
  ;.. do stuff
  (future (callback-fn (+ arg1 arg2) (- arg1 arg2))))

(defn sync-fn
  [async-fn]
  (fn [& args]
    (let [result (promise)]
      (apply async-fn (conj (vec args) #(deliver result %&)))
      @result)))

((sync-fn call-service) 8 7)

; parallelism on the cheap
(defn phone-numbers
  [string]
  (re-seq #"(\d{3})[\.-]?(\d{3})[\.-]?(\d{4})" string))
(phone-numbers "Snumil: 617-555-3909")

(def files (repeat 100
                   (apply str
                          (concat (repeat 1000000 \space)
                                  "Sunil: 617.555.2397, Betty: 508.555.2218"))))
(time (dorun (map phone-numbers files)))
(time (dorun (pmap phone-numbers files)))

(def more-files (repeat 100000
                        (apply str
                               (concat (repeat 1000 \space)
                                       "Sunil: 617.555.2397, Betty: 508.555.2218"))))
(time (dorun (map phone-numbers more-files)))
(time (dorun (pmap phone-numbers more-files)))

(time (->> files
           (partition-all 250)
           (pmap (fn [chunk] (doall (map phone-numbers chunk))))
           (apply concat)
           dorun))

; state and identity
(defmacro futures
  [n & exprs]
  (vec (for [_ (range n)
             expr exprs]
         `(future ~expr))))

(defmacro wait-futures
  [& args]
  `(doseq [f# (futures ~@args)]
     @f#))

(def sarah (atom {:name "Sarah" :age 25 :wears-glasses? false}))
(swap! sarah update-in [:age] + 3)
(update-in @sarah [:age] + 3)
@sarah

(swap! sarah (comp #(update-in % [:age] inc)
                   #(assoc % :wears-glasses? true)))
@sarah

(def xs (atom #{1 2 3}))
(wait-futures 1 (swap! xs (fn [v]
                            (Thread/sleep 250)
                            (println "trying 4")
                            (conj v 4)))
                (swap! xs (fn [v]
                            (Thread/sleep 500)
                            (println "trying 5")
                            (conj v 5))))
@xs

(compare-and-set! xs :wrong "new value")
(compare-and-set! xs @xs "new value")

(reset! xs :y)
@xs

; notifications and constraints

; watches
(defn echo-watch
  [key identity old new]
  (println key old "=>" new))

(def sarah (atom {:name "Sarah" :age 25}))

(add-watch sarah :echo echo-watch)
(swap! sarah update-in [:age] inc)

(add-watch sarah :echo2 echo-watch)
(swap! sarah update-in [:age] inc)

(remove-watch sarah :echo2)
(swap! sarah update-in [:age] inc)

(reset! sarah @sarah)

(def history (atom ()))

(defn log->list
  [dest-atom key source old new]
  (when (not= old new)
    (swap! dest-atom conj new)))

(def sarah (atom {:name "Sarah" :age 25}))
(add-watch sarah :record (partial log->list history))

(swap! sarah update-in [:age] inc)
(swap! sarah update-in [:age] inc)
(swap! sarah assoc :wears-glasses? true)
(swap! sarah update-in [:age] inc)
(clojure.pprint/pprint @history)

;; validators

(def n (atom 1 :validator pos?))
(swap! n + 500)
;; (swap! n - 1000) ; IllegalStateException: Invalid reference state

(def sarah (atom {:name "Sarah" :age 25}))
(set-validator! sarah :age)
;; (swap! sarah dissoc :age) ; IllegalStateException: Invalid reference state

(set-validator! sarah #(or (:age %)
                           (throw (IllegalStateException. "People must have ':age's!"))))
(swap! sarah dissoc :age)
(:age @sarah)

;; refs
(defn character
  [name & {:as opts}]
  (ref (merge {:name name :items #{} :health 500}
              opts)))

(def smaug (character "Smaug" :health 500 :strength 400 :items (set (range 50))))
(def bilbo (character "Bilbo" :health 100 :strength 100))
(def gandalf (character "Gandalf" :health 75 :mana 750))


(defn loot
  [from to]
  (dosync
   (when-let [item (first (:items @from))]
     (alter to update-in [:items] conj item)
     (alter from update-in [:items] disj item))))

(wait-futures 1
              (while (loot smaug bilbo))
              (while (loot smaug gandalf)))

@smaug
@bilbo
@gandalf

(map (comp count :items deref) [bilbo gandalf])
(filter (:items @bilbo) (:items @gandalf))

(def x (ref 0))

(time (wait-futures 5
                    (dotimes [_ 1000]
                      (dosync (alter x + (apply + (range 1000)))))
                    (dotimes [_ 1000]
                      (dosync (alter x - (apply + (range 1000)))))))

(time (wait-futures 5
                    (dotimes [_ 1000]
                      (dosync (commute x + (apply + (range 1000)))))
                    (dotimes [_ 1000]
                      (dosync (commute x - (apply + (range 1000)))))))

(defn flawed-loot
  [from to]
  (dosync
   (when-let [item (first (:items @from))]
     (commute to update-in [:items] conj item)
     (commute from update-in [:items] disj item))))

(def smaug (character "Smaug" :health 500 :strength 400 :items (set (range 50))))
(def bilbo (character "Bilbo" :health 100 :strength 100))
(def gandalf (character "Gandalf" :health 75 :mana 750))

(wait-futures 1
              (while (flawed-loot smaug bilbo))
              (while (flawed-loot smaug gandalf)))

(map (comp count :items deref) [bilbo gandalf])
(filter (:items @bilbo) (:items @gandalf))

(defn fixed-loot
  [from to]
  (dosync
   (when-let [item (first (:items @from))]
     (commute to update-in [:items] conj item)
     (alter from update-in [:items] disj item))))

(def smaug (character "Smaug" :health 500 :strength 400 :items (set (range 50))))
(def bilbo (character "Bilbo" :health 100 :strength 100))
(def gandalf (character "Gandalf" :health 75 :mana 750))

(wait-futures 1
              (while (fixed-loot smaug bilbo))
              (while (fixed-loot smaug gandalf)))

(map (comp count :items deref) [bilbo gandalf])
(filter (:items @bilbo) (:items @gandalf))

(defn attack
  [aggressor target]
  (dosync
   (let [damage (* (rand 0.1) (:strength @aggressor))]
     (commute target update-in [:health] #(max 0 (- % damage))))))

(defn heal
  [healer target]
  (dosync
   (let [aid (* (rand 0.1) (:mana @healer))]
     (when (pos? aid)
       (commute healer update-in [:mana] - (max 5 (/ aid 5)))
       (commute target update-in [:health] + aid)))))

(def alive? (comp pos? :health))

(defn play
  [character action other]
  (while (and (alive? @character)
              (alive? @other)
              (action character other))
    (Thread/sleep (rand-int 50))))

(wait-futures 1
              (play bilbo attack smaug)
              (play smaug attack bilbo))

(map (comp :health deref) [smaug bilbo])

(dosync
 (commute smaug assoc :health 500)
 (commute bilbo assoc :health 100))

(wait-futures 1
              (play bilbo attack smaug)
              (play smaug attack bilbo)
              (play gandalf heal bilbo))

(map (comp #(select-keys % [:name :health :mana]) deref) [smaug bilbo gandalf])

; Clobbering ref state with ref-set

(dosync (ref-set bilbo {:name "Bilbo"}))
(dosync (alter bilbo (constantly {:name "Bilbo"})))

; Enforcing local consistency by using validators

(defn- enforce-max-health
  [{:keys [name health]}]
  (fn [character-data]
    (or (<= (:health character-data) health)
        (throw (IllegalStateException. (str name " is already at max health!"))))))

(defn character
  [name & {:as opts}]
  (let [cdata (merge {:name name :items #{} :health 500}
                     opts)
        cdata (assoc cdata :max-health (:health cdata))
        validators (list* (enforce-max-health {:name name :health (:health cdata)})
                          (:validators cdata))]
    (ref (dissoc cdata :validators)
         :validator #(every? (fn [v] (v %)) validators))))

(def bilbo (character "Bilbo" :health 100 :strength 100))

;; (heal gandalf bilbo) ; IllegalStateException: Bilbo is already at max health

(dosync (alter bilbo assoc-in [:health] 95))
(heal gandalf bilbo) ; IllegalStateException.

(defn heal
  [healer target]
  (dosync
   (let [aid (min (* (rand 0.1) (:mana @healer))
                  (- (:max-health @target) (:health @target)))]
     (when (pos? aid)
       (commute healer update-in [:mana] - (max 5 (/ aid 5)))
       (alter target update-in [:health] + aid)))))

(dosync (alter bilbo assoc-in [:health] 95))
(heal gandalf bilbo)
(heal gandalf bilbo)

; The sharp corners of STM
; Side effecting functions strictly verboten

(defn unsafe
  []
  (io! (println "writing to databse...")))
(dosync (unsafe)) ; IllegalStateException: I/O in transaction

(def x (ref (java.util.ArrayList.)))
(wait-futures 2 (dosync (dotimes [v 5]
                          (Thread/sleep (rand-int 50))
                          alter x #(doto % (.add v)))))
@x

; Live lock
(def x (ref 0))
(dosync
 @(future (dosync (ref-set x 0)))
 (ref-set x 1))

(ref-max-history (ref "abc" :min-history 3 :max-history 30))

(def a (ref 0))
(future (dotimes [_ 500] (dosync (Thread/sleep 200) (alter inc a))))
@(future (dosync (Thread/sleep 1000) @a))
(ref-history-count a)

(def a (ref 0 :max-history 100))
(future (dotimes [_ 500] (dosync (Thread/sleep 20) (alter a inc))))
@(future (dosync (Thread/sleep 1000) @a))
(ref-history-count a)

(def a (ref 0 :min-history 50 :max-history 100))
(future (dotimes [_ 500] (dosync (Thread/sleep 20) (alter a inc))))
@(future (dosync (Thread/sleep 1000) @a))
(ref-history-count a)

; Write skew

(def daylight (ref 1))

(defn attack
  [aggressor target]
  (dosync
   (let [damage (* (rand 0.1) (:strength @aggressor) @daylight)]
     (commute target update-in [:health] #(max 0 (- % damage))))))

(wait-futures 1
              (play bilbo attack smaug)
              (play smaug attack bilbo))

; Vars

map
#'map ; <=> (var map)
@#'map

; Private vars

(def ^:private everything 42)
(def ^{:private true} everything 42) ; both equivalent

(ns other-namespace)

(refer 'sandbox.clj)
everything ; RuntimeException : unable to resolve symbol everything in this context
@#'sandbox.clj/everything

(def a
  "A sample value"
  5)

(defn b
  "A simple calculation using `a`"
  [c]
  (+ a c))

(clojure.repl/doc a) ; prints doc in console
(clojure.repl/doc b)

(meta #'a)

(def ^{:doc "A sample value."} a 5)
(clojure.repl/doc a)
(alter-meta! #'a assoc :doc "A dummy value")
(clojure.repl/doc a)

; Constants

(def max-value 255)
(defn valid-value?
  [v]
  (<= v max-value))
(valid-value? 218)
(valid-value? 298)
(def max-value 500)
(valid-value? 299)

(def ^:const max-value 255)
(defn valid-value?
  [v]
  (<= v max-value))
(def max-value 500)
(valid-value? 299)

; Dynamic Scope

(let [a 1
      b 2]
  (println (+ a b))
  (let [b 3
        + -]
    (println (+ a b))))

(def ^:dynamic *max-value* 255)
(defn valid-value? [v] (<= v *max-value*))
(binding [*max-value* 500]
  (valid-value? 299))
(valid-value? 299)

(binding [*max-value* 500]
  (println (valid-value? 299))
  (doto (Thread. #(println "in other thread:" (valid-value? 299)))
    .start
    .join))

(def ^:dynamic *var* :root)
(defn get-*var* [] *var*)
(binding [*var* :a]
  (binding [*var* :b]
    (binding [*var* :c]
      (get-*var*))))

(defn http-get
  [url-string]
  (let [conn (-> url-string java.net.URL. .openConnection)
        response-code (.getResponseCode conn)]
    (if (== 404 response-code)
      [response-code]
      [response-code (-> conn .getInputStream slurp)])))

(http-get "http://google.com/bad-url")
(http-get "http://google.com")

(def ^:dynamic *response-code* nil)
(defn http-get
  [url-string]
  (let [conn (-> url-string java.net.URL. .openConnection)
        response-code (.getResponseCode conn)]
    (when (thread-bound? #'*response-code*)
      (set! *response-code* response-code))
    (when (not= 404 response-code) (-> conn .getInputStream slurp))))

(http-get "http://google.com")
*response-code*

(binding [*response-code* nil]
  (let [content (http-get "http://google.com/bad-url")]
    (println "Response code was:" *response-code*)
    ; do stuff
    ))

*response-code*

; Dynamic scope propagates through Clojure-native concurrency forms

(binding [*max-value* 500]
  (println (valid-value? 299))
  @(future (valid-value? 299)))

(binding [*max-value* 500]
  (map valid-value? [299]))

(map #(binding [*max-value* 500]
        (valid-value? %))
     [299])

; Vars are not variables

(defn never-do-this []
  (def x 123)
  (def y 456)
  (def x (+ x y))
  x)
x
(never-do-this)
x

(def x 80)
(defn never-do-this []
  (def x 123)
  (def y 456)
  (def x (+ x y))
  x)
x
(never-do-this)
x

; Changing a var's root binding

(def x 0)
(alter-var-root #'x inc)
x

; Forward declarations

(def j)
j

(declare complex-helper-fn other-helper-fn)

(defn public-api-function
  [arg1 arg2]
  ; do stuff
  (other-helper-fn arg1 arg2 (complex-helper-fn arg1 arg2)))

(defn- complex-helper-fn
  [arg1 arg2]
  ; do stuff
  )

(defn- other-helper-fn
  [arg1 arg2]
  ; do stuff
  )

; Agents

(def a (agent 500))
(send a range 502)
@a

(def a (agent 0))
(send a inc)

(def a (agent 4000))
(def b (agent 11000))

(send-off a #(Thread/sleep %))
(send-off b #(Thread/sleep %))
@a

(await a b)
@a
@b

(def a (agent nil))
(send a (fn [_] (throw (Exception. "something is wrong"))))
a
(send a identity)

; Dealing with erros in agent actions

(restart-agent a 42)
(send a inc)
(reduce send a (for [x (range 3)]
                 (fn [_] (throw (Exception. (str ("error #" x)))))))
(agent-error a)
(restart-agent a 42)
(agent-error a)
(restart-agent a 42 :clear-actions true)
(agent-error a)

; Agents error handlers and modes

(def a (agent nil :error-mode :continue))
(send a (fn [_] (throw (Exception. "something is wrong"))))
(send a identity)

(def a (agent nil
              :error-mode :continue
              :error-handler (fn [the-agent exception]
                               (.println System/out (.getMessge exception)))))
(send a (fn [_] (throw (Exception. "something is wrong"))))
(send a identity)

(set-error-handler! a (fn [the-agent exception]
                        (when (= "FATAL" (.getMessage exception))
                          (set-error-mode! the-agent :fail))))

(send a (fn [_] (throw (Exception. "FATAL"))))
(send a identity)

; Persisting reference states with an agent-base write-behind log

(require '[clojure.java.io :as io])

(def console (agent *out*))
(def character-log (agent (io/writer "character-states.log" :append true)))

(defn write
  [^java.io.Writer w & content]
  (doseq [x (interpose " " content)]
    (.write w (str x)))
  (doto w
    (.write "\n")
    .flush))

(defn log-reference
  [reference & writer-agents]
  (add-watch reference :log
             (fn [_ reference old new]
               (doseq [writer-agent writer-agents]
                 (send-off writer-agent write new)))))

(def smaug (character "Smaug" :health 500 :strength 400))
(def bilbo (character "Bilbo" :health 100 :health 100))
(def gandalf (character "Gandalf" :health 75 :mana 1000))

(log-reference bilbo console character-log)
(log-reference smaug console character-log)

(wait-futures 1
              (play bilbo attack smaug)
              (play smaug attack bilbo)
              (play gandalf heal bilbo))
