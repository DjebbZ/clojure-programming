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

; primitive logging system
(defn print-logger
  [writer]
  #(binding [*out* writer]
     (println %)))

(def *out*-logger (print-logger *out*))
(*out*-logger "hello")

(def writer (java.io.StringWriter.))
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
(javadoc java.io.BufferedWriter)

(System/getProperty "user.dir")
(.getAbsolutePath (java.io.File. "."))

(defn multi-logger
  [& logger-fns]
  #(doseq [f logger-fns]
     (f %)))

(def log (multi-logger *out*-logger log->file))
(log "hello again")

(javadoc java.util.Formatter)
(defn timestamped-logger
  [logger]
  #(logger (format "[%1$tY-%1$tm-%1$te %1$tH:%1$tM:%1$tS] %2$s" (java.util.Date.) %)))

(def log-timestamped (timestamped-logger log))
(log-timestamped "goodbye, now")

; side-effects: I/O
(require '[clojure.xml :as xml])

; doesn't work anymore since Twitter 1.1 API requires authentication
(defn twitter-followers
  [username]
  (->> (str "https://api.twitter.com/1.1/users/show.xml?screen_name" username)
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
