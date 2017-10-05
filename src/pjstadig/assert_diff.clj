(ns pjstadig.assert-diff
  (:require
   [clojure.data :as data]
   [clojure.set :as set]
   [clojure.string :as string]
   [clojure.test :refer [assert-expr do-report]]))

(def ^:dynamic *context*
  [])

(defmacro with-context
  [x & body]
  `(binding [*context* (conj *context* ~x)]
     ~@body))

(defn context-msg
  ([msg]
   (context-msg msg nil))
  ([msg other-msg]
   (str (when (seq msg)
          (str msg
               (when (or (seq *context*) (seq other-msg))
                 ": ")))
        (when (seq *context*)
          (str "in [" (string/join " " *context*) "]"
               (when (seq other-msg)
                 " ")))
        (when (seq other-msg)
          (str (string/join " " other-msg))))))

(defn pass*
  [file-and-line expected actual msg]
  (merge file-and-line
         {:type :pass
          :expected expected
          :actual actual
          :message (context-msg msg)}))

(defn pass
  [file-and-line expected actual msg]
  (do-report (pass* file-and-line expected actual msg)))

(defn fail*
  [file-and-line expected actual msg & other-msg]
  (merge file-and-line
         {:type :fail
          :expected expected
          :actual actual
          :message (context-msg msg other-msg)}))

(defn fail
  [file-and-line expected actual msg & other-msg]
  (do-report (apply fail* file-and-line expected actual msg other-msg)))

(declare assert-diff)

(defn assert-map-diff
  [file-and-line expected actual msg]
  (let [[missing-keys extra-keys both-keys] (data/diff (set (keys expected))
                                                       (set (keys actual)))]
    (when (seq missing-keys)
      (fail file-and-line missing-keys #{} msg "map is missing keys"))
    (when (seq extra-keys)
      (fail file-and-line #{} extra-keys msg "map has extra keys"))
    (doseq [key both-keys]
      (when (not= (get expected key) (get actual key))
        (with-context key
          (assert-diff file-and-line (get expected key) (get actual key)
                       msg))))))

(defn assert-vector-diff
  [file-and-line expected actual msg]
  (let [expected-count (count expected)
        actual-count (count actual)]
    (if (not= expected-count actual-count)
      (fail file-and-line expected-count actual-count msg
            "vector length is different"))
    (doseq [i (range (min expected-count actual-count))
            :let [expected (nth expected i)
                  actual (nth actual i)]]
      (when (not= expected actual)
        (with-context i
          (assert-diff file-and-line expected actual msg))))))

(defn assert-set-diff
  [file-and-line expected actual msg]
  (let [missing-values (set/difference expected actual)
        extra-values (set/difference actual expected)]
    (when (seq missing-values)
      (fail file-and-line missing-values #{} msg "set is missing values"))
    (when (seq extra-values)
      (fail file-and-line #{} extra-values msg "set has extra values"))))

(defn assert-seq-diff
  [file-and-line expected actual msg]
  (loop [expected (seq expected)
         actual (seq actual)
         i 0]
    (cond
      (and expected actual)
      (do (when (not= (first expected) (first actual))
            (with-context i
              (assert-diff file-and-line (first expected) (first actual) msg)))
          (recur (next expected) (next actual) (inc i)))
      (or expected actual)
      (fail file-and-line (+ i (count expected)) (+ i (count actual)) msg
            "seq length is different"))))

(defn assert-diff
  [file-and-line expected actual msg]
  (cond
    (and (map? expected) (map? actual))
    (assert-map-diff file-and-line expected actual msg)
    (and (vector? expected) (vector? actual))
    (assert-vector-diff file-and-line expected actual msg)
    (and (set? expected) (set? actual))
    (assert-set-diff file-and-line expected actual msg)
    (and (sequential? expected) (sequential? actual))
    (assert-seq-diff file-and-line expected actual msg)
    :else
    (fail file-and-line expected actual msg)))

(defmacro file-and-line
  ([]
   `(@#'clojure.test/stacktrace-file-and-line
     (drop-while
      #(let [cl-name# (.getClassName ^StackTraceElement %)]
         (or (string/starts-with? cl-name# "java.lang.")
             (string/starts-with? cl-name# "clojure.test$")))
      (.getStackTrace (Thread/currentThread)))))
  ([line]
   `(merge (file-and-line) {:line ~line})))

(defn assert-expr-body
  [msg expected actuals]
  `(let [file-and-line# (file-and-line)
         expected# ~expected
         actuals# (list ~@actuals)
         msg# ~msg]
     (if (empty? actuals#)
       (fail file-and-line# 2 1 "= expects at least two arguments")
       (doseq [actual# actuals#]
         (if (= expected# actual#)
           (pass file-and-line# expected# actual# msg#)
           (assert-diff file-and-line# expected# actual# msg#))))))

(defmethod assert-expr '=
  [msg [_ expected & actuals :as form]]
  (assert-expr-body msg expected actuals))

(defmethod assert-expr `=
  [msg [_ expected & actuals :as form]]
  (assert-expr-body msg expected actuals))
