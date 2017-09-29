(ns pjstadig.assert-diff
  (:require
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
  [line expected actual msg]
  {:type :pass
   :expected expected
   :actual actual
   :message (context-msg msg)
   :line line})

(defn pass
  [line expected actual msg]
  (do-report (pass* line expected actual msg)))

(defn fail*
  [line expected actual msg & other-msg]
  {:type :fail
   :expected expected
   :actual actual
   :message (context-msg msg other-msg)
   :line line})

(defn fail
  [line expected actual msg & other-msg]
  (do-report (apply fail* line expected actual msg other-msg)))

(declare assert-diff)

(defn assert-map-diff
  [line expected actual msg]
  (let [expected-keys (set (keys expected))
        actual-keys (set (keys actual))]
    (if (not= expected-keys actual-keys)
      (let [missing-keys (set/difference expected-keys actual-keys)
            extra-keys (set/difference actual-keys expected-keys)]
        (when (seq missing-keys)
          (fail line missing-keys #{} msg "map is missing keys"))
        (when (seq extra-keys)
          (fail line #{} extra-keys msg "map has extra keys")))
      (doseq [key expected-keys]
        (when (not= (get expected key) (get actual key))
          (with-context key
            (assert-diff line (get expected key) (get actual key) msg)))))))

(defn assert-vector-diff
  [line expected actual msg]
  (let [expected-count (count expected)
        actual-count (count actual)]
    (if (not= expected-count actual-count)
      (fail line expected-count actual-count msg "vector length is different")
      (doseq [[i expected] (map vector (range) expected)
              :let [actual (nth actual i)]]
        (when (not= expected actual)
          (with-context i
            (assert-diff line expected actual msg)))))))

(defn assert-set-diff
  [line expected actual msg]
  (let [missing-values (set/difference expected actual)
        extra-values (set/difference actual expected)]
    (when (seq missing-values)
      (fail line missing-values #{} msg "set is missing values"))
    (when (seq extra-values)
      (fail line #{} extra-values msg "set has extra values"))))

(defn assert-list-diff
  [line expected actual msg]
  (let [expected-count (count expected)
        actual-count (count actual)]
    (if (not= expected-count actual-count)
      (fail line expected-count actual-count msg "list length is different")
      (doseq [[i expected actual] (map vector (range) expected actual)]
        (when (not= expected actual)
          (with-context i
            (assert-diff line expected actual msg)))))))

(defn assert-diff
  [line expected actual msg]
  (cond
    (and (map? expected) (map? actual))
    (assert-map-diff line expected actual msg)
    (and (vector? expected) (vector? actual))
    (assert-vector-diff line expected actual msg)
    (and (set? expected) (set? actual))
    (assert-set-diff line expected actual msg)
    (and (list? expected) (list? actual))
    (assert-list-diff line expected actual msg)
    :else
    (fail line expected actual msg)))

(defmethod assert-expr '=
  [msg [_ expected & actuals :as form]]
  (let [line (:line (meta form))]
    (if (empty? actuals)
      (fail line 2 1 "= expects at least two arguments")
      `(let [line# ~line
             expected# ~expected
             actuals# (list ~@actuals)
             msg# ~msg]
         (doseq [actual# actuals#]
           (if (= expected# actual#)
             (pass line# expected# actual# msg#)
             (assert-diff line# expected# actual# msg#)))))))
