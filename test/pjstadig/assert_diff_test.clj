(ns pjstadig.assert-diff-test
  (:require
   [clojure.test :refer :all]
   [pjstadig.assert-diff :refer :all]))

(defmacro reporting
  [& body]
  `(let [reports# (volatile! #{})]
     (with-redefs [do-report (fn [report#]
                               (vswap! reports# conj report#))]
       ~@body)
     @reports#))

(deftest t-assert-object-diff
  (is (= #{(pass* 16 :a :a "msg")}
         (reporting (is (= :a :a) "msg"))))
  (is (= #{(fail* 18 :a :b "msg")}
         (reporting (is (= :a :b) "msg")))))

(deftest t-assert-map-diff
  (is (= #{(pass* 22 {:a :b} {:a :b} "msg")}
         (reporting (is (= {:a :b} {:a :b}) "msg"))))
  (is (= #{(fail* 24 :d :e "msg: in [:c]")}
         (reporting (is (= {:a :b :c :d} {:a :b :c :e}) "msg"))))
  (is (= #{(fail* 26 #{:a} #{} "msg: map is missing keys")}
         (reporting (is (= {:a :b} {}) "msg"))))
  (is (= #{(fail* 28 #{} #{:a} "msg: map has extra keys")}
         (reporting (is (= {} {:a :b}) "msg")))))

(deftest t-assert-vector-diff
  (is (= #{(pass* 32 [:a] [:a] "msg")}
         (reporting (is (= [:a] [:a]) "msg"))))
  (is (= #{(fail* 34 :b :d "msg: in [1]")}
         (reporting (is (= [:a :b :c] [:a :d :c]) "msg"))))
  (is (= #{(fail* 36 2 1 "msg: vector length is different")}
         (reporting (is (= [:a :b] [:a]) "msg"))))
  (is (= #{(fail* 38 1 2 "msg: vector length is different")}
         (reporting (is (= [:a] [:a :b]) "msg")))))

(deftest t-assert-set-diff
  (is (= #{(pass* 42 #{:a} #{:a} "msg")}
         (reporting (is (= #{:a} #{:a}) "msg"))))
  (is (= #{(fail* 45 #{} #{:c} "msg: set has extra values")
           (fail* 45 #{:b} #{} "msg: set is missing values")}
         (reporting (is (= #{:a :b} #{:a :c}) "msg")))))

(deftest t-assert-seq-diff
  (is (= #{(pass* 49 '(:a) '(:a) "msg")}
         (reporting (is (= '(:a) '(:a)) "msg"))))
  (is (= #{(fail* 51 :b :d "msg: in [1]")}
         (reporting (is (= '(:a :b :c) '(:a :d :c)) "msg"))))
  (is (= #{(fail* 53 2 1 "msg: seq length is different")}
         (reporting (is (= '(:a :b) '(:a)) "msg"))))
  (is (= #{(fail* 55 1 2 "msg: seq length is different")}
         (reporting (is (= '(:a) '(:a :b)) "msg"))))
  (testing "seq first"
    (is (= #{(pass* 58 '(:a) '(:a) "msg")}
           (reporting (is (= (lazy-seq '(:a)) '(:a)) "msg"))))
    (is (= #{(fail* 60 :b :d "msg: in [1]")}
           (reporting (is (= (lazy-seq '(:a :b :c)) '(:a :d :c)) "msg"))))
    (is (= #{(fail* 62 2 1 "msg: seq length is different")}
           (reporting (is (= (lazy-seq '(:a :b)) '(:a)) "msg"))))
    (is (= #{(fail* 64 1 2 "msg: seq length is different")}
           (reporting (is (= (lazy-seq '(:a)) '(:a :b)) "msg")))))
  (testing "seq last"
    (is (= #{(pass* 67 '(:a) '(:a) "msg")}
           (reporting (is (= '(:a) (lazy-seq '(:a))) "msg"))))
    (is (= #{(fail* 69 :b :d "msg: in [1]")}
           (reporting (is (= '(:a :b :c) (lazy-seq '(:a :d :c))) "msg"))))
    (is (= #{(fail* 71 2 1 "msg: seq length is different")}
           (reporting (is (= '(:a :b) (lazy-seq '(:a))) "msg"))))
    (is (= #{(fail* 73 1 2 "msg: seq length is different")}
           (reporting (is (= '(:a) (lazy-seq '(:a :b))) "msg"))))))

(deftest t-recursive-diff
  (is (= #{(fail* 78 #{:c} #{} "msg: in [:a :b 0] set is missing values")
           (fail* 78 #{} #{:d} "msg: in [:a :b 0] set has extra values")}
         (reporting (is (= {:a {:b [#{:c}]}} {:a {:b [#{:d}]}}) "msg")))))
