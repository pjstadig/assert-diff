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
  (is (= #{(pass* {:file "assert_diff_test.clj" :line 16} :a :a "msg")}
         (reporting (is (= :a :a) "msg"))))
  (is (= #{(fail* {:file "assert_diff_test.clj" :line 18} :a :b "msg")}
         (reporting (is (= :a :b) "msg")))))

(deftest t-assert-map-diff
  (is (= #{(pass* {:file "assert_diff_test.clj" :line 23} {:a :b} {:a :b}
                  "msg")}
         (reporting (is (= {:a :b} {:a :b}) "msg"))))
  (is (= #{(fail* {:file "assert_diff_test.clj" :line 25} :d :e "msg: in [:c]")}
         (reporting (is (= {:a :b :c :d} {:a :b :c :e}) "msg"))))
  (is (= #{(fail* {:file "assert_diff_test.clj" :line 28} #{:a} #{}
                  "msg: map is missing keys")}
         (reporting (is (= {:a :b} {}) "msg"))))
  (is (= #{(fail* {:file "assert_diff_test.clj" :line 31} #{} #{:a}
                  "msg: map has extra keys")}
         (reporting (is (= {} {:a :b}) "msg"))))
  (is (= #{(fail* {:file "assert_diff_test.clj" :line 35} #{} #{:f}
                  "msg: map has extra keys")
           (fail* {:file "assert_diff_test.clj" :line 35} :d :e "msg: in [:c]")}
         (reporting (is (= {:a :b :c :d} {:a :b :c :e :f :g}) "msg")))))

(deftest t-assert-vector-diff
  (is (= #{(pass* {:file "assert_diff_test.clj" :line 39} [:a] [:a] "msg")}
         (reporting (is (= [:a] [:a]) "msg"))))
  (is (= #{(fail* {:file "assert_diff_test.clj" :line 41} :b :d "msg: in [1]")}
         (reporting (is (= [:a :b :c] [:a :d :c]) "msg"))))
  (is (= #{(fail* {:file "assert_diff_test.clj" :line 44} 2 1
                  "msg: vector length is different")}
         (reporting (is (= [:a :b] [:a]) "msg"))))
  (is (= #{(fail* {:file "assert_diff_test.clj" :line 47} 1 2
                  "msg: vector length is different")}
         (reporting (is (= [:a] [:a :b]) "msg")))))

(deftest t-assert-set-diff
  (is (= #{(pass* {:file "assert_diff_test.clj" :line 51} #{:a} #{:a} "msg")}
         (reporting (is (= #{:a} #{:a}) "msg"))))
  (is (= #{(fail* {:file "assert_diff_test.clj" :line 56} #{} #{:c}
                  "msg: set has extra values")
           (fail* {:file "assert_diff_test.clj" :line 56} #{:b} #{}
                  "msg: set is missing values")}
         (reporting (is (= #{:a :b} #{:a :c}) "msg")))))

(deftest t-assert-seq-diff
  (is (= #{(pass* {:file "assert_diff_test.clj" :line 60} '(:a) '(:a) "msg")}
         (reporting (is (= '(:a) '(:a)) "msg"))))
  (is (= #{(fail* {:file "assert_diff_test.clj" :line 62} :b :d "msg: in [1]")}
         (reporting (is (= '(:a :b :c) '(:a :d :c)) "msg"))))
  (is (= #{(fail* {:file "assert_diff_test.clj" :line 65} 2 1
                  "msg: seq length is different")}
         (reporting (is (= '(:a :b) '(:a)) "msg"))))
  (is (= #{(fail* {:file "assert_diff_test.clj" :line 68} 1 2
                  "msg: seq length is different")}
         (reporting (is (= '(:a) '(:a :b)) "msg"))))
  (testing "seq first"
    (is (= #{(pass* {:file "assert_diff_test.clj" :line 71} '(:a) '(:a) "msg")}
           (reporting (is (= (lazy-seq '(:a)) '(:a)) "msg"))))
    (is (= #{(fail* {:file "assert_diff_test.clj" :line 74} :b :d
                    "msg: in [1]")}
           (reporting (is (= (lazy-seq '(:a :b :c)) '(:a :d :c)) "msg"))))
    (is (= #{(fail* {:file "assert_diff_test.clj" :line 77} 2 1
                    "msg: seq length is different")}
           (reporting (is (= (lazy-seq '(:a :b)) '(:a)) "msg"))))
    (is (= #{(fail* {:file "assert_diff_test.clj" :line 80} 1 2
                    "msg: seq length is different")}
           (reporting (is (= (lazy-seq '(:a)) '(:a :b)) "msg")))))
  (testing "seq last"
    (is (= #{(pass* {:file "assert_diff_test.clj" :line 83} '(:a) '(:a) "msg")}
           (reporting (is (= '(:a) (lazy-seq '(:a))) "msg"))))
    (is (= #{(fail* {:file "assert_diff_test.clj" :line 85} :b :d "msg: in [1]")}
           (reporting (is (= '(:a :b :c) (lazy-seq '(:a :d :c))) "msg"))))
    (is (= #{(fail* {:file "assert_diff_test.clj" :line 88} 2 1
                    "msg: seq length is different")}
           (reporting (is (= '(:a :b) (lazy-seq '(:a))) "msg"))))
    (is (= #{(fail* {:file "assert_diff_test.clj" :line 91} 1 2
                    "msg: seq length is different")}
           (reporting (is (= '(:a) (lazy-seq '(:a :b))) "msg"))))))

(deftest t-recursive-diff
  (is (= #{(fail* {:file "assert_diff_test.clj" :line 98} #{:c} #{}
                  "msg: in [:a :b 0] set is missing values")
           (fail* {:file "assert_diff_test.clj" :line 98} #{} #{:d}
                  "msg: in [:a :b 0] set has extra values")}
         (reporting (is (= {:a {:b [#{:c}]}} {:a {:b [#{:d}]}}) "msg")))))
