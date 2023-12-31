(ns uniform.util.zip-test
  (:require
    [clojure.test :as t]
    [rewrite-clj.zip :as r.zip]
    [uniform.test-helper :as h]
    [uniform.util.zip :as sut]))

(defn- next-target
  [zloc]
  (r.zip/find-value zloc r.zip/next 'TARGET))

(t/deftest expression?-test
  (t/is (true? (-> (h/parse "foo")
                   (r.zip/next*)
                   (sut/expression?))))
  (t/is (true? (-> (h/parse "(foo)")
                   (r.zip/next*)
                   (sut/expression?))))
  (t/is (false? (-> (h/parse " ")
                    (r.zip/next*)
                    (sut/expression?))))
  (t/is (false? (-> (h/parse "\t")
                    (r.zip/next*)
                    (sut/expression?))))
  (t/is (false? (-> (h/parse "\n")
                    (r.zip/next*)
                    (sut/expression?)))))

(t/deftest whitespace?-test
  (let [whitespace?-test #(->
                            (h/parse %)
                            (r.zip/next*)
                            (sut/whitespace?))]
    (t/is (false? (whitespace?-test "foo")))
    (t/is (true? (whitespace?-test " ")))
    (t/is (true? (whitespace?-test "  ")))
    (t/is (true? (whitespace?-test "\t")))))

(t/deftest previous-line-test
  (t/is (= ""
           (-> (h/parse "(\nfoo\nbar\nbaz)")
               (sut/previous-line))))

  (t/is (= "("
           (-> (h/parse "(\nTARGET\nbar\nbaz)")
               (next-target)
               (h/prev-linebreak)
               (sut/previous-line))))

  (t/is (= "foo"
           (-> (h/parse "(\nfoo\nTARGET\nbaz)")
               (next-target)
               (h/prev-linebreak)
               (sut/previous-line))))

  (t/is (= "bar"
           (-> (h/parse "(\nfoo\nbar\nTARGET)")
               (next-target)
               (h/prev-linebreak)
               (sut/previous-line))))

  (t/is (= "#("
           (-> (h/parse "#(TARGET %)")
               (next-target)
               (sut/previous-line))))

  (t/is (= "^{:foo 'bar}("
           (-> (h/parse "^{:foo 'bar}(TARGET)")
               (next-target)
               (sut/previous-line))))

  (t/is (= "(foo ("
           (-> (h/parse "(foo (TARGET))")
               (next-target)
               (sut/previous-line)))))

(t/deftest in-quoted-list?-test
  (t/is (true? (-> (h/parse "'(foo TARGET)")
                   (next-target)
                   (sut/in-quoted-list?))))
  (t/is (false? (-> (h/parse "~(foo TARGET)")
                    (next-target)
                    (sut/in-quoted-list?))))
  (t/is (false? (-> (h/parse "(foo TARGET)")
                    (next-target)
                    (sut/in-quoted-list?)))))

(t/deftest in-map?-test
  (t/is (true? (-> (h/parse "'{:foo TARGET}")
                   (next-target)
                   (sut/in-map?))))
  (t/is (false? (-> (h/parse "'{:foo :bar} TARGET")
                    (next-target)
                    (sut/in-map?))))
  (t/is (false? (-> (h/parse "'(:foo TARGET)")
                    (next-target)
                    (sut/in-map?)))))
