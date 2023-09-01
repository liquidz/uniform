(ns uniform.rule-test
  (:require
    [clojure.test :as t]
    [rewrite-clj.node :as r.node]
    [rewrite-clj.parser :as r.parser]
    [uniform.rule :as sut]))

(defn- apply-rule
  [rule s]
  (-> (r.parser/parse-string-all s)
      (sut/apply-rules [rule])
      (r.node/string)))

(defn- test-rule
  [rule expected input]
  (t/is (= expected
           (apply-rule rule input)
           (apply-rule rule (apply-rule rule input)))))

(t/deftest missing-whitespace-test
  (let [test-rule* (partial test-rule sut/missing-whitespace)]
    (test-rule*
      "(foo (bar))"
      "(foo(bar))")
    (test-rule*
      "(= \"foo\" bar)"
      "(=\"foo\"bar)")))

(t/deftest too-many-spaces-test
  (let [test-rule* (partial test-rule sut/too-many-spaces)]
    (test-rule*
      "(foo bar)"
      "(foo   bar)")
    (test-rule*
      "(foo) (bar)"
      "(foo)   (bar)")
    (test-rule*
      "(foo)   ;; comment"
      "(foo)   ;; comment")
    (test-rule*
      "(foo bar)"
      "(foo\t\tbar)")
    (test-rule*
      "(foo\n   bar)"
      "(foo\n   bar)")
    (test-rule*
      "\"foo   bar\""
      "\"foo   bar\"")))

(t/deftest too-many-linebreaks-test
  (let [test-rule* (partial test-rule sut/too-many-linebreaks)]
    (test-rule*
      "foo\nbar"
      "foo\n\nbar")

    (test-rule*
      "foo\nbar"
      "foo\r\n\r\nbar")

    (test-rule*
      "foo"
      "\nfoo")

    (test-rule*
      "(foo)"
      "(\nfoo)")))

(t/deftest space-around-comma-test
  (let [test-rule* (partial test-rule sut/space-around-comma)]
    (test-rule*
      "foo, bar"
      "foo , bar")

    (test-rule*
      "foo, bar"
      "foo,bar")

    (test-rule*
      "foo\nbar"
      "foo,\nbar")))
