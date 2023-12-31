(ns uniform.rule.base-test
  (:require
    [clojure.test :as t]
    [rewrite-clj.node :as r.node]
    [rewrite-clj.parser :as r.parser]
    [uniform.rule :as u.rule]
    [uniform.rule.base :as sut]))

(defn- apply-rule
  [rule s]
  (-> (r.parser/parse-string-all s)
      (u.rule/apply-rules [rule])
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
      "\"foo   bar\"")
    (test-rule*
      "(foo)"
      "(foo)   ")
    (test-rule*
      "(foo bar)"
      "(foo bar  )")))

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

(t/deftest comma-test
  (let [test-rule* (partial test-rule sut/comma)]
    (t/testing "not map"
      (test-rule*
        "foo bar baz"
        "foo,bar, baz")

      (test-rule*
        "(foo bar)"
        "(foo , bar)")

      (test-rule*
        "(foo bar baz)"
        "(foo,bar, baz)")
      (test-rule*
        "[foo bar baz]"
        "[foo,bar, baz]")

      (test-rule*
        "foo\nbar"
        "foo,\nbar"))

    (t/testing "map"
      (test-rule*
        "{foo bar, foo bar, foo bar\nfoo bar, foo bar foo bar}"
        "{foo bar,foo bar, foo bar,\nfoo bar , foo bar foo bar}"))))

(t/deftest hard-tab-to-space-test
  (let [test-rule* (partial test-rule sut/hard-tab-to-space)]
    (test-rule*
      "foo bar"
      "foo\tbar")

    (test-rule*
      "foo bar"
      "foo  \t\t  bar")

    (test-rule*
      "\"foo\tbar\""
      "\"foo\tbar\"")))
