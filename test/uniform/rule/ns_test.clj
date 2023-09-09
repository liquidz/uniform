(ns uniform.rule.ns-test
  (:require
    [clojure.test :as t]
    [rewrite-clj.node :as r.node]
    [rewrite-clj.parser :as r.parser]
    [uniform.rule :as u.rule]
    [uniform.rule.ns :as sut]))

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

(t/deftest TODO-require-importが複数ある場合の改行
  (let [test-rule* (partial test-rule sut/ns-missing-linebreaks)]
    (test-rule*
      "(ns foo\n(:require\n[foo]\n[bar]))"
      "(ns foo\n(:require [foo]\n[bar]))")
    (test-rule*
      "(ns foo\n(:import\n[foo]\n[bar]))"
      "(ns foo\n(:import [foo]\n[bar]))")
    (test-rule*
      "(ns foo\n(:require\n[foo]\n[bar]))"
      "(ns foo\n(:require [foo] [bar]))")
    (test-rule*
      "(ns foo\n(:import\n[foo]\n[bar]))"
      "(ns foo\n(:import [foo] [bar]))")
    (test-rule*
      "(ns foo\n(:require [foo]))"
      "(ns foo\n(:require [foo]))")
    (test-rule*
      "(ns foo\n(:import [foo]))"
      "(ns foo\n(:import [foo]))")))
