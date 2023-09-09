(ns uniform.core
  (:require
    [rewrite-clj.node :as r.node]
    [rewrite-clj.parser :as r.parser]
    [rewrite-clj.zip :as r.zip]
    [uniform.rule :as u.rule]
    [uniform.rule.ns :as u.r.ns]))

(defn format-code
  [code]
  (-> code
      (r.parser/parse-string-all)
      (u.rule/apply-rules
        [u.rule/missing-whitespace
         u.rule/hard-tab-to-space
         u.rule/too-many-spaces
         u.rule/too-many-linebreaks
         u.rule/comma])
      (u.rule/apply-rules
        #(r.zip/find-value % r.zip/next 'ns)
        [u.r.ns/ns-missing-linebreaks])
      (u.rule/apply-rules
        [u.rule/indent])
      (r.node/string)))

(comment
  (let [org  "(ns (:require [foo] [bar]))\n(   foo , (bar(baz)  )\n\n      baz)
             {:foo   \"  bar\"}"]
    (println org)
    (println "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")
    (println (format-code org))))
