(ns uniform.core
  (:require
    [rewrite-clj.node :as r.node]
    [rewrite-clj.parser :as r.parser]
    [rewrite-clj.zip :as r.zip]
    [uniform.rule :as u.rule]
    [uniform.rule.base :as u.r.base]
    [uniform.rule.indent :as u.r.indent]
    [uniform.rule.ns :as u.r.ns]))

(defn format-code
  [code]
  (-> code
      (r.parser/parse-string-all)
      (u.rule/apply-rules
        [u.r.base/missing-whitespace
         u.r.base/hard-tab-to-space
         u.r.base/too-many-spaces
         u.r.base/too-many-linebreaks
         u.r.base/comma])
      (u.rule/apply-rules
        #(r.zip/find-value % r.zip/next 'ns)
        [u.r.ns/ns-missing-linebreaks])
      (u.rule/apply-rules
        [u.r.indent/indent])
      (r.node/string)))

(comment
  (let [org  "(ns (:require [foo] [bar]))\n(   foo , (bar(baz)  )\n\n      baz)
             {:foo   \"  bar\"\n:bar baz,:baz hello,\n}"]
    (println "..........................................")
    (println "..........................................")
    (println "..........................................")
    (println org)
    (println "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")
    (println (format-code org))))
