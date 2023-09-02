(ns uniform.core
  (:require
    [rewrite-clj.node :as r.node]
    [rewrite-clj.parser :as r.parser]
    [uniform.rule :as u.rule]))

(defn format-code
  [code]
  (-> code
      (r.parser/parse-string-all)
      (u.rule/apply-rules [u.rule/missing-whitespace
                           u.rule/hard-tab-to-space
                           u.rule/too-many-spaces
                           u.rule/too-many-linebreaks
                           u.rule/comma])
      (u.rule/apply-rules [u.rule/indent])
      (r.node/string)))

(comment
  (let [org  "(   foo , (bar(baz)  )\n\n      baz)
             {:foo   \"  bar\"}"]
    (println org)
    (println "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")
    (println (format-code org))))
