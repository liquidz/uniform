(ns uniform.core-test
  (:require
    [clojure.string :as str]
    [clojure.test :as t]
    [rewrite-clj.node :as r.node]
    [rewrite-clj.parser :as r.parser]
    [rewrite-clj.zip :as r.zip]
    [uniform.core :as sut]))

(defn- parse
  [s]
  (-> s
      (r.parser/parse-string-all)
      (r.zip/of-node* {:track-position? true})))

(defn- test*
  [{:keys [zloc rule expected]}]
  (let [{:keys [pred edit]} rule]
    (t/is (pred zloc))
    (t/is (= expected
             (-> (edit zloc)
                 (r.zip/root)
                 (r.node/string))))))

(t/deftest missing-whitespace-test
  (test* {:rule sut/missing-whitespace
          :zloc (-> (parse "(foo(bar))")
                    (r.zip/down)
                    (r.zip/next))
          :expected "(foo (bar))"}))









;
;
;
; (let [zloc (-> (parse "(foo\n  (bar\n                    (baz)))")
;                (->> (r.zip/skip r.zip/next* (complement r.zip/whitespace?)))
;                (r.zip/next)
;                (->> (r.zip/skip r.zip/next* (complement r.zip/whitespace?))))]
;   (-> (unindent zloc)
;
;       (r.zip/root)
;       (r.node/string)))
;
