(ns uniform.test-helper
  (:require
    [rewrite-clj.node :as r.node]
    [rewrite-clj.parser :as r.parser]
    [rewrite-clj.zip :as r.zip]))

(defn parse
  [s]
  (-> s
      (r.parser/parse-string-all)
      (r.zip/of-node* {:track-position? true})))

(defn root-str
  [zloc]
  (-> zloc
      (r.zip/root)
      (r.node/string)))

(defn next-linebreak
  [zloc]
  (cond->> zloc
    (r.zip/linebreak? zloc)
    (r.zip/next*)

    :always
    (r.zip/skip r.zip/next* (complement r.zip/linebreak?))))

(defn prev-linebreak
  [zloc]
  (r.zip/skip r.zip/prev* (complement r.zip/linebreak?) zloc))
