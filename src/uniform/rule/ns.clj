(ns uniform.rule.ns
  (:require
    [rewrite-clj.node :as r.node]
    [rewrite-clj.zip :as r.zip]
    [uniform.util.zip :as u.u.zip]))

(def ns-missing-linebreaks
  {:pred (fn [zloc]
           (and
             (not (r.zip/whitespace? zloc))
             (contains? #{:require :import} (r.zip/sexpr zloc))
             (-> (r.zip/up zloc)
                 (r.zip/child-sexprs)
                 (rest)
                 (count)
                 (>= 2))))
   :edit (fn [org-zloc]
           (if-let [zloc (r.zip/skip r.zip/right* (complement u.u.zip/whitespace?) org-zloc)]
             (loop [zloc zloc]
               (let [zloc (r.zip/replace* zloc (r.node/newlines 1))
                     next-whitespace-zloc (r.zip/skip r.zip/right* (complement u.u.zip/whitespace?) zloc)]
                 (if next-whitespace-zloc
                   (recur next-whitespace-zloc)
                   zloc)))
             org-zloc))})
