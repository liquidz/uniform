(ns uniform.rule.indent
  (:require
    [rewrite-clj.zip :as r.zip]
    [uniform.indent :as u.indent]))

(def indent
  {:pred (fn [zloc]
           (r.zip/linebreak? zloc))
   :edit (fn [zloc]
           (-> zloc
               (u.indent/unindent)
               (u.indent/indent)))})
