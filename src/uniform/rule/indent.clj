(ns uniform.rule.indent
  (:require
    [rewrite-clj.zip :as r.zip]
    [uniform.util.indent :as u.u.indent]))

(def indent
  {:pred (fn [zloc]
           (r.zip/linebreak? zloc))
   :edit (fn [zloc]
           (-> zloc
               (u.u.indent/unindent)
               (u.u.indent/indent)))})
