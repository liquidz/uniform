(ns uniform.core
  (:require
    [clojure.string :as str]
    [rewrite-clj.node :as r.node]
    [rewrite-clj.parser :as r.parser]
    [rewrite-clj.zip :as r.zip]
    [rewrite-clj.zip.whitespace :as r.z.whitespace]
    [uniform.indent :as u.indent]
    [uniform.zip :as u.zip]))

(def rules
  [missing-whitespace
   too-many-spaces
   too-many-linebreaks
   comma-next-to-space])

(defn x [v]
  (println "................")
  v)

(comment
  (let [org  "(   foo , (bar(baz)  )\n\n      baz)
             {:foo   \"  bar\"}"]
    (println org)
    (println "vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv")
    (def node
      (-> org
          (r.parser/parse-string-all)))
          ;(r.zip/of-node* {:track-position? true})))
    (-> node
        (apply-rules rules)
        (x)
        (apply-rules [indent])
        ;(r.zip/root)
        (r.node/string)
        (println))))

(def missing-whitespace
  {:pred (fn [zloc]
           (and (u.zip/expression? zloc)
                (u.zip/expression? (r.zip/right* zloc))))
   :edit (fn [zloc]
           (r.z.whitespace/insert-space-right zloc))})

(def too-many-spaces
  {:pred (fn [zloc]
           (and (r.zip/whitespace? zloc)
                (not (r.zip/linebreak? zloc))
                (> (count (r.zip/string zloc)) 1)))
   :edit (fn [zloc]
           (if (and (u.zip/expression? (r.zip/left* zloc))
                    (u.zip/expression? (r.zip/right* zloc)))
             (r.zip/replace* zloc (r.node/whitespace-node " "))
             (r.zip/remove* zloc)))})

(def too-many-linebreaks
  {:pred (fn [zloc]
           (and (r.zip/linebreak? zloc)
                (> (count (r.zip/string zloc)) 1)))
   :edit (fn [zloc]
           (r.zip/replace* zloc (r.node/newlines 1)))})

(def comma-next-to-space
  {:pred (fn [zloc]
           (and (u.zip/comma? zloc)
                (r.zip/whitespace? (r.zip/left* zloc))))
   :edit (fn [zloc]
           (-> zloc
               (r.zip/left*)
               (r.zip/remove*)))})

(def indent
  {:pred (fn [zloc]
           (r.zip/linebreak? zloc))
   :edit (fn [zloc]
           (println "KITERU?" (r.zip/tag zloc)
                    "unindent" (-> zloc (u.indent/unindent) (r.zip/tag)))

           (-> zloc
               (u.indent/unindent)
               (u.indent/indent)))})

(defn matching-rule
  [zloc rules]
  (some (fn [{:as rule :keys [pred]}]
          (when (pred zloc)
            rule))
        rules))

(defn prewalk* [zloc f]
  (loop [zloc zloc]
    (cond
      (r.zip/end? zloc)
      zloc

      :else
      (recur (r.zip/next* (f zloc))))))

(defn- apply-rules
  [node rules]
  (-> node
      (r.zip/of-node* {:track-position? true})
      (prewalk* #(if-let [{:keys [edit]} (matching-rule % rules)]
                   (edit %)
                   %))
      (r.zip/root)))

;; (defn- apply-rules2
;;   [zloc rules]
;;   (-> (u.zip/move-to-root zloc)
;;       (prewalk* #(do
;;                    (println "FIXME" (r.zip/tag %))
;;                    (if-let [{:keys [edit]} (matching-rule % rules)]
;;                      (edit %)
;;                      %)))))
