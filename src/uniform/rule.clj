(ns uniform.rule
  (:require
    [rewrite-clj.node :as r.node]
    [rewrite-clj.zip :as r.zip]
    [rewrite-clj.zip.whitespace :as r.z.whitespace]
    [uniform.indent :as u.indent]
    [uniform.zip :as u.zip]))

(def missing-whitespace
  {:pred (fn [zloc]
           (and (u.zip/expression? zloc)
                (u.zip/expression? (r.zip/right* zloc))))
   :edit (fn [zloc]
           (r.z.whitespace/insert-space-right zloc))})

(def too-many-spaces
  {:pred (fn [zloc]
           (and (u.zip/whitespace? zloc)
                (not (r.zip/linebreak? (r.zip/left* zloc)))
                (not (u.zip/comment? (r.zip/right* zloc)))
                (> (count (r.zip/string zloc)) 1)))
   :edit (fn [zloc]
           (if (and (u.zip/expression? (r.zip/left* zloc))
                    (u.zip/expression? (r.zip/right* zloc)))
             (r.zip/replace* zloc (r.node/whitespace-node " "))
             (r.zip/remove* zloc)))})

(def too-many-linebreaks
  {:pred (fn [zloc]
           (and (r.zip/linebreak? zloc)
                (or (> (count (r.zip/string zloc)) 1)
                    (nil? (r.zip/left* zloc)))))
   :edit (fn [zloc]
           (if (r.zip/left* zloc)
             (r.zip/replace* zloc (r.node/newlines 1))
             (r.zip/remove* zloc)))})

(def space-around-comma
  {:pred (fn [zloc]
           (u.zip/comma? zloc))
   :edit (fn [zloc]
           (cond-> zloc
             (r.zip/whitespace? (r.zip/left* zloc))
             (->
               (r.zip/left*)
               (r.zip/remove*))

             (not (r.zip/whitespace? (r.zip/right* zloc)))
             (r.z.whitespace/insert-space-right)

             (r.zip/linebreak? (r.zip/right* zloc))
             (r.zip/remove*)))})

(def indent
  {:pred (fn [zloc]
           (r.zip/linebreak? zloc))
   :edit (fn [zloc]
           (-> zloc
               (u.indent/unindent)
               (u.indent/indent)))})

;; ==================================================

(defn- matching-rule
  [zloc rules]
  (some (fn [{:as rule :keys [pred]}]
          (when (pred zloc)
            rule))
        rules))

(defn- prewalk* [zloc f]
  (loop [zloc zloc]
    (cond
      (r.zip/end? zloc)
      zloc

      :else
      (recur (r.zip/next* (f zloc))))))

(defn apply-rules
  [node rules]
  (-> node
      (r.zip/of-node* {:track-position? true})
      (prewalk* #(if-let [{:keys [edit]} (matching-rule % rules)]
                   (edit %)
                   %))
      (r.zip/root)))
