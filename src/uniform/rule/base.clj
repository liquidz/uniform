(ns uniform.rule.base
  (:require
    [clojure.string :as str]
    [rewrite-clj.node :as r.node]
    [rewrite-clj.zip :as r.zip]
    [rewrite-clj.zip.whitespace :as r.z.whitespace]
    [uniform.util.zip :as u.u.zip]))

(def missing-whitespace
  {:pred (fn [zloc]
           (and (u.u.zip/expression? zloc)
                (u.u.zip/expression? (r.zip/right* zloc))))
   :edit (fn [zloc]
           (r.z.whitespace/insert-space-right zloc))})

(def too-many-spaces
  {:pred (fn [zloc]
           (and (u.u.zip/whitespace? zloc)
                (not (r.zip/linebreak? (r.zip/left* zloc)))
                (not (u.u.zip/comment? (r.zip/right* zloc)))
                (> (count (r.zip/string zloc)) 1)))
   :edit (fn [zloc]
           (if (and (u.u.zip/expression? (r.zip/left* zloc))
                    (u.u.zip/expression? (r.zip/right* zloc)))
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

(def hard-tab-to-space
  "
  - Tabs vs Spaces
    - https://guide.clojure.style/#spaces
  "
  {:pred (fn [zloc]
           (and
             (u.u.zip/whitespace? zloc)
             (str/includes? (r.zip/string zloc) "\t")))
   :edit (fn [zloc]
           (->
             zloc
             (r.zip/remove*)
             (r.z.whitespace/insert-space-right)))})

(def comma
  "
  - No Commas in Sequential Collection Literals
    - https://guide.clojure.style/#no-commas-for-seq-literals
  - Optional Commas in Map Literals
    - https://guide.clojure.style/#opt-commas-in-map-literals
  "
  {:pred (fn [zloc]
           (u.u.zip/comma? zloc))
   :edit (fn [zloc]
           (let [;; Delete spaces before commas
                 zloc (if (r.zip/whitespace? (r.zip/left* zloc))
                        (-> zloc
                            (r.zip/left*)
                            (r.zip/remove*)
                            (r.zip/right*))
                        zloc)
                 ;; Remove spaces after commas
                 zloc (if (u.u.zip/whitespace? (r.zip/right* zloc))
                        (-> zloc
                            (r.zip/right*)
                            (r.zip/remove*))
                        zloc)
                 ;; Remove commas if the next node is a linebreak
                 zloc (if (r.zip/linebreak? (r.zip/right* zloc))
                        (r.zip/remove* zloc)
                        zloc)
                 ;; Insert space after commas in maps
                 zloc (if (and
                            (u.u.zip/comma? zloc)
                            (u.u.zip/in-map? zloc))
                        (r.z.whitespace/insert-space-right zloc)
                        zloc)
                 ;; Replace commas with spaces outside of maps
                 zloc (if (and
                            (u.u.zip/comma? zloc)
                            (not (u.u.zip/in-map? zloc)))
                        (r.zip/replace* zloc (r.node/whitespace-node " "))
                        zloc)]
             zloc))})
