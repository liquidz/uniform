(ns uniform.indent
  (:require
    [rewrite-clj.node :as r.node]
    [rewrite-clj.zip :as r.zip]
    [uniform.string :as u.str]
    [uniform.zip :as u.zip]))

(defn unindent
  [zloc]
  (loop [zloc zloc]
    (let [next-zloc (r.zip/next* zloc)]
      (if (u.zip/whitespace? next-zloc)
        (recur (r.zip/remove* next-zloc))
        zloc))))

(defn- indent*
  [zloc {:keys [offset]}]
  (println "prev" (-> zloc
                      (r.zip/leftmost*)
                      (u.zip/previous-line))
           "tag" (r.zip/tag zloc)
           "node-str" (u.zip/node-str zloc))
  (let [n (-> zloc
              (r.zip/leftmost*)
              (u.zip/previous-line)
              (u.str/width))
        n (if (pos? n)
            (+ n offset)
            n)]
    (if (pos? n)
      (r.zip/insert-right* zloc (r.node/spaces n))
      zloc)))

(defn indent
  [zloc]
  (let [up-tag (some-> zloc (r.zip/up) (r.zip/tag))
        leftmost-tag (some-> zloc (r.zip/leftmost) (r.zip/tag))]
    (println "up-tag" up-tag "leftmost tag" leftmost-tag)
    (cond
      (or
        ;; e.g. ([] ...), ((fn [] ...) ...)
        (and
          (= :list up-tag)
          (not= :token leftmost-tag))
        ;; '(...)
        (u.zip/quoted-list? zloc)
        ;; collections
        (contains? #{:vector :map :set} up-tag))
      (indent* zloc {:offset 0})

      (contains? #{:list :forms :fn} up-tag)
      (indent* zloc {:offset 1})

      :else
      (do
        (println "up-tag" up-tag ", leftmost-tag" leftmost-tag)
        zloc))))
