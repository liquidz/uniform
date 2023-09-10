(ns uniform.indent
  (:require
    [rewrite-clj.node :as r.node]
    [rewrite-clj.zip :as r.zip]
    [uniform.util.string :as u.u.str]
    [uniform.util.zip :as u.u.zip]))

(defn unindent
  [zloc]
  (loop [zloc zloc]
    (let [next-zloc (r.zip/next* zloc)]
      (if (u.u.zip/whitespace? next-zloc)
        (recur (r.zip/remove* next-zloc))
        zloc))))

(defn- indent*
  [zloc {:keys [offset]}]
  (let [n (-> zloc
              (r.zip/leftmost*)
              (u.u.zip/previous-line)
              (u.u.str/width))
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
    (cond
      (or
        ;; e.g. ([] ...), ((fn [] ...) ...)
        (and
          (= :list up-tag)
          (not= :token leftmost-tag))
        ;; '(...)
        (u.u.zip/in-quoted-list? zloc)
        ;; collections
        (contains? #{:vector :map :set} up-tag))
      (indent* zloc {:offset 0})

      (contains? #{:list :forms :fn} up-tag)
      (indent* zloc {:offset 1})

      :else
      zloc)))
