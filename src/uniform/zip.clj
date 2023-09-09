(ns uniform.zip
  (:require
    [rewrite-clj.node :as r.node]
    [rewrite-clj.zip :as r.zip]))

(def ^:private tag-prefix-map
  {:deref "@"
   :fn "#("
   :list "("
   :map "{"
   :meta "^"
   :meta* "#^"
   :namespaced-map "#"
   :quote "'"
   :reader-macro "#"
   :set "#{"
   :forms ""
   :syntax-quote "`"
   :uneval "#_"
   :unquote "~"
   :unquote-splicing "~@"
   :var "#'"
   :vector "["})

(defn tag-prefix
  [zloc]
  (let [tag (r.zip/tag zloc)]
    (tag-prefix-map tag)))

(defn expression?
  [zloc]
  (and
    zloc
    (not (r.zip/whitespace-or-comment? zloc))
    (not (r.zip/linebreak? zloc))))

(defn comma?
  [zloc]
  (-> zloc
      (r.zip/node)
      (r.node/tag)
      (= :comma)))

(defn whitespace?
  [zloc]
  (= :whitespace
     (r.zip/tag zloc)))

(defn comment?
  [zloc]
  (= :comment
     (r.zip/tag zloc)))

(defn node-str
  [zloc]
  (r.node/string (r.zip/node zloc)))

(defn previous-line
  [zloc]
  (loop [zloc zloc
         buf '()]
    (let [left-zloc (r.zip/left* zloc)
          up-zloc (r.zip/up zloc)
          prev-zloc (or left-zloc up-zloc)
          tag-str (tag-prefix up-zloc)]
      (cond
        (or (not prev-zloc)
            (r.zip/linebreak? prev-zloc))
        (apply str buf)

        left-zloc
        (recur left-zloc (conj buf (node-str left-zloc)))

        (and up-zloc tag-str)
        (recur up-zloc (conj buf tag-str))

        :else
        (recur up-zloc (conj buf (node-str up-zloc)))))))

(defn quoted-list?
  [zloc]
  (let [up-zloc (r.zip/up zloc)]
    (and
      (= :list (some-> up-zloc (r.zip/tag)))
      (= :quote (some-> up-zloc (r.zip/up) (r.zip/tag))))))
