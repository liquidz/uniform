(ns uniform.rule
  (:require
    [rewrite-clj.zip :as r.zip]))

(defn- matching-rule
  [zloc rules]
  (some (fn [{:as rule :keys [pred]}]
          (when (pred zloc)
            rule))
        rules))

(defn- prewalk*
  [zloc f]
  (loop [zloc zloc]
    (cond
      (r.zip/end? zloc)
      zloc

      :else
      (recur (r.zip/next* (f zloc))))))

(defn apply-rules
  ([node rules]
   (apply-rules node identity rules))
  ([node f rules]
   (let [zloc (-> node
                  (r.zip/of-node* {:track-position? true}))]
     (r.zip/root
       (if-let [zloc' (f zloc)]
         (-> zloc'
             (prewalk* #(if-let [{:keys [edit]} (matching-rule % rules)]
                          (edit %)
                          %)))
         zloc)))))
