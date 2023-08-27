(ns uniform.indent-test
  (:require
    [clojure.test :as t]
    [rewrite-clj.zip :as r.zip]
    [uniform.indent :as sut]
    [uniform.test-helper :as h]
    [uniform.zip :as u.zip]))

(t/deftest unindent-test
  (let [unindent #(-> (h/parse %)
                      (sut/unindent)
                      (h/root-str))]
    (t/is (= "" (unindent "")))
    (t/is (= "(foo)" (unindent "(foo)")))
    (t/is (= "(foo)" (unindent "  (foo)"))))

  (t/testing "zloc position"
    (let [zloc (-> (h/parse "foo\nbar")
                   (h/next-linebreak)
                   (sut/unindent))]
      (t/is (r.zip/linebreak? zloc))
      (t/is (= "foo\nbar" (h/root-str zloc))))

    (let [zloc (-> (h/parse "foo\n   bar")
                   (h/next-linebreak)
                   (sut/unindent))]
      (t/is (r.zip/linebreak? zloc))
      (t/is (= "foo\nbar" (h/root-str zloc))))))


(t/deftest indent-form-test
  (t/is (= "(foo\n  bar)"
           (->> (h/parse "(foo\nbar)")
                (h/next-linebreak)
                (sut/indent)
                (h/root-str))))
  (t/is (= "(foo\n  (bar\n    baz))"
           (-> (h/parse "(foo\n  (bar\nbaz))")
               (h/next-linebreak)
               (h/next-linebreak)
               (sut/indent)
               (h/root-str))))

  (t/testing "leftmost is not token"
    (t/is (= "([]\n foo)"
             (->> (h/parse "([]\nfoo)")
                  (h/next-linebreak)
                  (sut/indent)
                  (h/root-str))))
    (t/is (= "((fn [v] v)\n foo)"
             (->> (h/parse "((fn [v] v)\nfoo)")
                  (h/next-linebreak)
                  (sut/indent)
                  (h/root-str)))))

  (t/testing "nested"
    (t/is (= "(foo (bar)\n  baz)"
             (-> (h/parse "(foo (bar)\nbaz)")
                 (h/next-linebreak)
                 (sut/indent)
                 (h/root-str))))))

(t/deftest indent-vector-test
    (t/is (= "[foo\n bar]"
             (->> (h/parse "[foo\nbar]")
                  (h/next-linebreak)
                  (sut/indent)
                  (h/root-str))))
    (t/is (= "[foo\n bar\n baz]"
             (->> (h/parse "[foo\n bar\nbaz]")
                  (h/next-linebreak)
                  (h/next-linebreak)
                  (sut/indent)
                  (h/root-str))))

    (t/testing "nested"
      (t/is (= "[[foo]\n bar]"
               (->> (h/parse "[[foo]\nbar]")
                    (h/next-linebreak)
                    (sut/indent)
                    (h/root-str))))
      (t/is (= "[[foo\n  bar]]"
               (->> (h/parse "[[foo\nbar]]")
                    (h/next-linebreak)
                    (sut/indent)
                    (h/root-str))))))

(t/deftest indent-set-test
    (t/is (= "#{foo\n  bar}"
             (->> (h/parse "#{foo\nbar}")
                  (h/next-linebreak)
                  (sut/indent)
                  (h/root-str))))
    (t/is (= "#{foo\n  bar\n  baz}"
             (->> (h/parse "#{foo\n  bar\nbaz}")
                  (h/next-linebreak)
                  (h/next-linebreak)
                  (sut/indent)
                  (h/root-str))))

    (t/testing "nested"
      (t/is (= "#{#{foo}\n  bar}"
               (->> (h/parse "#{#{foo}\nbar}")
                    (h/next-linebreak)
                    (sut/indent)
                    (h/root-str))))
      (t/is (= "#{#{foo\n    bar}}"
               (->> (h/parse "#{#{foo\nbar}}")
                    (h/next-linebreak)
                    (sut/indent)
                    (h/root-str))))))

(t/deftest indent-map-test
  (t/is (= "{foo\n bar}"
           (->> (h/parse "{foo\nbar}")
                (h/next-linebreak)
                (sut/indent)
                (h/root-str))))

  (t/is (= "{foo\n bar\n baz}"
           (->> (h/parse "{foo\n bar\nbaz}")
                (h/next-linebreak)
                (h/next-linebreak)
                (sut/indent)
                (h/root-str))))

  (t/testing "meta map"
    (t/is (= "^{foo\n  bar} (form)"
             (->> (h/parse "^{foo\nbar} (form)")
                  (h/next-linebreak)
                  (sut/indent)
                  (h/root-str))))

    (t/is (= "^{foo\n  bar\n  baz} (form)"
             (->> (h/parse "^{foo\n  bar\nbaz} (form)")
                  (h/next-linebreak)
                  (h/next-linebreak)
                  (sut/indent)
                  (h/root-str))))))

(t/deftest indent-lambda-test
  (t/is (= "#(foo\n   bar)"
           (->> (h/parse "#(foo\nbar)")
                (h/next-linebreak)
                (sut/indent)
                (h/root-str))))
  (t/is (= "#(foo\n   (bar\n     baz))"
           (-> (h/parse "#(foo\n   (bar\nbaz))")
               (h/next-linebreak)
               (h/next-linebreak)
               (sut/indent)
               (h/root-str)))))

(t/deftest indent-quote-test
  (t/is (= "'(foo\n  bar)"
           (->> (h/parse "'(foo\nbar)")
                (h/next-linebreak)
                (sut/indent)
                (h/root-str))))
  (t/is (= "'[foo\n  bar]"
           (->> (h/parse "'[foo\nbar]")
                (h/next-linebreak)
                (sut/indent)
                (h/root-str))))
  (t/is (= "'{foo\n  bar}"
           (->> (h/parse "'{foo\nbar}")
                (h/next-linebreak)
                (sut/indent)
                (h/root-str)))))

(t/deftest indent-unquote-test
  (t/is (= "~(foo\n   bar)"
           (->> (h/parse "~(foo\nbar)")
                (h/next-linebreak)
                (sut/indent)
                (h/root-str))))

  (t/testing "unquote-splicing"
    (t/is (= "~@(foo\n    bar)"
             (->> (h/parse "~@(foo\nbar)")
                  (h/next-linebreak)
                  (sut/indent)
                  (h/root-str))))))

(t/deftest indent-root-test
  (t/is (= "\n(foo)"
           (->> (h/parse "\n(foo)")
                (h/next-linebreak)
                (sut/indent)
                (h/root-str))))

  (t/is (= "(foo)\n(bar)"
           (->> (h/parse "(foo)\n(bar)")
                (h/next-linebreak)
                (sut/indent)
                (h/root-str)))))
