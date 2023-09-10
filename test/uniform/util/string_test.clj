(ns uniform.util.string-test
  (:require
    [clojure.test :as t]
    [uniform.util.string :as sut]))

(t/deftest width-test
  (t/is (= 0  (sut/width "")))
  (t/is (= 15 (sut/width "hello lispworld")))
  (t/is (= 15 (sut/width "こんにちは 世界")))
  (t/is (= 15 (sut/width "helloこんにちは")))
  (t/is (= 15 (sut/width "surrogatepair𩸽"))))
