(ns slide.core-test
  (:require [clojure.test :refer :all]
            [slide.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(deftest images
  (testing "slide.images/getFiles"
    (is (= (slide.images/getFiles
            "/Users/pwv/a/photos/2015/20150704DeRuyterNY") 6))))
