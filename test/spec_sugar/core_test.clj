(ns spec-sugar.core-test
  (:require [clojure.test :refer :all]
            [spec-sugar.core :as ss]
            [clojure.spec :as s]))

(deftest typed-args
  (testing "We can parse arguments that are typed"
    (are [x] (s/valid? ::ss/typed-binding-form x)
      '[x :- integer?]
      '[x :- boolean?]
      '[xs :- (s/* integer?)]
      '[x :- ::int]
      '[{:keys [a b]} :- (s/* integer?)])
    (are [x] (not (s/valid? ::ss/typed-binding-form x))
      '[1]
      '[x]
      '[x :- :a]
      '[x :-])))
