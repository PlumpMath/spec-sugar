(ns spec-sugar.core-test
  (:require [clojure.test :refer :all]
            [spec-sugar.core :as ss]
            [clojure.spec :as s]
            [clojure.core.specs :as cs]))

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

(deftest maybe-typed-args
  (testing "We can parse arguments that are typed"
    (are [x] (s/valid? ::ss/maybe-typed-args x)
      '[x :- integer?]
      '[x :- boolean?]
      '[xs :- (s/* integer?)]
      '[x :- ::int]
      '[{:keys [a b]} :- (s/* integer?)]
      '[x :- integer? y]
      '[x :- boolean? y :- integer?]
      '[xs :- (s/* integer?) y c d :- integer?]
      '[x :- ::int d :- ::int e f :- (s/* integer?)]
      '[{:keys [a b]} :- (s/* integer?) ops second-ops :- map?])
    (are [x] (not (s/valid? ::ss/maybe-typed-args x))
      '[1 x]
      '[x y :-]
      '[x :- :a y :-]
      '[x :-])))

(deftest defns-parsing
  (testing "We can parse full normal defn"
    (are [x] (s/valid? ::ss/defn-args x)
      '(add [x y] (+ x y))
      '(add "with-doc" [x y] (+ x y))
      '(add ([x y] (+ x y)) ([x y z] (+ x y z)))))
  (testing "and one with optional types"
    (are [x] (s/valid? ::ss/defn-args x)
      '(add :- integer?
            [x :- ::int y]
            (+ x y))
      '(add
            [x :- ::int y]
            (+ x y))
      '(add :- integer?
            [x y]
            (+ x y))
      '(add :- integer?
            "docstring"
            [x :- ::int y]
            (+ x y))
      '(add
            "docstring"
            [x :- ::int y]
            (+ x y))
      '(add :- integer?
            "docstring"
            [x :- ::int y :- integer?]
            (+ x y))
      '(add :- integer?
            "docstring"
            ([x :- ::int y :- integer?]
             (+ x y))
            ([x :- ::int y c]
             (+ x y)))
      '(add :- integer?
            "docstring"
            ([x :- ::int y :- integer?]
             (+ x y))))))

(deftest untype
  (testing "we can remove the types of any definitions"
    (are [x y] (= y (s/unform ::ss/defn-args (ss/untype-all (s/conform ::ss/defn-args x))))
      '(add :- integer?
            [x :- ::int y]
            (+ x y))
      '(add [x y] (+ x y))

      '(add
        [x :- ::int y]
        (+ x y))
      '(add [x y] (+ x y))

      '(add :- integer?
            [x y]
            (+ x y))
      '(add [x y] (+ x y))

      '(add :- integer?
            "docstring"
            [x :- ::int y]
            (+ x y))
      '(add "docstring" [x y] (+ x y))

      '(add
        "docstring"
        [x :- ::int y]
        (+ x y))
      '(add "docstring" [x y] (+ x y))

      '(add :- integer?
            "docstring"
            [x :- ::int y :- integer?]
            (+ x y))
      '(add "docstring" [x y] (+ x y)))))

(deftest collect-types
  (testing "We can extract the types from the form"
    (are [x y] (= y (ss/collect-types (s/conform ::ss/defn-args x)))
      '(add :- integer?
            [x :- ::int y]
            (+ x y))
      '{::ss/ret integer?
        ::ss/args [[::int any?]]}

      '(add
        [x :- ::int y]
        (+ x y))
      '{::ss/ret any?
        ::ss/args [[::int any?]]}

      '(add :- integer?
            [x y]
            (+ x y))
      '{::ss/ret integer?
        ::ss/args [[any? any?]]}

      '(add :- integer?
            "docstring"
            [x :- ::int y]
            (+ x y))
      '{::ss/ret integer?
        ::ss/args [[::int any?]]}

      '(add
        "docstring"
        [x :- ::int y]
        (+ x y))
      '{::ss/ret any?
        ::ss/args [[::int any?]]}

      '(add :- integer?
            "docstring"
            [x :- ::int y :- integer?]
            (+ x y))
      '{::ss/ret integer?
        ::ss/args [[::int integer?]]})))
