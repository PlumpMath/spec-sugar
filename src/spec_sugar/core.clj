(ns spec-sugar.core
  (:require [clojure.spec :as s]
            [clojure.walk :as w]
            [clojure.core.specs :as cs]))

(comment
  ;; Reference

  (s/def ::local-name (s/and simple-symbol? #(not= '& %)))

  (s/def ::binding-form
    (s/or :sym ::local-name
          :seq ::seq-binding-form
          :map ::map-binding-form))

  (s/def ::arg-list
    (s/and
     vector?
     (s/cat :args (s/* ::binding-form)
            :varargs (s/? (s/cat :amp #{'&} :form ::binding-form))))))

(defn is-spec?
  "returns true if spec-or-k is a spec, predicate, regex or resolvable kw/sym"
  [spec-or-k]
  (boolean (or (and (ident? spec-or-k)
                    (and (keyword? spec-or-k) (some? (namespace spec-or-k))))
               (s/spec? spec-or-k)
               (s/regex? spec-or-k)
               (symbol? spec-or-k)
               (try
                 (s/spec spec-or-k)
                 (catch Exception _ false)))))

(defn vector-spec
  "Create a spec that it is a vector and other conditions and unforms to a vector.

  Ex (vector-spec (s/spec ::binding-form))
      (vector-spec (s/* integer?))"
  [form]
  (let [s (s/spec (s/and vector? form))]
    (reify
      s/Specize
      (specize* [_] s)
      (specize* [_ _] s)

      s/Spec
      (conform* [_ x] (s/conform* s x))
      (unform* [_ x] (vec (s/unform* s x)))
      (explain* [_ path via in x] (s/explain s path via in x))
      (gen*  [_ overrides path rmap] (s/gen* s overrides path rmap))
      (with-gen* [_ gfn] (s/with-gen s gfn))
      (describe* [_] (s/describe* s)))))

(s/def ::spec is-spec?)

(s/def ::separator #{:-})

(s/def ::typed-binding-form
  (s/cat :form ::cs/binding-form
         :separator ::separator
         :type ::spec))

(s/def ::maybe-typed-args
  (s/* (s/alt :untyped ::cs/binding-form :typed ::typed-binding-form)))

(s/def ::args+body
  (s/cat :args (vector-spec (s/spec ::maybe-typed-args))
         :prepost (s/? map?)
         :body (s/* any?)))

(s/def ::unform-args+body
  (s/cat :args identity
         :prepost (s/? map?)
         :body (s/* any?)))

(s/def ::bodies
  (s/alt :arity-1 ::args+body
         :arity-n (s/cat :bodies (s/+ (s/spec ::args+body))
                         :attr (s/? map?))))

(s/def ::defn-args
  (s/cat :name simple-symbol?
         :ret-type (s/? (s/cat :sep ::separator :ret-spec ::spec))
         :docstring (s/? string?)
         :meta (s/? map?)
         :bs ::bodies))

(defn untype-arg
  "Takes arg:
  [:untyped [:sym 'y]]

  [:typed {:form [:sym 'x], :separator :-, :type :spec-sugar.core-test/int}]

  and returns the :untyped version"
  [arg]
  (case (first arg)
    :typed [:untyped (get-in arg [1 :form])]
    arg))

(defn untype-all
  "Walks a conformed value and removes any types it finds"
  [all-args]
  (w/prewalk #(cond
                (and (map? %) (contains? % :args)) (update % :args (partial mapv untype-arg))
                (and (map? %) (contains? % :ret-type)) (dissoc % :ret-type)
                :else %)
             all-args))

(s/fdef defns
        :ret any?
        :args ::defn-args)

(defmacro defns [& args]
  (let [parsed-args (s/conform ::defn-args args)]
    ;; XXX shuold collect the types and do something with them
    (remove nil?
            (list 'clojure.core/defn
                  (:name parsed-args)
                  (:docstring parsed-args)
                  (:meta parsed-args)
                  (s/unform ::bodies (untype-all (:bs parsed-args)))))))

(comment

  (s/conform
   ::cs/defn-args

   ['foo "fasdf" '[x] '(+ x 1)]
   )

  (s/def ::int integer?)

  (defns add [x y]
    (+ x y))

  (defns add :- integer?
    "A docstring"
    [x ;; nothing
     y :- ::int]
    (+ x y))

  )
