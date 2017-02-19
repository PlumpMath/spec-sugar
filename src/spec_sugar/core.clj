(ns spec-sugar.core
  (:require [clojure.spec :as s]
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
            :varargs (s/? (s/cat :amp #{'&} :form ::binding-form)))))
)

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

(s/def ::spec is-spec?)

(s/def ::separator #{:-})

(s/def ::typed-binding-form
  (s/cat :form ::cs/binding-form
         :separator ::separator
         :type ::spec))

(s/def ::maybe-typed-args
  (s/* (s/alt :untyped ::cs/binding-form :typed ::typed-binding-form)))

(s/def ::args+body
  (s/cat :args (s/and vector? (s/spec ::maybe-typed-args))
         :prepost (s/? map?)
         :body (s/* any?)))

(s/def ::bodies
  (s/alt :arity-1 ::args+body
         :arity-n (s/cat :bodies (s/+ (s/spec ::args+body))
                         :attr (s/? map?))))

(s/def ::defn-args
  (s/cat :name simple-symbol?
         :ret (s/? (s/cat :sep ::separator :ret-spec ::spec))
         :docstring (s/? string?)
         :meta (s/? map?)
         :bs ::bodies))

(comment
  (s/fdef defns
          :ret any?
          :args (s/cat :name simple-symbol?
                       :ret-spec ::spec
                       :meta (s/? map?)
                       :bs )))

(defmacro defns [& args]
  #_(let [args (s/conform ::cs/defn-args args)])
  (def -args args)
  (comment
    (clojure.spec/fdef (:name args)
                       :ret
                       :args (s/cat ))
    )

  (list 'clojure.core/defn
        (:name args)
        (:ret-spec args)
        (:docstring args)
        (:meta args)))

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
