(ns spec-sugar.core
  (:require [clojure.spec :as s]
            [clojure.core.specs :as cs]))

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

(s/def ::typed-binding-form
  (s/cat :form ::cs/binding-form
         :separator #{:-}
         :type ::spec))

(s/def ::maybe-typed-args
  (s/* (s/alt :untyped ::cs/binding-form :typed ::typed-binding-form)))

(comment


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

  (s/def ::args+body
    (s/cat :args ::arg-list
           :prepost (s/? map?)
           :body (s/* any?))))

(s/def ::cs/defn-args
  (s/cat :name simple-symbol?
         :docstring (s/? string?)
         :meta (s/? map?)
         :bs (s/alt :arity-1 ::cs/args+body
                    :arity-n (s/cat :bodies (s/+ (s/spec ::cs/args+body))
                                    :attr (s/? map?)))))

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
