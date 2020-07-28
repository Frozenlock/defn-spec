(ns defn-spec.core
  (:require [clojure.spec.alpha :as s]
            [defn-spec.defn-args :as defn-args]))

(defn assert*
  [kind fn-name spec x]
  (when (not (s/valid? spec x))
    (let [ed (s/explain-data spec x)]
      (throw (ex-info
              (str
               (case kind
                 :args (str "Call to "
                            fn-name
                            " did not conform to spec:")
                 :ret (str "Return value from "
                           fn-name
                           " did not conform to spec:"))
               "\n"
               (with-out-str (s/explain-out ed)))
              ed)))))

(defn unform-to-arglists
  [bs]
  (let [[arity-type conformed-bodies] bs]
    (case arity-type
      :arity-1 (list (s/unform ::defn-args/arg-list (:args conformed-bodies)))
      :arity-n (map #(s/unform ::defn-args/arg-list (:args %))
                    (:bodies conformed-bodies)))))

#?(:clj
   (defn qualify-symbol
     [sym-name]
     (symbol (str *ns*) (str sym-name))))

;; From https://github.com/borkdude/speculative/blob/master/src/speculative/impl.cljc
#?(:clj (defmacro ?
          [& {:keys [cljs clj]}]
          (if (contains? &env '&env)
            `(if (:ns ~'&env) ~cljs ~clj)
            (if #?(:clj (:ns &env) :cljs true)
              cljs
              clj))))

#?(:clj
   (defn- defn-spec-form
     [args source]
     (let [{:keys [name docstring meta bs]} (s/conform ::defn-args/defn-args args)
           qualified-name (qualify-symbol name)
           args-spec (::args meta)
           ret-spec (::ret meta)
           body (let [[arity-type conformed-bodies] bs]
                  (case arity-type
                    :arity-1 (s/unform ::defn-args/args+body conformed-bodies)
                    :arity-n (mapv (partial s/unform ::defn-args/args+body) (:bodies conformed-bodies))))
           inner-fn-name (symbol (str name "-impl"))
           args-sym (gensym "args")
           result-sym (gensym "result")]
       `(do (defn ~name
              ~@(when docstring [docstring])
              ~@(when meta [meta])
              [& ~args-sym]
              ~@(when args-spec [`(assert* :args '~qualified-name ~args-spec ~args-sym)])
              (let [~result-sym (apply (fn ~inner-fn-name ~@body) ~args-sym)]
                ~@(when ret-spec [`(assert* :ret '~qualified-name (s/spec ~ret-spec) ~result-sym)])
                ~result-sym))

            (s/fdef ~name
              ~@(when args-spec [:args args-spec])
              ~@(when ret-spec [:ret ret-spec]))

            (alter-meta! (var ~(symbol name)) merge {:arglists '~(unform-to-arglists bs)} ~source)

            (? :clj
               (var-get (var ~(symbol name)))

               :cljs
               (var ~(symbol name)))))))

#?(:clj
   (defmacro defn-spec
     "Exact same parameters as `defn`. You may optionally include `::args`
     and/or `::ret` in your function's attr-map to have the args and/or return
     value of your function checked with `s/assert`."
     {:arglists '([name doc-string? attr-map? [params*] prepost-map? body]
                  [name doc-string? attr-map? ([params*] prepost-map? body) + attr-map?])}
     [& args]
     (defn-spec-form args (meta &form))))

#?(:clj
   (s/fdef defn-spec
     :args ::defn-args/defn-args
     :ret any?))
