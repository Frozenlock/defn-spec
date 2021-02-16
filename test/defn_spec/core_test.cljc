(ns defn-spec.core-test
  #?@
   (:clj
    [(:require
      [clojure.spec.alpha :as s]
      [clojure.test :refer [deftest is testing]]
      [defn-spec.core :as ds])]
    :cljs
    [(:require
      [cljs.spec.alpha :as s]
      [cljs.test :refer-macros [deftest is testing]]
      [defn-spec.core :as ds])
     (:require-macros [defn-spec.core-test :refer [is-error-thrown]])]))

#?(:clj
   (defn- cljs-env?
     "Take the &env from a macro, and tell whether we are expanding into cljs."
     [env]
     (boolean (:ns env))))

#?(:clj
   (defmacro is-exception-thrown
     "(is (thrown-with-msg? ...)) for specified exceptions in Clojure/ClojureScript."
     [clj-exc-class cljs-exc-class re expr]
     (let [is (if (cljs-env? &env) 'cljs.test/is 'clojure.test/is)
           exc-class (if (cljs-env? &env) cljs-exc-class clj-exc-class)]
       `(~is (~'thrown-with-msg? ~exc-class ~re ~expr)))))

#?(:clj
   (defmacro is-error-thrown
     "(is (thrown-with-msg? ...)) for general exceptions in Clojure/ClojureScript."
     [re expr]
     `(is-exception-thrown java.lang.Exception js/Error ~re ~expr)))

(ds/defn-spec arity-1-fn
  {::ds/args (s/cat :x int?)
   ::ds/ret  nat-int?}
  [x]
  (inc x))

(ds/defn-spec n-arity-fn
  "docstring"
  {::ds/args (s/cat :x int? :y (s/? int?))
   ::ds/ret  nat-int?}
  ([x] (n-arity-fn x 0))
  ([x y]
   (+ x y)))

(deftest test-function-calls
  (is (arity-1-fn 1))
  (testing "args vec is checked"
    (is-error-thrown #"did not conform to spec" (arity-1-fn "")))
  (testing "Return value is checked."
    (is-error-thrown #"did not conform to spec" (arity-1-fn -2)))

  (is (n-arity-fn 1))
  (is (n-arity-fn 1 2))
  (is (:doc (meta #'n-arity-fn)))
  (is-error-thrown #"did not conform to spec" (n-arity-fn 1 "2"))
  (is-error-thrown #"did not conform to spec" (n-arity-fn -1 0)))

#?(:clj
   (deftest disallowed-meta-keys
     (let [e (try (eval '(defn-spec.core/defn-spec my-fn
                           {:some-meta "data"}
                           [x] x))
                  (catch Exception e (.getCause e)))]
       (is (= "Only 'args' and 'ret' are allowed in provided metadata" (.getMessage e)))
       (is (= {:disallowed-keys '(:some-meta)} (ex-data e))))))
