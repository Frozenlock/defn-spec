# defn-spec

A Clojure(Script) wrapper around `defn` that adds checking to a function's args and/or return value via assertions.

This fork of [defn-spec](https://github.com/Provisdom/defn-spec) **always** checks funtion's args and/or return value.
It also disallows any provided metadata keys that aren't `ret` and `args` to avoid typos.

[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.frozenlock/defn-spec.svg)](https://clojars.org/org.clojars.frozenlock/defn-spec)


## Usage

You use `defn-spec` in the same way you would use `defn`:

```clojure
(require '[clojure.spec.alpha :as s])
(require '[defn-spec.core :as ds])

(ds/defn-spec my-inc
  [x]
  (inc x))
```

This time with specs:

```clojure
(ds/defn-spec my-inc
  {::ds/args (s/cat :x int?)
   ::ds/ret  nat-int?}
  [x]
  (inc x))
```

## Tests

- Clojure: `lein test`
- Clojurescript: `lein doo <js-env>` (ex: `lein doo phantom`)

## License

Copyright © 2018 Provisdom

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
