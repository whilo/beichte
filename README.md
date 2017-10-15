# beichte

*Formalize and enforce purity staticly without a type system.*

Confess (German: beichten) your sins and expose your side-effects. Beichte will
track down you Clojure source code recursively and examine all accesses to vars
statically with [tools.analyzer](https://github.com/clojure/tools.analyzer).
Access to variables that it does not know by a whitelist will taint all access
to vars depending on the access and so on.

## Usage

~~~clojure
(require '[beichte.core :refer [impure? pure-ground]]
         '[clojure.tools.analyzer.jvm :as jvm])

(impure? (jvm/analyze '(map (fn [x] (+ 1 x)) [1 2 (inc 3)]) (jvm/empty-env)))
;; => nil, pure :D

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(impure? (jvm/analyze '(foo 42) (jvm/empty-env)))
;; => {;; deeply nested analysis stack finds problematic method call:
       {:problem :unsafe-instance-call, :class java.io.Writer, :method append, :form (. *out* (append system-newline))}}}
	   
@pure-ground
;; => dictionary entries of known purity of functions, currently also cache
;; you can add entries, effectively marking them as pure, be careful though...
~~~

## TODO

- separate cache from ground truth
- integrate as linter, e.g. in eastwood
  https://github.com/jonase/eastwood/tree/master/src/eastwood/linters
- investigate tools.decompiler as fallback if we cannot find the source

## License

Copyright © 2017 Christian Weilbach

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
