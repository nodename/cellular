=======
cellular
========

A core.async implementation of Per Brinch Hansen,
["Parallel Cellular Automata: A Model Program for Computational Science"](http://surface.syr.edu/eecs_techreports/167) (1992).

### Usage

####Clojure:

See ASCII output by running
```
lein repl
```

```
(use 'utils.helpers)
```
and either
```
(use 'cellular.forestfire)
```
or
```
(use 'cellular.laplace)
```
and then (for example)
```
(print-all<! (simulate-forestfire 3 2 10))
```

#### ClojureScript:

Build:
```
lein cljx
lein cljsbuild once
```
then open resources/public/cellular.html in your browser.

## License

Copyright © 2013 Alan Shaw

Distributed under the Eclipse Public License, the same as Clojure.
