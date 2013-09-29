#Overview

## Algorithms for The Little Schemer and The Seasoned Schemer

I was not fortunate enough (some would say unfortunate enough) to learn Lisp/Scheme in college.  It has been on my list for years and when Clojure came along and started getting popular, I decided it was time.

Douglas Crockford says in _JavaScript: The Good Parts_ that all computer scientists should read _The Little Schemer_ to truly understand recursion and how to think recursively.

Not being content to do one thing at a time, I decided to combine this reading with learning the modern Lisp-on-the-JVM, Clojure.  So I read _The Little Schemer_ and every time it challenged you to write the algorithm, I did so in Clojure.

About half way through, I started doing it in Scala as well, which I am also in the early stages of learning - trying to figure out how to do functional programming in Scala.

After completing _The Little Schemer_, I started on _The Seasoned Schemer_ (TSS). I did TSS only in Clojure, this time using Leiningen and trying to write in more idiomatic Clojure (as much as I can since I'm still learning what's idiomatic).

### Clojure Notes - The Little Schemer

This repo has all the algorithms in _The Little Schemer_ done in Clojure, using only Clojure's lists and the core methods of ISeq: <code>cons, first (car), rest (cdr), number?, list?</code> and in later chapters the built-in <code>=</code> method, since it is multi-purpose for multiple datatypes and, also in later chapters, the arithmetic operators.  Everything else was implemented from first principles according to the teachings of _The Little Schemer_.

The algorithms are in the file **littleclj.clj**.  I also write simple assert tests for them in **littleclj-test.clj**.  At the time I wrote these I didn't know how to use Clojure's assert method nor any unit testing framework, so I wrote my own poor-man's version.

This code will work with the standard Clojure REPL - I've tried it with Clojure 1.1, 1.2 and 1.3 and all work.


### Scala Notes - The Little Schemer

Doing this in Scala is harder because it is not a Lisp and has strong typing, so handling the typing has been interesting, but helping me grok Scala.  I don't claim this is at all idiomatic Scala.  Idiot Scala, maybe.

For now I've implemented very simple assert functions after how I did it in Clojure.  I need to learn ScalaTest and port my tests to that...

The single .scala file is meant to be run through the Scala REPL, since Little Schemer functions and tests are all in this one file.


### The Seasoned Schemer (TSS)

I did TSS as a [Leiningen](https://github.com/technomancy/leiningen) project, so look in the clojure/seasoned-schemer directory for the standard leiningen files and dirs. In order to do chapter 12's <code>letrec</code> exercises I downloaded a [letrec Clojure macro](https://gist.github.com/486880) written by Michal Marczyk. The only change I made was to put it into namespace - it is in the src/seasoned_schemer/macros.clj file.

This time I used clojure.test (the default for a Leiningen project) to write all the unit tests. You can run them with <code>lein run</code> or load it in the repl (<code>lein repl</code>) by doing:

<pre>
$ cd clojure/seasoned-schemer
$ lein repl  
seasoned-schemer.core=> (load-file "test/seasoned_schemer/test/core.clj")  
seasoned-schemer.core=> (use 'seasoned-schemer.test.core)  
seasoned-schemer.core=> (rt)  ;; I've added this to run all the tests, reloading  
                              ;; the files/namespaces into the repl to catch new changes  
</pre>

## Status

Author         :  Michael Peterson  
Last updated   :  Dec 2011
