## Algorithms for The Little Clojurer

I was not fortunate enough (some would say unforunate enough) to learn Lisp/Scheme in college.  It has been on my list for years and when Clojure came along and started getting popular, I decided it was time.

Douglas Crockford says in <i>JavaScript: The Good Parts</i> that all computer scientsts should read <i>The Little Schemer</i> to truly understand recursion and how to think recursively.

Not being content to do one thing at a time, I decided to combine this reading with learning the modern Lisp-on-the-JVM, Clojure.  So I read <i>The Little Schemer</i> and every time it challenged you to write the algorithm, I did so in Clojure.

This repo has all the algorithms in <i>The Little Schemer</i> done in Clojure, using only Clojure's lists and the core methods of ISeq: <code>cons, first (car), rest (cdr), number?</code> and the predicate method <code>list?</code>.  Everything else was implemented from first principles according to the teachings of <i>The Little Schemer</i>.

The algorithms are in the file *littleclj.clj*.  I also write simple assert tests for them in *littleclj-test.clj*.  At the time I wrote these I didn't know how to use Clojure's assert method nor any unit testing framework, so I wrote my own poor-man's version.

This was a great way to learn the subset of Clojure required to implement core Lisp/Scheme style functions.  I would like to do the same exercise in Scala, which I'm also in the middle of learning.

Of course, there is a sequel to <i>Little Schemer</i>: <i>The Seasoned Schemer</i>.  Perhaps I will tackle that one as well?

Author :  Michael Peterson
Date   :  Oct-Nov 2011
