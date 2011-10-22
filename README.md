#Overview

## Algorithms for The Little Schemer

I was not fortunate enough (some would say unfortunate enough) to learn Lisp/Scheme in college.  It has been on my list for years and when Clojure came along and started getting popular, I decided it was time.

Douglas Crockford says in _JavaScript: The Good Parts_ that all computer scientsts should read _The Little Schemer_ to truly understand recursion and how to think recursively.

Not being content to do one thing at a time, I decided to combine this reading with learning the modern Lisp-on-the-JVM, Clojure.  So I read _The Little Schemer_ and every time it challenged you to write the algorithm, I did so in Clojure.

About half way through, I started doing it in Scala as well, which I am also in the early stages of learning - trying to figure out how to do functional programming in Scala.


### Clojure Notes

This repo has all the algorithms in _The Little Schemer_ done in Clojure, using only Clojure's lists and the core methods of ISeq: <code>cons, first (car), rest (cdr), number?, list?</code> and in later chapters the built-in <code>=</code> method, since it is multi-purpose for multiple datatypes and, also in later chapters, the arithmetic operators.  Everything else was implemented from first principles according to the teachings of _The Little Schemer_.

The algorithms are in the file **littleclj.clj**.  I also write simple assert tests for them in **littleclj-test.clj**.  At the time I wrote these I didn't know how to use Clojure's assert method nor any unit testing framework, so I wrote my own poor-man's version.

I run all this code in the Clojure repl that came with Stuart Halloway's *Programming Clojure*.  So it was developed using Clojure 1.1.  **TODO: need to test with 1.2 and 1.3**

This was a great way to learn the subset of Clojure required to implement core Lisp/Scheme style functions.  I would like to do the same exercise in Scala, which I'm also in the middle of learning.

Of course, there is a sequel to _The Little Schemer_ - _The Seasoned Schemer_.  Perhaps I will tackle that one as well?

### Scala Notes

Doing this in Scala is harder because it is not a Lisp and has strong typing, so handling the typing has been interesting, but helping me grok Scala.  I don't claim this is at all idiomatic Scala.  Idiot Scala, maybe.

For now I've implemented very simple assert functions after how I did it in Clojure.  I need to learn ScalaTest and port my tests to that...

The single .scala file is meant to be run through the scala REPL, since Little Schemer functions and tests are all in this one file.

## Status

Currently, I have finished up through chapter 8 of with Clojure and in the middle of chapter 6 for Scala.  More coming soon.

Author :  Michael Peterson<br />
Date   :  Oct-Nov 2011
