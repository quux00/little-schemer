(ns seasoned-schemer.helpers
  (:require [clojure.walk :as walk]))

;; From https://gist.github.com/486880
(defmacro letrec [bindings & body]
  (let [bcnt (quot (count bindings) 2)
        arrs (gensym "bindings_array")
        arrv `(make-array Object ~bcnt)
        bprs (partition 2 bindings)
        bssl (map first bprs)
        bsss (set bssl)
        bexs (map second bprs)
        arrm (zipmap bssl (range bcnt))
        btes (map #(walk/prewalk (fn [f]
                                   (if (bsss f)
                                     `(aget ~arrs ~(arrm f))
                                     f))
                                 %)
                  bexs)]
    `(let [~arrs ~arrv]
       ~@(map (fn [s e]
                `(aset ~arrs ~(arrm s) ~e))
              bssl
              btes)
       (let [~@(mapcat (fn [s]
                         [s `(aget ~arrs ~(arrm s))])
                       bssl)]
         ~@body))))

(comment
  
    (letrec [ev? (fn [n] (if (zero? n) true (od? (dec n))))
           od? (fn [n] (if (zero? n) false (ev? (dec n))))]
    (ev? 11))

  ;; ...and the same with 10

  (letrec [xs (lazy-seq (filter even? ys)) ys (range 10)] xs)

  ;; see http://stackoverflow.com/questions/2761253/graph-representation-in-dr-scheme/2761677#2761677
  (letrec [NY (list "NY" (delay [London Paris]))
           Paris (list "Paris" (delay [NY]))
           London (list "London" (delay [NY]))]
    (ffirst @(first (nfirst @(second NY)))))

  )


;; From clojure-contrib.monads by Konrad Hinsen
;; https://github.com/clojure/clojure-contrib/blob/master/modules/monads/src/main/clojure/clojure/contrib/monads.clj
;; TODO: not sure how to use this - probably need to understand the entire monad module
;;       to understand how to "return a continuation" from the function you pass to call-cc
;; I tried, but failed, using it with the example code from the wikipedia:
;;   http://en.wikipedia.org/wiki/Call-with-current-continuation
;;   and didn't understand the example code provided with clojure-contrib.monad
(defn call-cc
  "A computation in the cont monad that calls function f with a single
   argument representing the current continuation. The function f should
   return a continuation (which becomes the return value of call-cc),
   or call the passed-in current continuation to terminate."
  [f]
  (fn [c]
    (let [cc (fn cc [a] (fn [_] (c a)))
          rc (f cc)]
      (rc c))))
