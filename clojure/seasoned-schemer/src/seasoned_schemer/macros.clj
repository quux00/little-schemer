(ns seasoned-schemer.macros
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
