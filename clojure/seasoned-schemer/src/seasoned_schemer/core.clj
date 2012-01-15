(ns seasoned-schemer.core
  (:use [seasoned-schemer.helpers])
  (:gen-class))

;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 11 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

;; two-in-a-row funcs done with actual recursion
(declare is-first?)

(defn two-in-a-row?-v1 [lat]
  (if (empty? lat)
    false
    (is-first? (first lat) (rest lat))))

(defn- is-first? [a lat]
  (or (= a (first lat))
      (two-in-a-row?-v1 lat)))

(declare two-in-a-row-b?)

(defn two-in-a-row? [lat]
  (two-in-a-row-b? (first lat) (rest lat)))

(defn- two-in-a-row-b? [a lat]
  (cond
   (empty? lat) false
   (= a (first lat)) true
   :else (two-in-a-row-b? (first lat) (rest lat))
   ))

;;two-in-a-row funcs done with Clojure recur
(declare two-in-a-row-recur-b?)

(defn two-in-a-row-recur? [lat]
  (two-in-a-row-recur-b? (first lat) (rest lat)))

(defn- two-in-a-row-recur-b? [a lat]
  (cond
   (empty? lat) false
   (= a (first lat)) true
   :else (recur (first lat) (rest lat))))

;; sum-of-prefixes
;; TODO: check if this one can be done with (Clojure's) map as well ...
(declare sum-of-prefixes-b)

(defn sum-of-prefixes [tup]
  (sum-of-prefixes-b 0 tup)
  )

(defn- sum-of-prefixes-b-ORIG [preceding tup]
  ;; doesn't work, comes out backwards => how use conj in a true recursion?
  ;; would forcing to vector each time do it (but then be less performant?)
  (conj (sum-of-prefixes-b (first tup) (rest tup)) (+ preceding (first tup)))
)

(defn- sum-of-prefixes-b [accum tup]
  (if (empty? tup)
    '()
    (cons (+ accum (first tup)) (sum-of-prefixes-b (+ accum (first tup)) (rest tup)))))

;; use loop/recur

;; Note that we do not have to create a delegate -b method to handle the two
;; args, since we can create a new param list in the loop assignment
(defn sum-of-prefixes-recur [tup]
  (loop [acctot 0 tup tup outvec []]
    (if (empty? tup)
      outvec
      (let [newtot (+ acctot (first tup))]
        (recur newtot
               (rest tup)
               (conj outvec newtot))
        )
      )
    )
  )

;;  sum-of-prefixes-b [1 2]
;; (conj (conj ([2]) (+ 1 2)) (+ 0 1))


;; scramble - lisp way (true recursion)
(declare pick)
(declare scramble-b)
(declare scramble-b-with-let)

(defn scramble [tup]
  (scramble-b-with-let tup [])
  )

(defn- scramble-b [tup rev-pre]
  (if (empty? tup)
    []
    (cons (pick (first tup) (cons (first tup) rev-pre))
          (scramble-b (rest tup) (cons (first tup) rev-pre))))
  )

(defn- scramble-b-with-let [tup rev-pre]
  (if (empty? tup)
    []
    (let [new-rev-pre (cons (first tup) rev-pre)]
      (cons (pick (first tup) new-rev-pre)
            (scramble-b (rest tup) new-rev-pre)))
    )
  )


;; scramble with Clojure recur

;; first version uses Lisp style lists, requiring a reverse on the
;; final result list
(defn scramble-recur-list [tup]
  (loop [tup tup rev-pre '() outvec '()]
    (if (empty? tup)
      (reverse outvec)
      (recur (rest tup)
             (cons (first tup) rev-pre)
             (cons (pick (first tup) (cons (first tup) rev-pre)) outvec)))
    )
  )

;; second version uses Clojure vectors, using conj on the vec to
;; avoid having to reverse the final collection
(defn scramble-recur-vec [tup]
  (loop [tup tup rev-pre [] outvec []]
    (if (empty? tup)
      outvec
      (recur (rest tup)
             (cons (first tup) rev-pre)  ; cons to get the prefix list in reverse order
             (conj outvec (pick (first tup) (cons (first tup) rev-pre))))
      )
    )
  )

(defn pick [n lat]
  {:pre [(> n 0) (<= n (count lat))]} ; Clojure pre-condition check
  (if (= 1 n)
    (first lat)
    (pick (dec n) (rest lat))
    ))

;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 12 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

;; Clojure does not have letrec, but michalmarczyk has created a letrec macro
;; for Clojure that may do the trick: https://gist.github.com/486880

;; first define the old style multirember
(defn multirember-ch3 [a lat]
  (cond
   (empty? lat) []
   (= a (first lat)) (multirember-ch3 a (rest lat))
   :else (cons (first lat) (multirember-ch3 a (rest lat)))
   )
  )

;; note that the recur version removes the need to repeat +a+
;; which seems the point of doing letrec in the SS book - does it
;; solve the problem entirely or just on the surface?
(defn multirember-ch3-recur [a lat]
  (loop [lat lat newlat []]
    (cond
     (empty? lat) newlat
     (= a (first lat)) (recur (rest lat) newlat)
     :else (recur (rest lat) (conj newlat (first lat)))
     )
    ))

(defn multirember-letrec [a lat]
  (letrec [mr (fn [lat]
                (cond
                 (empty? lat) []
                 (= a (first lat)) (mr (rest lat))
                 :else (cons (first lat) (mr (rest lat)))))]
          (mr lat)
          )
  )

;; traditional Lisp true-recursive version
(defn member? [a lat]
  (cond
   (empty? lat) false
   (= a (first lat)) true
   :else (member? a (rest lat))))

;; Clojure recur version, which again avoids the need to
;; pass a around in each invocation of the "recursive" loop
(defn member?-recur [a lat]
  (loop [lat lat]
    (cond
     (empty? lat) false
     (= a (first lat)) true
     :else (recur (rest lat)))))

;; letrec of member?
(defn member?-letrec [a lat]
  (letrec [memb? (fn [lat]
                   (cond
                    (empty? lat) false
                    (= a (first lat)) true
                    :else (member? a (rest lat))))]
          (memb? lat))
  )

;; --- union ---
(defn union
  "Recursive version of union that takes everything in set2
   and whatever is in set1 only, but not in set2"
  [set1 set2]
  (cond
   (empty? set1) set2 ;; can we change this to a #{} set?
   (member? (first set1) set2) (union (rest set1) set2)
   :else (cons (first set1) (union (rest set1) set2))   )
  )

(defn union-letrec [set1 set2]
  (letrec [U (fn [set1]
               (cond
                (empty? set1) set2
                (member? (first set1) set2) (U (rest set1))
                :else (cons (first set1) (U (rest set1)))
                ))]
          (U set1))
  )

;; Clojure provides the fn special form that optionally takes a name
;; allowing you to use that name in recursive calls, thus avoiding
;; the need for Lisp's letrec to do self-reference/self-recursion
(defn union-with-fn [set1 set2]
  ((fn UU [set1]
     (cond
      (empty? set1) set2
      (member? (first set1) set2) (UU (rest set1))
      :else (cons (first set1) (UU (rest set1)))
      )) set1)
  )

(defn union-recur
  "Idiomatic Clojure version of union using recur that takes
   everything in set2 and whatever is in set1 only, but not in set2"
  [set1 set2]
  (loop [set1 set1 outset []]
    (cond
     (empty? set1) (concat outset set2)
     (member? (first set1) set2) (recur (rest set1) outset)
     :else (recur (rest set1) (conj outset (first set1)))
     )
    )
  )

;; protected form of sum-of-prefixes and sum-of-prefixes-b
;; Note - this is not necessary if you use Clojure's recur
;; since sum-of-prefixes there did not require the helper
;; fn sum-of-prefixes-b, but here we implement the more
;; idiomatic Lisp version, as in the book

;; first with the letrec macro
(defn sum-of-prefixes2 [tup]
  (letrec [sum-b (fn [accum tup]
                   (if (empty? tup)
                     '()
                     (cons (+ accum (first tup)) (sum-b (+ accum (first tup)) (rest tup))))
                   )]
          (sum-b 0 tup))
  )

;; then using Clojure's fn to build an inner named function
(defn sum-of-prefixes2-with-fn [tup]
  ((fn sop-b [accum tup]
     (if (empty? tup)
       '()
       (cons (+ accum (first tup)) (sop-b (+ accum (first tup)) (rest tup))))
     ) 0 tup)
  )


;; protected version of scramble using Clojure's fn special form
(defn scramble2 [tup]
  ((fn scramble2-b [tup rev-pre]
     (if (empty? tup)
       []
       (let [new-rev-pre (cons (first tup) rev-pre)]
         (cons (pick (first tup) new-rev-pre)
               (scramble2-b (rest tup) new-rev-pre)))))
   tup [])
  )

;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 13 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

;; doing this only in idiomatic clojure with loop/recur
(defn intersect [set1 set2]
  (loop [set set1 outset []]
    (cond
     (empty? set) outset
     (member?-recur (first set) set2) (recur (rest set) (conj outset (first set)))
     :else (recur (rest set) outset)
     )
    )
  )

(defn intersect-all
  "Finds and returns the intersection of all sets in the coll of sets +lset+"
  [lset]
  (if (empty? lset)
    []  ; this manages the pre-condition (cd also do with Clojure :pre)
    (loop [llset (rest lset) outset (first lset)]
      (if (empty? llset)
        outset
        (recur (rest llset) (intersect outset (first llset)))))))


(defn rember-beyond-first
  "Removes all members from +lat+ after seeing the first +a+ in the coll"
  [a lat]
  ((fn R [lat]
     (cond
      (empty? lat) []
      (= a (first lat)) []
      :else (cons (first lat) (R (rest lat)))
      )
     ) lat)
  )

(defn rember-beyond-first-recur [a lat]
  (loop [lat lat outvec []]
    (cond
     (empty? lat) outvec
     (= a (first lat)) outvec
     :else (recur (rest lat) (conj outvec (first lat)))
     )
    )
  )

;; TODO: not sure how to do letcc in Clojure ...
;; There is a call-cc function in clojure-contrib.monads, but I'm not sure
;; how to use it, as I can't find much documentation or any examples of using it
;; There is also a delimited continuations (like "yield" in Ruby) library from
;; swannodette, but the documentation is a little sparse to quickly figure out
;; how to use it in this "letcc" context: https://github.com/swannodette/delimc
(defn rember-upto-last [a lat]
  ;; (call-cc ??)
  )

;; turns out to be very easy with Clojure loop/recur
;; no need to letcc and skip/jump to unwind the stack
(defn rember-upto-last-recur [a lat]
  (loop [lat lat outvec []]
    (cond
     (empty? lat) outvec
     (= a (first lat)) (recur (rest lat) [])  ; equiv of throwing out stack done to date
     :else (recur (rest lat) (conj outvec (first lat)))
     )
    )
  )

;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 14 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

(defn atom? [x]
  (not (coll? x)))

(defn leftmost [l]
  (cond
   (empty? l) []
   (atom? (first l)) (first l)
   :else (let [r (leftmost (first l))]
           (if (atom? r)
             r
             (leftmost (rest l))))))

;; since the arg list to leftmost is the same as what we need to recur
;; on, we don't need the loop binding, just use recur directly
;; but this is only a partially recur-based solution; one part requires
;; true recursion
(defn leftmost-recur [l]
  (cond
   (empty? l) []
   (atom? (first l)) (first l)
   :else (let [r (leftmost-recur (first l))]
           (if (atom? r)
             r
             (recur (rest l))))))

;; this does not achieve what it hoped to (I tried it with benchmarks)
;; need to research: http://stackoverflow.com/questions/3906831/how-do-i-generate-memoized-recursive-functions-in-clojure
(defn leftmost-recur-memoize [l]
  (let [memo-leftmost (memoize leftmost-recur-memoize)]
    (cond
     (empty? l) []
     (atom? (first l)) (first l)
     :else (let [r (memo-leftmost (first l))]
             (if (atom? r)
               r
               (recur (rest l)))
             ))))


;; (def leftmost-recur-memoize444
;;      (memoize
;;       (fn [l]
;;         (Thread/sleep 100)
;;         (cond
;;          (empty? l) []
;;          (atom? (first l)) (first l)
;;          :else (let [r (leftmost-recur-memoize444 (first l))]
;;                  (if (atom? r)
;;                    r
;;                    (recur (rest l))))))))

;; most idiomatic way to do this in Clojure - based on feedback
;; to my question on stackoverflow:
;;   http://stackoverflow.com/questions/8690675/caching-the-value-of-a-recursive-call-in-clojure-using-recur
;; this will be less performant for large lists/sequences, since it has to go through
;; all the elements, rather than just enough to get the first "atom"
(defn leftmost-with-flatten [l]
  ;; first return nil from an empty list, but this
  ;; spec wants empty list so make that the backup return val
  (or (first (flatten l)) [])
  )


;; this one matches the book
(defn rember1*
  "Removes the first instance of +a+ found in list l, even when it
     it is embedded in a sublist of l"
  [a l]
  ((fn R [ls]
     (cond
      (empty? ls) []
      (= a (first ls)) (rest ls)
      (atom? (first ls)) (cons (first ls) (R (rest ls)))
      :else (let [sv (R (first ls))]
              (if (= (first ls) sv) ;; if matches, then +a+ wasn't found and removed yet
                (cons (first ls) (R (rest ls)))
                (cons sv (rest ls)))))
       ) l)
    )

;; this is an alternative way to do an optimization to the true recursive
;; version without having to do a let statement - this works in Clojure bcs
;; the '=' function is generic - it works on "atoms" and lists/colls. That
;; is apparently not the case in Scheme and CL, so they have to distinguish
;; first and use eq? or eqlist? depending on the type
(defn rember1*-alt
  "Removes the first instance of +a+ found in list l, even when it
   it is embedded in a sublist of l"
  [a l]
  ((fn R [ls]
     (cond
      (empty? ls) []
      (= a (first ls)) (rest ls)
      (or (atom? (first ls))
          (let [sv (R (first ls))]
            (= (first ls) sv)))     (cons (first ls) (R (rest ls)))
      :else (cons (R (first ls)) (rest ls)))
     ) l)
  )

;; only recurs within a level or depth - still does true recursion into
;; sub-collections, bcs I haven't grokked how to do that iteratively (with
;; pseudo-recursion) yet
(defn rember1*-recur [a l]
  (loop [ls l accvec []]
    (cond
     (empty? ls) accvec
     (= a (first ls)) (into accvec (rest ls))  ; return right here
     (atom? (first ls)) (recur (rest ls) (conj accvec (first ls)))
     :else (let [sv (rember1*-recur a (first ls))]
             (if (= sv (first ls))
               (recur (rest ls) (conj accvec sv)) ;; didn't find +a+ in the sub-list
               (into (conj accvec sv) (rest ls))  ;; found it - so stop recurring
               )))
    )
  )


(defn depth*
  "Computes the depth of a nested collection - an empty collection has
   depth=1 and each additional nested collection increments the depth"
  [l]
  (cond
   (empty? l) 1
   (atom? (first l)) (depth* (rest l))
   :else (max (inc (depth* (first l)))
              (depth* (rest l)))
   )
  )

;; There doesn't seem to be a way to do this iteratively.
;; Even Clojure's flatten (via tree-seq) is truly recursive
;; since most data structures will not be so deep as to blow
;; the stack
;; TODO: - can we push depths onto a stack and then
;; run the through stack to find the max?
;; TODO: - does lazy-seq help here? Is it the Clojure way
;; to solve this?
(defn depth*-recur [l]
  (cond
   (empty? l) 1
   (atom? (first l)) (recur (rest l))
   :else (max (inc (depth*-recur (first l)))
              (depth*-recur (rest l)))
   )
  )

;; skipped the remainer of ch. 14, since it is all about using
;; letcc (call/cc?) in various ways, and I'm not sure how to use
;; that in Clojure yet


;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 15 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

(def x (atom [:chicago :pizza]))
(reset! x :skins)

(defn gourmet [food]
  (cons food (cons @x [])))

(defn gourmand [food]
  (reset! x food)
  (cons food (cons @x [])))

;; pp.95-100 shows how they define local variables in the scope
;; of the defined functions - and then use set! to modify those
;; based on the args passed in.

;; Clojure can do let-over-lambda
;; http://stackoverflow.com/questions/3306779/let-over-lambda-block-scanner-in-clojure
(defn omnivore [food]
  (let [x (atom "minestrone")]
    ((fn []
      (reset! x food)
      (cons food (cons @x []))))))

;; another way to do let-over-lambda - use def, not defn
;; this is closer to the Scheme way of doing it
(def omnivore-1
  (let [x (atom "minestrone")]
    (fn [food]
      (reset! x food)
      (cons food (cons @x [])))))


;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 16 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

;; I chose not to use two lists, one for numbers (Ns) and one
;; for real values (Rs), but rather use a map, as God intended
(def N2R (atom {}))

(defn getN2R [] @N2R)

(defn deep
  "Embeds the word 'pizza' in a nested list +n+ deep"
  [n]
    (if (= 0 n)
    "pizza"
    (cons (deep (dec n)) '()))
  )

(defn deep-with-external-memoize
  "Embeds the word 'pizza' in a nested list +n+ deep"
  [n]
  (if (= 0 n)
    (let [R "pizza"]
      (reset! N2R (assoc @N2R n R))
      R)
    (let [R (cons (deep-with-external-memoize (dec n)) '())]
      (reset! N2R (assoc @N2R n R))
      R)))

;; I changed to name to avoid conflict with clojure.core/find
(defn find-memoized [n map] (get map n))

(declare deepM-1)

(defn deep-knows-deepM-1
  "Embeds the word 'pizza' in a nested list +n+ deep"
  [n]
  (if (= 0 n)
    "pizza"
    (cons (deepM-1 (dec n)) '()))
  )

(defn deepM-1 [n]
  (let [R (find-memoized n @N2R)]
    (if R
      R
      (let [R (deep-knows-deepM-1 n)]
        ;; TODO: may be better to swap or compare-and-set rather than reset??
        (reset! N2R (assoc @N2R n R))
        R))))

;; non-memoized version
(defn deep-idiomatic-clj [n]
  (reduce (fn [& xs] (conj [] (first xs))) "pizza" (range n))
  )

;;now try the let-over-lambda version in Clojure (deepM p. 116)
(def deepM
  (let [n2r (atom {})]  ;; memoizes numbers to outputs
    (fn loc-deep [n]
      (let [mem (get @n2r n nil)]
        (if mem
          mem
          (let [ans (if (= 0 n)
                      "pizza"
                      (cons (loc-deep (dec n)) []))]
            (reset! n2r (assoc @n2r n ans))
            ans))))))

;; this one only memoizes the final answer, not all the answers in between
;; as we need to have loc-deep call the outer "main" function in deepM-2
;; have to use letfn to make mutually recursive calls
(def deepM-2
  (let [n2r       (atom {})
        loc-deep  (fn ldeep [n1]
                    (if (= 0 n1)
                      "pizza"
                      (cons (ldeep (dec n1)) '()))) ]
    (fn [n]
      (let [mem (get @n2r n nil)]
        (if mem
          mem
          (let [ans (loc-deep n)]
            (reset! n2r (assoc @n2r n ans))
            ans))))))

;; use letfn to make mutually recursive sub-functions
(def deepM-3
  (let [n2r (atom {})]
    (letfn [(loc-deep [n1]
              (if (= 0 n1)
                "pizza"
                (cons (loc-deepM (dec n1)) '())))
            
            (loc-deepM [n]
              (let [mem (get @n2r n nil)]
                (if mem
                  mem
                  (let [ans (loc-deep n)]
                    (reset! n2r (assoc @n2r n ans))
                    ans)))) ]
      loc-deepM)))

;; this version works in terms of returning the right answer
;; but IT NEVER MEMOIZES THE RESULTS BETWEEN CALLS, bcs each
;; time deepM-4 is clled it recreates the data stuctures in it
;; => this is the reason The Seasoned Schemer "17th Commandment"
;;    says to only use let and set! when there is a lambda between
;;    them - if the let occurs within the lambda (function def)
;;    it gets recreated each time the function is called, destroying
;;    any memoization you were trying to do.
(defn deepM-4 [n]
  (let [n2r (atom {})]
    (letfn [(loc-deep [n1]
              (if (= 0 n1)
                "pizza"
                (cons (loc-deepM (dec n1)) '())))

            (loc-deepM [i]
              (let [mem (get @n2r i nil)]
                (if mem
                  mem
                  (let [ans (loc-deep i)]
                    (reset! n2r (assoc @n2r i ans))
                    ans)))) ]
      (loc-deepM n))))


;; Now we start down the path of Y!,
;; the applicative order Y combinator

;; changed name to avoid conflict with clojure.core/length
(defn sslength-orig [l]
  (if (empty? l)
    0
    (inc (sslength-orig (rest l))))
  )

(def sslength
  (let [h (atom (fn [l] 0))]
    ;; I didn't explicitly return h, because reset! does that in Clojure
    (reset! h (fn [l]
                (if (empty? l)
                  0
                  (inc (@h (rest l))))))))

;; defn function that takes the length func as an arg
;; and returns a function that calcs length (and is the
;; one that needs to be passed to this function L)
(defn L [length]
  (fn [l]
    (if (empty? l)
      0
      (inc (length (rest l))))))

(def sslength2
  (let [h (atom (fn [l] 0))]
    ;; (reset! h (L @h))
    (reset! h (L (fn [arg] (@h arg))))))

(defn Y! [L]
  (let [h (atom (fn [l] nil))]
    (reset! h (L (fn [arg] (@h arg))))))

;; final version of length using applicative order
;; imperative Y-combinator
(def sslength3 (Y! L))

(comment
  (defn depth*
    "Computes the depth of a nested collection - an empty collection has
   depth=1 and each additional nested collection increments the depth"
    [l]
    (cond
     (empty? l) 1
     (atom? (first l)) (depth* (rest l))
     :else (max (inc (depth* (first l)))
                (depth* (rest l)))))
  )
(defn D [depth*]
  (fn [l]
    (cond
     (empty? l) 1
     (atom? (first l)) (depth* (rest l))
     :else (max (inc (depth* (first l)))
                (depth* (rest l)))
     )
    )
  )

(def Ydepth* (Y! D))

(def Y
  (fn [r]
    ((fn [f] (f f)) 
     (fn [Y]
       (r (fn [x] ((Y Y) x)))))))

(def biz
  (let [x (atom 0)]
    (fn [f]
      (reset! x (inc @x))
      (fn [a]
        (if (= a @x)
          0
          (f a))))))


;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 17 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

;; corresponds to version of deepM on p. 128
(def deepM-128
  (let [n2r (atom {})
        D (fn [n1]
            (if (= 0 n1)
              "pizza"
              (cons (deepM-128 (dec n1)) '())))]
    (fn [n]
      (let [mem (get @n2r n nil)]
        (if mem
          mem
          (let [ans (D n)]
            (reset! n2r (assoc @n2r n ans))
            ans))))))

;; first version of deepM on p. 129
(def deepM-129
  (let [n2r (atom {})]
    (fn [n]
      (let [mem (get @n2r n nil)]
        (if mem
          mem
          (let [ans ((fn [n1]    ;; <== replace this with a let stmt (p. 129)
                       (if (= 0 n1)
                         "pizza"
                         (cons (deepM-129 (dec n1)) '()))) n)]
            (reset! n2r (assoc @n2r n ans))
            ans))))
    )
  )

;; second version of deepM on p. 129
;; replace the "lambda" with a let
(def deepM-129-2
  (let [n2r (atom {})]
    (fn [n]
      (let [mem (get @n2r n nil)]
        (if mem
          mem
          (let [ans (let [n1 n]  ;; <== replaced fn with a let stmt (p. 129)
                      (if (= 0 n1)
                        "pizza"
                        (cons (deepM-129-2 (dec n1)) '())))]
            (reset! n2r (assoc @n2r n ans))
            ans))))))


(def set-counter (atom #(identity 0)))

;; p.131: consC
(def consC
  (let [N (atom 0)]
    (reset! set-counter #(reset! N %))
    (fn [x y]
      (reset! N (inc @N))
      (cons x y))))


(defn deep-with-consC [n]
  (if (= 0 n)
    "pizza"
    (consC (deep-with-consC (dec n)) [])
    )
  )

;; doesn't work (gets stackoverflow error) - ask why on stackoverflow?
;;  probably because it calls f as well as itself and maybe lazy-seq
;;  can't deal with that?
(defn supercounter-lazy
  "Takes a function argment and calls it 1000 times"
  [f]
  ;; TODO: add lazy-seq to this
  (letfn [(S [n]
            (lazy-seq
             (if (zero? n)
               (f n)
               (do
                 (f n)
                 (S (dec n))
                 )))
            )]
    (lazy-seq (S 10000))))


(defn supercounter
  "Takes a function argment and calls it 1000 times"
  [f]
  ;; TODO: add lazy-seq to this
  (letfn [(S [n]
            (if (zero? n)
               (f n)
               (do
                 (f n)
                 (S (dec n))
                 )))]
    (S 1000)))


;; this one uses consC to count how many times it has
;; called cons
(def deepM-129-3
  (let [n2r (atom {})]
    (fn [n]
      (let [mem (get @n2r n nil)]
        (if mem
          mem
          (let [ans (let [n1 n]  ;; <== replaced fn with a let stmt (p. 129)
                      (if (= 0 n1)
                        "pizza"
                        (consC (deepM-129-2 (dec n1)) '())))]
            (reset! n2r (assoc @n2r n ans))
            ans))))))


(defn rember1*-consC
  "Removes the first instance of +a+ found in list l, even when it
     it is embedded in a sublist of l"
  [a l]
  ((fn R [ls]
     (cond
      (empty? ls) []
      (= a (first ls)) (rest ls)
      (atom? (first ls)) (consC (first ls) (R (rest ls)))
      :else (let [sv (R (first ls))]
              (if (= (first ls) sv) ;; if matches, then +a+ wasn't found and removed yet
                (consC (first ls) (R (rest ls)))
                (consC sv (rest ls)))))
       ) l)
    )


;; LEFT OFF - next define set-counter blankly as below and then mod consC to have
;; a set-counter section (not sure how to call it)
;; TODO: best idea - do this in a Clojure way by:
;;  * making a macro that calls set-counter before the method starts (could also make it thread safe)
;;  * use the Clojure in Action (let-over-lambda?) style to get the count with a call
;;    to the closure with :set

;; adapted from p.131: consC
(def consJ
  (let [N (atom 0)]
    (fn
      ([x y]
         (swap! N inc)
         (cons x y))
      ([cmd]
         (condp = cmd
           :get-count @N
           :reset-count (reset! N 0))))))


(defn deep-consJ [n]
  (if (zero? n)
    "pizza"
    (lazy-seq
     (consJ (deep-consJ (dec n)) [])))
  )

(defn deep-consJ2 [n]
  (lazy-seq
   (if (zero? n)
    "pizza"
    (consJ (deep-consJ2 (dec n)) []))))

(defn deep-consJ3 [n]
  (lazy-seq
   (if (zero? n)
    "pizza"
    ;; NOTE: doesn't actually use consJ, just plain old cons
    (cons (deep-consJ3 (dec n)) []))))

;; from Joy of Clojure (p. 116)
(defn lz-rec-step [s]
  (lazy-seq
   (if (seq s)
     [(first s) (lz-rec-step (rest s))]
     [])))


;; not lazy version
(defn rec-step [s]
  (if (seq s)
    [(first s) (rec-step (rest s))]
    []))


(def deepM-consJ
  (let [n2r (atom {})]
    (fn [n]
      (let [mem (get @n2r n nil)]
        (if mem
          mem
          ;; TODO: need to add lazy-seq somewhere
          (let [ans (let [n1 n]
                      (if (= 0 n1)
                        "pizza"
                        (consJ (deepM-consJ (dec n1)) '())))]
            (reset! n2r (assoc @n2r n ans))
            ans))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLAYGROUND for closures based on Clojure in Action Ch. 13.3

(defn new-user [login password email]
  (let [nlookups (atom 0)]
    (fn [arg]
      (when (not= :lookups arg) (swap! nlookups inc))
      (condp = arg
        :login login
        :password password
        :email email
        :lookups @nlookups
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLAYGROUND to test out let-over-lambda = delete later after done testing
;; (defn let-over-lambda1 [food]
;;   (let [x (atom "minestrone")]
;;     ((fn []
;;       (reset! x food)
;;       (cons food (cons @x []))))))

;; (defn let-over-lambda1b [food & lookup]
;;   (when lookup (println ))
;;   (let [x (atom "minestrone")]
;;     ((fn []
;;       (reset! x food)
;;       (cons food (cons @x []))))))

;; ;; another way to do let-over-lambda - use def, not defn
;; ;; this is closer to the Scheme way of doing it
;; (def let-over-lambda2
;;   (let [x (atom "minestrone")]
;;     (fn [food]
;;       (reset! x food)
;;       (cons food (cons @x [])))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 18 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

;; Note - I wasn't able to grok how to get their versions
;; of kons, kar and kdr working, so this chapter is quite
;; incomplete

(defn kons1 [kar kdr]
  #(% kar kdr))

;; the only way I can figure out to get kons to work
(def kons
  (fn [kar kdr]
      ((fn [selector]
         (selector kar kdr)) #(cons % %2))))

(defn kar1 [c]
  (c (fn [a d] a)))

;; the only way I can figure out to get kar to work
(defn kar [[a & d]] a)

(defn kdr1 [c]
  (c (fn [a d] d)))

;; the only way I can figure out to get kdr to work
(def kdr
  (fn [[a & d]] d))

(defn lots [n]
  (if (zero? n)
    '()
    (kons :egg (lots (dec n)))))

(defn lenkth [l]
  (if (empty? l)
    0
    (inc (lenkth (kdr l))))
  )

(defn add-at-end
  "Adds :egg to end of a list (using cons, traditional Lisp style)"
  [l]
  (if (empty? (kdr l))
    (kons (first l) (kons :egg ()))
    (kons (first l) (add-at-end (kdr l)))))


(comment
  (defn add-at-end-too
    "Adds :egg to end of a list (using cons, traditional Lisp style)"
    [l]
    (letfn [(A [ls]
              (if (empty? kdr ls)
                ;; TODO: set-kdr is not defined and I have no idea what it is supposed to do
                set-kdr ls (kons :egg ())
                (A (kdr ls))
                ))]))
  )


;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 19 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

;; ch. 19 deals with letcc - continuations, which there is no
;; facility to do in Clojure (at least I don't know how to do it)

;; LEFT OFF: bottom of p. 160 - haven't read all of ch. 19 yet ...
