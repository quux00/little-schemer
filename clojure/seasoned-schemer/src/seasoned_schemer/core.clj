(ns seasoned-schemer.core
  (:use [seasoned-schemer.macros])
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

;; Clojure provides the fn special form that optionally takes a name,
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
(defn scramble2 [tup])
;; TODO: must finish!

;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 13 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

;; TODO: skipped a lot of functions here

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
(defn rember-upto-last [a lat]
  
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

;; since the arg list to leftmost is the same as what we need to recur
;; on, we don't need the loop binding, just use recur directly
(defn leftmost [l]
  (cond
   (empty? l) []
   (atom? (first l)) (first l)
   :else (let [r (leftmost (first l))]
           (if (atom? r)
             r
             (leftmost (rest l)))
           )
   )
  )
