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

(defn- scramble-b-with-let [tup rev-pre]
  (if (empty? tup)
    []
    (let [new-rev-pre (cons (first tup) rev-pre)]
      (cons (pick (first tup) new-rev-pre)
            (scramble-b (rest tup) new-rev-pre)))
    )
  )

(defn- scramble-b [tup rev-pre]
  (if (empty? tup)
    []
    (cons (pick (first tup) (cons (first tup) rev-pre))
          (scramble-b (rest tup) (cons (first tup) rev-pre))))
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

;; TODO: define letrec version here

;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 13 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;
