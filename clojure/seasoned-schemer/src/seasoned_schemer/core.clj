(ns seasoned-schemer.core
  (:gen-class))

;;; -------------------------------------------------- ;;;
;;; -----------------[ Chapter 1 ] ------------------- ;;;
;;; -------------------------------------------------- ;;;

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
  ;; doesn't work
  (conj (sum-of-prefixes-b (first tup) (rest tup)) (+ preceding (first tup)))
)

(defn- sum-of-prefixes-b [accum tup]
  (if (empty? tup)
    '()
    (cons (+ accum (first tup)) (sum-of-prefixes-b (+ accum (first tup)) (rest tup)))))

;; use loop/recur
(declare sum-of-prefixes-recur-b)

(defn sum-of-prefixes-recur [tup]
  (sum-of-prefixes-recur-b 0 tup)  
  )

(defn- sum-of-prefixes-recur-b [acctot tup]
  (loop [acctot acctot tup tup outvec []]
    (if (empty? tup)
      outvec
      (recur (+ acctot (first tup))
             (rest tup)
             (conj outvec (+ acctot (first tup)))) ;; NOT DRY
      )
    )
  )



;;  sum-of-prefixes-b [1 2]
;; (conj (conj ([2]) (+ 1 2)) (+ 0 1))


;;; -------------------------------------------------- ;;;
;;; -----------------[ Chapter 2 ] ------------------- ;;;
;;; -------------------------------------------------- ;;;

;;; -------------------------------------------------- ;;;
;;; -----------------[ Chapter 3 ] ------------------- ;;;
;;; -------------------------------------------------- ;;;

;; (defn -main [& args]
;;   (println "Welcome to the Seasoned Schemer")
;;   (println "tiar1: " (two-in-a-row? [1 2 3 3 4]))
;;   (println "tiar2: " (two-in-a-row? [1 2 3 :a 4]))
;;   (println "tiar3: " (two-in-a-row? '()))
;;   (println "tiar4: " (two-in-a-row? []))
;;   (println "rtiar1:" (two-in-a-row-recur? [1 2 3 3 4]))
;;   (println "rtiar2:" (two-in-a-row-recur? [1 2 3 :a 4]))
;;   (println "rtiar3:" (two-in-a-row-recur? '()))
;;   (println "rtiar4:" (two-in-a-row-recur? []))
;;   )


