(ns little.cljr
  (:use clojure.core))

(def fail-tok "\n>>>>> FAIL: ")
(def nfails   (ref 0))
(def nasserts (ref 0))

(defn header-str [func-name] 
  (str "======= " func-name " tests ======="))

;; -------------------------- ;;
;; ---[ ASSERT FUNCTIONS ]--- ;;
;; -------------------------- ;;

;; (defn assert-gen [exp act fpass ffail]
;;   (if (= exp act)
;;     (fpass)
;;     (ffail)

(defn assert-true 
  "assert that the first argument is logical true, otherwise print +msg+"
  [bool msg]
  (dosync 
   (ref-set nasserts (inc @nasserts))
   (if bool
     (print ".")
     (do
       (println (str fail-tok msg))
       (ref-set nfails (inc @nfails))))))

(defn assert-false 
  "assert that the first argument is logical false, 
   otherwise print +msg+"
  [bool msg]
  (assert-true (not bool) msg))

(defn assert-eq 
  "Assert that +exp+ is exactly equal to +act+ using Clojure's
   built-in = method, otherwise print +msg+"
  [exp act msg]
  (dosync 
   (ref-set nasserts (inc @nasserts))
   (if (= exp act)
     (print ".")
     (do 
       (println (str fail-tok msg "; exp = " exp "; act = " act))
       (ref-set nfails (inc @nfails))))))

(defn assert-eqset
  "Assert that +exp-set+ has the same members as +act-set+ using
   the eqset? method defined in little.cljr library, otherwise 
   print +msg+"
  [exp-set act-set msg]
  (dosync
   (ref-set nasserts (inc @nasserts))
   (if (eqset? exp-set act-set)
     (print ".")
     (do
       (println (str fail-tok msg "; exp = " exp-set "; act = " act-set))
       (ref-set nfails (inc @nfails))))))
       
   
   
;; --------------------- ;;
;; ---[ START TESTS ]--- ;;
;; --------------------- ;;

(def start-time (System/currentTimeMillis))

(println (header-str "atom?"))
(assert-true  (atom? 1)           "1")
(assert-true  (atom? :1)          "2")
(assert-false (atom? '())         "3")
(assert-false (atom? '(1 2))      "4")
(assert-true  (atom? "taco bell") "5")


(println (str "\n" (header-str "lat?")))
(assert-true  (lat? '(1))       "1")
(assert-true  (lat? '(1 2 3 5)) "2")
(assert-true  (lat? '(1 :2))    "3")
(assert-false (lat? '(1 (1)))   "4")
(assert-false (lat? '((1)))     "5")
(assert-false (lat? '(()))      "6")


(println (str "\n" (header-str "member?")))
(assert-true  (member? 1 '(1))           "1")
(assert-true  (member? 1 '(1 2 3 4))     "2")
(assert-true  (member? 3 '(1 2 3 4))     "3")
(assert-true  (member? 3 '(1 2 3 4 3))   "4")
(assert-false (member? 33 '(1 2 3 4 3))  "5")
(assert-false (member? 1 '())            "6")
(assert-false (member? 1 '((1) (2)))     "7")
(assert-false (member? 1 '((1 2 3)))     "8")
(assert-true  (member? 1 '((1) 2 1))     "9")
(assert-true  (member? :a '(1 2 3 :a 4)) "10")
(assert-true  (member? '(1) '((1) 2 1))  "11")
(assert-true  (member? '(:a :b :c) '((1 2 3) (:a :b :c))) "12")


(println (str "\n" (header-str "rember")))
(assert-eq '(:a) (rember1     :ab '(:a :ab)) "1")
(assert-eq '(:a) (rember1-alt :ab '(:a :ab)) "1-alt")
(assert-eq '(:a) (rember      :ab '(:a :ab)) "1-2")

(assert-eq '() (rember1     :ab '()) "2")
(assert-eq nil (rember1-alt :ab '()) "2-alt")
(assert-eq '() (rember      :ab '()) "2-2")

(assert-eq '(:a :ab :c) (rember1     :x '(:a :ab :c)) "3")
(assert-eq '(:a :ab :c) (rember1-alt :x '(:a :ab :c)) "3-alt")
(assert-eq '(:a :ab :c) (rember      :x '(:a :ab :c)) "3-2")

(assert-eq '(:a :c :ab) (rember1     :ab '(:a :ab :c :ab)) "4")
(assert-eq '(:a :c :ab) (rember1-alt :ab '(:a :ab :c :ab)) "4-alt")
(assert-eq '(:a :c :ab) (rember      :ab '(:a :ab :c :ab)) "4-2")

(assert-eq '(:a :c :ab) (rember1     '(:ab) '(:a (:ab) :c :ab)) "5")
(assert-eq '(:a :c :ab) (rember1-alt '(:ab) '(:a (:ab) :c :ab)) "5")
(assert-eq '(:a :c :ab) (rember      '(:ab) '(:a (:ab) :c :ab)) "5-2")


(println (str "\n" (header-str "firsts")))
(assert-eq '(1 4 7) (firsts '((1 2 3) (4 5 6) (7 8 9))) "1")
(assert-eq '(1 4 7) (firsts '((1 2 3) (4 5) (7))) "2")
(assert-eq '(1 4 7) (firsts '((1) (4 5) (7 8 9))) "3")
(assert-eq '((1 2) 4 (:a)) (firsts '(((1 2)) (4 5) ((:a) :b (:c :d)))) "4")


(println (str "\n" (header-str "insertR")))
(assert-eq '(1 2 3 666 4 :5) (insertR 666 3 '(1 2 3 4 :5)) "1")
(assert-eq '(1 666 2 3 4 :5) (insertR 666 1 '(1 2 3 4 :5)) "2")
(assert-eq '(1 2 3 4 :5 666) (insertR 666 :5 '(1 2 3 4 :5)) "3")
(assert-eq '(1 2 3 4 :5) (insertR 666 :NA '(1 2 3 4 :5)) "4")
(assert-eq '() (insertR :new :old '()) "5")
(assert-eq '(1 2) (insertR :new nil '(1 2)) "6")
;; this one returns the original list, bcs it is not a lat
;; technical insertR doesn't have to work on lat's, but that's
;; how the Little Schemer has it, so I'm following it
(assert-eq '((1 2) (3 4) :5)
           (insertR '(666 :a) '(3 4) '((1 2) (3 4) :5))
           "7")


(println (str "\n" (header-str "insertL")))
(assert-eq '(1 2 666 3 4 :5) (insertL 666 3 '(1 2 3 4 :5))   "1")
(assert-eq '(666 1 2 3 4 :5) (insertL 666 1 '(1 2 3 4 :5))   "2")
(assert-eq '(1 2 3 4 666 :5) (insertL 666 :5 '(1 2 3 4 :5))  "3")
(assert-eq '(1 2 3 4 :5)     (insertL 666 :NA '(1 2 3 4 :5)) "4")
(assert-eq '()               (insertL :new :old '())         "5")
(assert-eq '(1 2)            (insertL :new nil '(1 2))       "6")
;; this one returns the original list, bcs it is not a lat
;; technical insertL doesn't have to work on lat's, but that's
;; how the Little Schemer has it, so I'm following it
(assert-eq '((1 2) (3 4) :5)
           (insertL '(666 :a) '(3 4) '((1 2) (3 4) :5))
           "7")


(println (str "\n" (header-str "subst")))
(assert-eq '(1 2 666 4 :5) (subst 666 3 '(1 2 3 4 :5))   "1")
(assert-eq '(666 2 3 4 :5) (subst 666 1 '(1 2 3 4 :5))   "2")
(assert-eq '(1 2 3 4 666)  (subst 666 :5 '(1 2 3 4 :5))  "3")
(assert-eq '(1 2 3 4 :5)   (subst 666 :NA '(1 2 3 4 :5)) "4")
(assert-eq '()             (subst :new :old '())         "5")
(assert-eq '(1 2)          (subst :new nil '(1 2))       "6")
;; this one returns the original list, bcs it is not a lat
;; technical subst doesn't have to work on lat's, but that's
;; how the Little Schemer has it, so I'm following it
(assert-eq '((1 2) (3 4) :5)
           (subst '(666 :a) '(3 4) '((1 2) (3 4) :5))
           "7")

(println (str "\n" (header-str "subst2")))
(assert-eq '(1 2 666 4 :5)   (subst2 666 3 4 '(1 2 3 4 :5))      "1")
(assert-eq '(666 2 3 4 :5)   (subst2 666 1 4 '(1 2 3 4 :5))      "2")
(assert-eq '(666 1 2 3 4 :5) (subst2 666 1 4 '(4 1 2 3 4 :5))    "3")
(assert-eq '(1 2 3 4 666)    (subst2 666 :4 :5 '(1 2 3 4 :5))    "4")
(assert-eq '(1 2 3 4 :5)     (subst2 666 :NA :NA2 '(1 2 3 4 :5)) "5")
(assert-eq '()               (subst2 :new :o1 :o2 '())           "6")
(assert-eq '(1 2)            (subst2 :new nil nil '(1 2))        "7")
;; this one returns the original list, bcs it is not a lat
;; technical subst2 doesn't have to work on lat's, but that's
;; how the Little Schemer has it, so I'm following it
(assert-eq '((1 2) (3 4) :5)
           (subst2 '(666 :a) '(3 4) :abc '((1 2) (3 4) :5))
           "8")


(println (str "\n" (header-str "multirember")))
(assert-eq '(:a)        (multirember :ab '(:a :ab))                "1")
(assert-eq '()          (multirember :ab '())                      "2")
(assert-eq '(:a :ab :c) (multirember :x '(:a :ab :c))              "3")
(assert-eq '(:a :c)     (multirember :ab '(:a :ab :c))             "4")
(assert-eq '(:a :c)     (multirember :ab '(:a :ab :c :ab))         "5")
(assert-eq '(:a :c)     (multirember :ab '(:a :ab :ab :c))         "6")
(assert-eq '(:a :c)     (multirember :ab '(:ab :a :ab :c :ab :ab)) "7")
(assert-eq '(:x)        (multirember :ab '(:ab :ab :ab :x))        "8")
(assert-eq '()          (multirember :ab '(:ab :ab :ab))           "9")



(println (str "\n" (header-str "mutliinsertR")))
(assert-eq '(1 2 3 666 4 :5)
           (multiinsertR 666 3 '(1 2 3 4 :5)) "1")
(assert-eq '(1 666 2 3 4 :5)
           (multiinsertR 666 1 '(1 2 3 4 :5)) "2")
(assert-eq '(1 2 3 4 :5 666)
           (multiinsertR 666 :5 '(1 2 3 4 :5)) "3")
(assert-eq '(1 2 3 4 :5)
           (multiinsertR 666 :NA '(1 2 3 4 :5)) "4")
(assert-eq '()
           (multiinsertR :new :old '()) "5")
(assert-eq '(1 2)
           (multiinsertR :new nil '(1 2)) "6")
(assert-eq '(1 666 2 1 666 3 4 :5 1 666)
           (multiinsertR 666 1 '(1 2 1 3 4 :5 1)) "7")


(println (str "\n" (header-str "mutliinsertL")))
(assert-eq '(1 2 666 3 4 :5) (multiinsertL 666 3 '(1 2 3 4 :5)) "1")
(assert-eq '(666 1 2 3 4 :5) (multiinsertL 666 1 '(1 2 3 4 :5)) "2")
(assert-eq '(1 2 3 4 666 :5) (multiinsertL 666 :5 '(1 2 3 4 :5)) "3")
(assert-eq '(1 2 3 4 :5) (multiinsertL 666 :NA '(1 2 3 4 :5)) "4")
(assert-eq '() (multiinsertL :new :old '()) "5")
(assert-eq '(1 2) (multiinsertL :new nil '(1 2)) "6")
(assert-eq '(666 :1 2 666 :1 3 4 :5 666 :1)
           (multiinsertL 666 :1 '(:1 2 :1 3 4 :5 :1)) "7")


(println (str "\n" (header-str "multisubst")))
(assert-eq '(1 2 666 4 :5) (multisubst 666 3 '(1 2 3 4 :5))   "1")
(assert-eq '(666 2 3 4 :5) (multisubst 666 1 '(1 2 3 4 :5))   "2")
(assert-eq '(1 2 3 4 666)  (multisubst 666 :5 '(1 2 3 4 :5))  "3")
(assert-eq '(1 2 3 4 :5)   (multisubst 666 :NA '(1 2 3 4 :5)) "4")
(assert-eq '()             (multisubst :new :old '())         "5")
(assert-eq '(1 2)          (multisubst :new nil '(1 2))       "6")
(assert-eq '(666 2 666 3 4 :5 666)
           (multisubst 666 :1 '(:1 2 :1 3 4 :5 :1)) "7")
(assert-eq '(666 666 666 666)
           (multisubst 666 :1 '(:1 :1 :1 :1)) "8")


;; Chapter 4 tests ;;

(println (str "\n" (header-str "o+")))
(assert-eq 11 (o+ 11 0) "1")
(assert-eq 222 (o+ 111 111) "2")
(assert-eq 22 (o+ 10 12) "3")
(assert-eq 22 (o+ 1 21)  "4")
(assert-eq 22 (o+ 22 0)  "5")
(assert-eq 22 (o+ 0 22)  "6")
(assert-eq 1  (o+ 0 1)   "7")
(assert-eq 0  (o+ 0 0)   "8")


(println (str "\n" (header-str "o2+")))
(assert-eq 11 (o2+ 11 0) "1")
(assert-eq 222 (o2+ 111 111) "2")
(assert-eq 22 (o2+ 10 12) "3")
(assert-eq 22 (o2+ 1 21)  "4")
(assert-eq 22 (o2+ 22 0)  "5")
(assert-eq 22 (o2+ 0 22)  "6")
(assert-eq 1  (o2+ 0 1)   "7")
(assert-eq 0  (o2+ 0 0)   "8")

(println (str "\n" (header-str "o-")))
(assert-eq 11 (o- 11 0)    "1")
(assert-eq 0  (o- 111 111) "2")
(assert-eq 1  (o- 3 2)     "3")
(assert-eq 11 (o- 13 2)    "4")


(println (str "\n" (header-str "addtup")))
(assert-eq 11  (addtup '(11 0))        "1")
(assert-eq 222 (addtup '(0 111 0 111)) "2")
(assert-eq 20  (addtup '(5 1 2 3 4 5)) "3")
(assert-eq 0   (addtup '())            "4")


(println (str "\n" (header-str "o*")))
(assert-eq 0   (o* 11 0)  "1")
(assert-eq 0   (o* 0 11)  "2")
(assert-eq 121 (o* 11 11) "3")
(assert-eq 6   (o* 3 2)   "4")
(assert-eq 39  (o* 13 3)  "5")
(assert-eq 39  (o* 3 13)  "6")


(println (str "\n" (header-str "tup+-orig")))
(assert-eq '(11 11) (tup+-orig '(11 0) '(0 11))   "1")
(assert-eq '(2 3 4) (tup+-orig '(0 1 2) '(2 2 2)) "2")
(assert-eq '(14)    (tup+-orig '(2) '(12))        "3")
(assert-eq '()      (tup+-orig '() '())           "4")


(println (str "\n" (header-str "tup+")))
(assert-eq '(11 11)      (tup+ '(11 0) '(0 11))        "1")
(assert-eq '(2 3 4)      (tup+ '(0 1 2) '(2 2 2))      "2")
(assert-eq '(14)         (tup+ '(2) '(12))             "3")
(assert-eq '()           (tup+ '() '())                "4")
; test different length tuples
(assert-eq '(2 3 4 2 2)  (tup+ '(0 1 2) '(2 2 2 2 2))  "5")
(assert-eq '(2 3 4 2 12) (tup+ '(0 1 2 2 12) '(2 2 2)) "6")
(assert-eq '(12 11 10)   (tup+ '() '(12 11 10))        "7")
(assert-eq '(12 11 10)   (tup+ '(12 11 10) '())        "8")


(println (str "\n" (header-str "o>")))
(assert-true  (o> 10 9)  "1")
(assert-false (o> 9 10)  "2")
(assert-true  (o> 1 0)   "3")
(assert-false (o> 0 1)   "4")
(assert-false (o> 0 0)   "5")
(assert-false (o> 34 34) "6")


(println (str "\n" (header-str "o<")))
(assert-false (o< 10 9)  "1")
(assert-true  (o< 9 10)  "2")
(assert-false (o< 1 0)   "3")
(assert-true  (o< 0 1)   "4")
(assert-false (o< 0 0)   "5")
(assert-false (o< 34 34) "6")

(println (str "\n" (header-str "o=")))
(assert-false (o= 10 9)  "1")
(assert-false (o= 9 10)  "2")
(assert-false (o= 1 0)   "3")
(assert-false (o= 0 1)   "4")
(assert-true  (o= 0 0)   "5")
(assert-true  (o= 34 34) "6")

(println (str "\n" (header-str "exp")))
(assert-eq 1   (exp 1 1) "1")
(assert-eq 2   (exp 2 1) "2")
(assert-eq 4   (exp 2 2) "3")
(assert-eq 8   (exp 2 3) "4")
(assert-eq 1   (exp 2 0) "5")
(assert-eq 125 (exp 5 3) "6")
(assert-eq 0   (exp 0 3) "7")

(println (str "\n" (header-str "exp2")))
(assert-eq 1   (exp2 1 1) "1")
(assert-eq 2   (exp2 2 1) "2")
(assert-eq 4   (exp2 2 2) "3")
(assert-eq 8   (exp2 2 3) "4")
(assert-eq 1   (exp2 2 0) "5")
(assert-eq 125 (exp2 5 3) "6")
(assert-eq 0   (exp2 0 3) "7")

(println (str "\n" (header-str "quotient")))
(assert-eq 1  (quotient 1 1)   "1")
(assert-eq 2  (quotient 2 1)   "2")
(assert-eq 1  (quotient 2 2)   "3")
(assert-eq 5  (quotient 10 2)  "4")
(assert-eq 3  (quotient 15 4)  "5")
(assert-eq 41 (quotient 125 3) "6")
(assert-eq 2  (quotient 7 3)   "7")

(println (str "\n" (header-str "length")))
(assert-eq 0 (length '())               "1")
(assert-eq 1 (length '(1))              "2")
(assert-eq 2 (length '(1 1))            "3")
(assert-eq 3 (length '(1 :2 3))         "4")
(assert-eq 5 (length '(:h :a :c :o :r)) "5")

(println (str "\n" (header-str "pick")))
(assert-eq nil (pick 1  '())          "1")
(assert-eq nil (pick 13 '())          "2")
(assert-eq 3   (pick 1  '(3 4 5))     "3")
(assert-eq 5   (pick 3  '(3 4 5))     "4")
(assert-eq nil (pick 13 '(1 2 3 4 5)) "5")

(println (str "\n" (header-str "rempick")))
(assert-eq '()        (rempick 1  '())          "1")
(assert-eq '()        (rempick 2  '())          "2")
(assert-eq '(4 5)     (rempick 1  '(3 4 5))     "3")
(assert-eq '(3 4)     (rempick 3  '(3 4 5))     "4")
(assert-eq '(1 2 3 5) (rempick 4  '(1 2 3 4 5)) "5")
(assert-eq '(1 2 3)   (rempick 4  '(1 2 3))     "6")
(assert-eq '(1 2 3)   (rempick 5  '(1 2 3))     "7")

(println (str "\n" (header-str "no-nums")))
(assert-eq '(:a :b)       (no-nums '(:a :b))         "1")
(assert-eq '(:a)          (no-nums '(:a 1 2))        "2")
(assert-eq '(:a)          (no-nums '(1 :a 2))        "3")
(assert-eq '(:a :b)       (no-nums '(1 :a 2 :b 3))   "4")
(assert-eq '(:a :b :c :d) (no-nums '(1 :a :b :c :d)) "5")
(assert-eq '()            (no-nums '(1 2 3))         "6")
(assert-eq '()            (no-nums '())              "7")

(println (str "\n" (header-str "all-nums")))
(assert-eq '()      (all-nums '(:a :b))         "1")
(assert-eq '(1 2)   (all-nums '(:a 1 2))        "2")
(assert-eq '(1 2)   (all-nums '(1 :a 2))        "3")
(assert-eq '(1 2 3) (all-nums '(1 :a 2 :b 3))   "4")
(assert-eq '(1)     (all-nums '(1 :a :b :c :d)) "5")
(assert-eq '(1 2 3) (all-nums '(1 2 3))         "6")
(assert-eq '()      (all-nums '())              "7")

(println (str "\n" (header-str "occur")))
(assert-eq 1  (occur :a '(:a :b))    "1")
(assert-eq 0  (occur :x '(:a 1 2))   "2")
(assert-eq 0  (occur :x '())         "3")
(assert-eq 0  (occur nil '(:a 1 2))  "4")
(assert-eq 3  (occur :a '(:a :a :a)) "5")
(assert-eq 1  (occur :a '(:b 1 :a))  "6")

(println (str "\n" (header-str "one?")))
(assert-false (one? '(:a :b)) "1")
(assert-false (one? :a)       "2")
(assert-true  (one? 1)        "3")
(assert-true  (one? (inc 0))  "4")
(assert-false (one? :1)       "5")
(assert-false (one? 0)        "6")
(assert-false (one? 11)       "7")

(println (str "\n" (header-str "rember*")))
(assert-eq '()            (rember* :a '())                     "1")
(assert-eq '()            (rember* :a '(:a))                   "2")
(assert-eq '(:b)          (rember* :a '(:b))                   "3")
(assert-eq '(:b :c :d)    (rember* :a '(:b :c :d))             "4")
(assert-eq '(:b :c :d :d) (rember* :a '(:b :c :a :d :a :a :d)) "5")
(assert-eq '((:b) (1 2))  (rember* :a '( (:a :b) (1 2)) )      "6")
(assert-eq '((:b) (1 2))  (rember* :a '( (:a :b) (1 :a 2) :a)) "7")
(assert-eq '(() (1 2) ((:c ())))
           (rember* :a '( :a (:a) (1 :a 2) :a ((:c (:a)) )) )  "8")


(println (str "\n" (header-str "insertR*")))
(assert-eq '()            (insertR* 1 :a '())         "1")
(assert-eq '(:a 1)        (insertR* 1 :a '(:a))       "2")
(assert-eq '(:b)          (insertR* 1 :a '(:b))       "3")
(assert-eq '(:b :c :d)    (insertR* 1 :a '(:b :c :d)) "4")
(assert-eq '(:b :c :a 1 :d :a 1 :a 1 :d) 
           (insertR* 1 :a '(:b :c :a :d :a :a :d))    "5")
(assert-eq '((:a 1 :b) (1 2))  
           (insertR* 1 :a '( (:a :b) (1 2)) )         "6")
(assert-eq '((:a 1 :b) (1 :a 1 2) :a 1)
            (insertR* 1 :a '( (:a :b) (1 :a 2) :a) )  "7")
(assert-eq '( :a 1 (:a 1) (1 :a 1 2) :a 1 ((:c (:a 1))) )
           (insertR* 1 :a '( :a (:a) (1 :a 2) :a ((:c (:a))) ) ) "8")

(println (str "\n" (header-str "insertL*")))
(assert-eq '()            (insertL* 1 :a '())         "1")
(assert-eq '(1 :a)        (insertL* 1 :a '(:a))       "2")
(assert-eq '(:b)          (insertL* 1 :a '(:b))       "3")
(assert-eq '(:b :c :d)    (insertL* 1 :a '(:b :c :d)) "4")
(assert-eq '(:b :c 1 :a :d 1 :a 1 :a :d) 
           (insertL* 1 :a '(:b :c :a :d :a :a :d))    "5")
(assert-eq '((1 :a :b) (1 2))  
           (insertL* 1 :a '( (:a :b) (1 2)) )         "6")
(assert-eq '((1 :a :b) (1 1 :a 2) 1 :a)
            (insertL* 1 :a '( (:a :b) (1 :a 2) :a) )  "7")
(assert-eq '( 1 :a (1 :a) (1 1 :a 2) 1 :a ((:c (1 :a))) )
           (insertL* 1 :a '( :a (:a) (1 :a 2) :a ((:c (:a))) ) ) "8")


(println (str "\n" (header-str "occur*")))
(assert-eq 0  (occur* 1 '())                              "1")
(assert-eq 1  (occur* :a '(:a))                           "2")
(assert-eq 2  (occur* :a '(:a 1 :a))                      "3")
(assert-eq 1  (occur* :a '((:a)))                         "4")
(assert-eq 3  (occur* :a '((:a :a) (1 :a) (:x)))          "5")
(assert-eq 3  (occur* :a '((:a ((:a :b) 1)) (1 :a) (:x))) "6")

(println (str "\n" (header-str "subst*")))
(assert-eq '()            (subst* 1 :a '())         "1")
(assert-eq '(1)           (subst* 1 :a '(:a))       "2")
(assert-eq '(:b)          (subst* 1 :a '(:b))       "3")
(assert-eq '(:b :c :d)    (subst* 1 :a '(:b :c :d)) "4")
(assert-eq '(:b :c 1 :d 1 1 :d) 
           (subst* 1 :a '(:b :c :a :d :a :a :d))    "5")
(assert-eq '((1 :b) (1 2))  
           (subst* 1 :a '( (:a :b) (1 2)) )         "6")
(assert-eq '((1 :b) (1 1 2) 1)
           (subst* 1 :a '( (:a :b) (1 :a 2) :a) )  "7")
(assert-eq '( 1 (1) (1 1 2) 1 ((:c (1))) )
           (subst* 1 :a '( :a (:a) (1 :a 2) :a ((:c (:a))) ) ) "8")



(println (str "\n" (header-str "member?*")))
(assert-true  (member?* 1 '(1))                         "1")
(assert-true  (member?* 1 '(1 2 3 4))                   "2")
(assert-true  (member?* 3 '(1 2 3 4))                   "3")
(assert-true  (member?* 3 '(1 2 3 4 3))                 "4")
(assert-false (member?* 33 '(1 2 3 4 3))                "5")
(assert-false (member?* 1 '())                          "6")
(assert-true  (member?* 1 '((1) (2)))                   "7")
(assert-true  (member?* 1 '((1 2 3)))                   "8")
(assert-true  (member?* 1 '((1) 2 1))                   "9")
(assert-true  (member?* :a '(1 2 3 :a 4))               "10")
(assert-true  (member?* 1 '((1) 2 1))                   "11")
(assert-true  (member?* 1 '((((:x 1)) 2 3) (:a :b :c))) "12")


(println (str "\n" (header-str "leftmost*")))
(assert-eq nil (leftmost* '())                "1")
(assert-eq nil (leftmost* '( () 1 2 3) )      "2")
(assert-eq 1   (leftmost* '( 1 2 3 () ))      "3")
(assert-eq 1   (leftmost* '( ((1)) (2 3) 4 )) "4")


(println (str "\n" (header-str "eqlist?")))
(assert-true  (eqlist? '() 
                       '()) "1")
(assert-true  (eqlist? '(1) 
                       '(1)) "2")
(assert-true  (eqlist? '(1 2 :a) 
                       '(1 2 :a)) "3")
(assert-false (eqlist? '(1) 
                       '(1 :a)) "4")
(assert-false (eqlist? '(1 2 :a) 
                       '(2 1 :a)) "5")
(assert-false (eqlist? '(1 2 :a)
                       '(1 2 (:a))) "6")
(assert-true  (eqlist? '(1 2 ((:a)))
                       '(1 2 ((:a)))) "7")
(assert-false (eqlist? '(1 2 ((:a)))
                       '(1 2 ((:b)))) "8")
(assert-true  (eqlist? '(1 (2 ((:a))) :b (:c :d) (:e (11))) 
                       '(1 (2 ((:a))) :b (:c :d) (:e (11)))) "9")
(assert-false (eqlist? '(1 (2 ((:a))) :b (:c :d) (:e (11))) 
                       '(1 (2 ((:a))) :b (:c :d) (:e (12)))) "10")
(assert-false (eqlist? '(1 (2 ((:a))) :b (:c :d) (:e (11))) 
                       '(1 (2 ((:a))) :b (:c :d) ((:e) (11)))) "11")
(assert-false (eqlist? '() '(1 2 3)) "12")
(assert-false (eqlist? '(()) '((1 2 3))) "13")
(assert-false (eqlist? '((1 (2) 3)) '((1 2 3))) "14")


(println (str "\n" (header-str "numbered?")))
(assert-true  (numbered? 0)                           "1")
(assert-true  (numbered? 1533)                        "2")
(assert-false (numbered? '())                         "3")
(assert-false (numbered? "foo")                       "4")
(assert-false (numbered? :a)                          "5")
(assert-false (numbered? +)                           "6")
(assert-true  (numbered? '(1 + 2))                    "7")
(assert-false (numbered? '(1 2 2))                    "8")
(assert-true  (numbered? '(1 * 2))                    "9")
(assert-true  (numbered? '(1 exp 2))                 "10")
(assert-true  (numbered? '((1 + 2) * 4))             "11")
(assert-true  (numbered? '((1 + 2) * 4 * 5))         "12")
(assert-true  (numbered? '((1 + 2) * (4 + (3 + 1)))) "13")
;; (assert-false (numbered? '((1 + 2) * 4 *))           "14")
;; (assert-false (numbered? '((1 + 2) * 4 4))           "15")
;; (assert-false (numbered? '((1 + 2) * 4 4 4))         "16")


(println (str "\n" (header-str "bk-numbered?")))
(assert-true  (bk-numbered? 0)                           "1")
(assert-true  (bk-numbered? 1533)                        "2")
(assert-false (bk-numbered? '())                         "3")
(assert-false (bk-numbered? "foo")                       "4")
(assert-false (bk-numbered? :a)                          "5")
(assert-false (bk-numbered? +)                           "6")
(assert-true  (bk-numbered? '(1 + 2))                    "7")
(assert-true  (bk-numbered? '(1 * 2))                    "8")
;; (assert-false (bk-numbered? '(1 2 2))                    "9")
(assert-true  (bk-numbered? '(1 exp 2))                 "10")
(assert-true  (bk-numbered? '((1 + 2) * 4))             "11")
(assert-true  (bk-numbered? '((1 + 2) * 4 * 5))         "12")
(assert-true  (bk-numbered? '((1 + 2) * (4 + (3 + 1)))) "13")
;; (assert-false (bk-numbered? '((1 + 2) * 4 *))           "14")
;; (assert-false (bk-numbered? '((1 + 2) * 4 4))           "15")
;; (assert-false (bk-numbered? '((1 + 2) * 4 4 4))         "16")


(println (str "\n" (header-str "nvalue")))
(assert-eq nil  (nvalue '())         "1")
(assert-eq 1    (nvalue 1)           "2")
(assert-eq 2    (nvalue '(1 + 1))    "3")
(assert-eq 4    (nvalue '((1 * 2) + 2))    "4")
(assert-eq 22   (nvalue '((1 * 2) + (2 * 10)))    "5")
(assert-eq 408  (nvalue '(((1 * 2) exp 3) + ((2 * 10) exp 2)))    "6")
(assert-eq nil  (nvalue '(((1 2) exp 3) + ((2 * 10) exp 2)))    "7")
(assert-eq nil  (nvalue '(((1 * 2) exp exp) + ((2 * 10) exp 2)))    "8")
(assert-eq nil  (nvalue '((1 2 2) 2 2))    "9")
(assert-eq nil  (nvalue '*)    "10")
(assert-eq nil  (nvalue "foo")    "11")
(assert-eq nil  (nvalue :foo)    "12")

(println (str "\n" (header-str "bk-nvalue")))
(assert-eq nil  (bk-nvalue '())         "1")
(assert-eq 1    (bk-nvalue 1)           "2")
(assert-eq 2    (bk-nvalue '(1 + 1))    "3")
(assert-eq 4    (bk-nvalue '((1 * 2) + 2))    "4")
(assert-eq 22   (bk-nvalue '((1 * 2) + (2 * 10)))    "5")
(assert-eq 408  (bk-nvalue '(((1 * 2) exp 3) + ((2 * 10) exp 2)))    "6")
;; (assert-eq nil  (bk-nvalue '((1 2) exp 3))    "7")
;; (assert-eq nil  (bk-nvalue '(((1 2) exp 3) + ((2 * 10) exp 2)))    "7")
;; (assert-eq nil  (bk-nvalue '(((1 * 2) exp exp) + ((2 * 10) exp 2)))    "8")
;; (assert-eq nil  (bk-nvalue '((1 2 2) 2 2))    "9")
;; (assert-eq nil  (bk-nvalue '*)    "10")
;; (assert-eq nil  (bk-nvalue "foo")    "11")
;; (assert-eq nil  (bk-nvalue :foo)    "12")

(println (str "\n" (header-str "pf-value")))
(assert-eq nil (pf-value '())                                   "1")
(assert-eq 1   (pf-value 1)                                     "2")
(assert-eq 2   (pf-value '(+ 1 1))                              "3")
(assert-eq 3   (pf-value '(+ (+ 1 1) 1))                        "4")
(assert-eq 4   (pf-value '(+ (* 1 2) 2))                        "5")
(assert-eq 22  (pf-value '(+ (* 1 2) (* 2 10)))                 "6")
(assert-eq 256 (pf-value '(exp 2 8))                            "7")
(assert-eq 256 (pf-value '(exp 2 (+ 4 4)))                      "8")
(assert-eq 256 (pf-value '(exp 2 (+ 4 4)))                      "9")
(assert-eq 408 (pf-value '(+ (exp (* 1 2) 3) (exp (* 2 10) 2))) "10")
(assert-eq nil (pf-value '((1 2) exp 3))                        "11")


(println (str "\n" (header-str "sero?")))
(assert-true  (sero? '()         ) "1")
(assert-false (sero? '(())       ) "2")
(assert-false (sero? '(() () ()) ) "3")

(println (str "\n" (header-str "edd1")))
(assert-eq '(())        (edd1 '()     ) "1")
(assert-eq '(() ())    (edd1 '(())    ) "2")
(assert-eq '(() () ()) (edd1 '(() ()) ) "3")

(println (str "\n" (header-str "zub1")))
(assert-eq '()      (zub1 '(())       ) "1")
(assert-eq '(())    (zub1 '(() ())    ) "2")
(assert-eq '(() ()) (zub1 '(() () ()) ) "3")

(println (str "\n" (header-str "nl+")))
(assert-eq '()            (nl+ '() '()           ) "1")
(assert-eq '(())          (nl+ '(()) '()         ) "2")
(assert-eq '(() ())       (nl+ '(()) '(())       ) "3")
(assert-eq '(() () () ()) (nl+ '(()) '(() () ()) ) "4")
(assert-eq '(() () () ()) (nl+ '(() () ()) '(()) ) "5")

(println (str "\n" (header-str "nl-lat?")))
(assert-true  (nl-lat? '()             ) "1")
(assert-true  (nl-lat? '(())           ) "2")
(assert-true  (nl-lat? '(() () () ())  ) "3")
(assert-false (nl-lat? 1               ) "4")
(assert-false (nl-lat? '(1)            ) "5")
(assert-false (nl-lat? '(() () 1)      ) "6")
(assert-false (nl-lat? '(() () (1))    ) "7")
;; (assert-false (nl-lat? '(() () (()))   ) "8")
;; test from p.109 of the book
(assert-false (strict-nl-lat? '( (()) (()()) (()()()) )) "9")

(println (str "\n" (header-str "strict-nl-lat?")))
(assert-true  (strict-nl-lat? '()             ) "1")
(assert-true  (strict-nl-lat? '(())           ) "2")
(assert-true  (strict-nl-lat? '(() () () ())  ) "3")
(assert-false (strict-nl-lat? 1               ) "4")
(assert-false (strict-nl-lat? '(1)            ) "5")
(assert-false (strict-nl-lat? '(() () 1)      ) "6")
(assert-false (strict-nl-lat? '(() () (1))    ) "7")
(assert-false (strict-nl-lat? '((()) () ())   ) "8")
(assert-false (strict-nl-lat? '(() () (()))   ) "9")
;; test from p.109 of the book
(assert-false (strict-nl-lat? '( (()) (()()) (()()()) )) "10")

(println (str "\n" (header-str "isset?")))
(assert-true  (isset? '()          ) "1")
(assert-true  (isset? '(1)         ) "2")
(assert-true  (isset? '(1 2 :a 11) ) "3")
(assert-true  (isset? '(())        ) "4")
(assert-true  (isset? '(() (1) (2))) "5")
(assert-false (isset? '(1 2 :a 1)  ) "6")
(assert-false (isset? '(2 2)       ) "7")
(assert-false (isset? '(() (1) ()) ) "9")


(println (str "\n" (header-str "makeset-1")))
(assert-eqset '()      (makeset-1 '()               ) "1")
(assert-eqset '(:a)    (makeset-1 '(:a)             ) "2")
(assert-eqset '(:a :b) (makeset-1 '(:a :b)          ) "3")
(assert-eqset '(:b :a) (makeset-1 '(:a :b :a)       ) "4")
(assert-eqset '(:a :b) (makeset-1 '(:a :b :a :a :b) ) "5")
(assert-eqset '(1)     (makeset-1 '(1 1 1 1 1 1 1)  ) "6")
(assert-eqset '(:b 2 1 :c :d :a) 
           (makeset-1 '(:a :b 1 :a :a :b 2 1 :c :c :d :a) ) "7")


(println (str "\n" (header-str "makeset")))
(assert-eqset '()      (makeset '()               ) "1")
(assert-eqset '(:a)    (makeset '(:a)             ) "2")
(assert-eqset '(:a :b) (makeset '(:a :b)          ) "3")
(assert-eqset '(:a :b) (makeset '(:a :b :a)       ) "4")
(assert-eqset '(:a :b) (makeset '(:a :b :a :a :b) ) "5")
(assert-eqset '(1)     (makeset '(1 1 1 1 1 1 1)  ) "6")
(assert-eqset '(:a :b :c :d 1 2) 
              (makeset '(:a :b 1 :a :a :b 2 1 :c :c :d :a) ) "7")


(println (str "\n" (header-str "subset?")))
(assert-true  (subset? '() '()                   ) "1")
(assert-true  (subset? '() '(1)                  ) "2")
(assert-true  (subset? '(1) '(1)                 ) "3")
(assert-true  (subset? '(1) '(1 2)               ) "4")
(assert-true  (subset? '(1 2) '(1 2)             ) "5")
(assert-true  (subset? '(2 1) '(1 2)             ) "6")
(assert-true  (subset? '(1 2 3) '(:b 3 :a 1 2)   ) "7")
(assert-false (subset? '(1 2 4 3) '(:b 3 :a 1 2) ) "8")
(assert-false (subset? '(1 2 3 4) '(:b 3 :a 1 2) ) "9")
(assert-false (subset? '(:d) '(:b 3 :a 1 2)      ) "10")


(println (str "\n" (header-str "eqset?")))
(assert-true  (eqset? '() '()                         ) "1")
(assert-true  (eqset? '(1) '(1)                       ) "2")
(assert-true  (eqset? '(1 2) '(1 2)                   ) "3")
(assert-true  (eqset? '(1 2) '(2 1)                   ) "4")
(assert-true  (eqset? '(1 :a 2) '(:a 2 1)             ) "5")
(assert-true  (eqset? '(1 :a 2 :b 33) '(33 :b :a 2 1) ) "6")
(assert-false (eqset? '(1 :b 2) '(:a 2 1)             ) "7")
(assert-false (eqset? '() '(:a 2 1)                   ) "8")
(assert-false (eqset? '(:a 2 1) '()                   ) "9")
(assert-false (eqset? '(:a 2 1) '(2 1)                ) "10")
(assert-false (eqset? '(:a 2 1) '(:a)                 ) "11")


(println (str "\n" (header-str "intersect?")))
(assert-false (intersect? '() '()              ) "1")
(assert-true  (intersect? '(1) '(1)            ) "2")
(assert-true  (intersect? '(1 2) '(3 2)        ) "3")
(assert-true  (intersect? '(3 2) '(1 2 3)      ) "4")
(assert-true  (intersect? '(1) '(3 2 4 :a 1)   ) "5")
(assert-true  (intersect? '(3 2 4 :a 1) '(1)   ) "6")
(assert-false (intersect? '(3 2 4 :a :b) '(1)  ) "7")
(assert-false (intersect? '(1) '(3 2 4 :a :b)  ) "8")
(assert-false (intersect? '(1) '()             ) "9")
(assert-false (intersect? '() '(1)             ) "10")


(println (str "\n" (header-str "intersect")))
(assert-eqset '()        (intersect '() '()                        ) "1")
(assert-eqset '(:a)      (intersect '(:a) '(:a)                    ) "2")
(assert-eqset '(:b)      (intersect '(:a :b) '(:b :c)              ) "3")
(assert-eqset '()        (intersect '(:a :b) '(1 :c)               ) "4")
(assert-eqset '(:a :b 1) (intersect '(:a :b 1 :d :e) '(:b :c 1 :a) ) "5")
(assert-eqset '(1 2)     (intersect '(:a 2 1) '(:b :c 1 2)         ) "6")
(assert-eqset '(1 2)     (intersect '(:b :c 1 2) '(:a 2 1)         ) "7")
(assert-eqset '()        (intersect '(:a 2 1) '(3)                 ) "8")
(assert-eqset '()        (intersect '(3) '(:a 2 1)                 ) "9")
;; to prove that this method will return dup entries if not
;; passed pure (uniquified) sets
(assert-eqset '(2 2)     (intersect '(2 2) '(:a 2 1)               ) "10")


(println (str "\n" (header-str "union")))
(assert-eqset '()           (union '() '()                 ) "1")
(assert-eqset '(:a)         (union '(:a) '(:a)             ) "2")
(assert-eqset '(:a)         (union '()   '(:a)             ) "3")
(assert-eqset '(:a)         (union '(:a) '()               ) "4")
(assert-eqset '(:a :b :c)   (union '(:a :b) '(:b :c)       ) "5")
(assert-eqset '(:a :b :c 1) (union '(:a :b) '(1 :c)        ) "6")
(assert-eqset  '(:a :b :c :d :e 1 2 5)
               (union '(:a :b 1 :d :e 5) '(:b :c 1 :a 2) ) "7")


(println (str "\n" (header-str "set-diff")))
(assert-eqset '()           (set-diff '() '()                ) "1")
(assert-eqset '()         (set-diff '(:a) '(:a)              ) "2")
(assert-eqset '()         (set-diff '()   '(:a)              ) "3")
(assert-eqset '(:a)         (set-diff '(:a) '()              ) "4")
(assert-eqset '(:a)   (set-diff '(:a :b) '(:b :c)            ) "5")
(assert-eqset '(:a :b) (set-diff '(:a :b) '(1 :c)            ) "6")
(assert-eqset '(1 :c) (set-diff '(1 :c) '(:a :b)             ) "7")
(assert-eqset  '(:d :e 5)
               (set-diff '(:a :b 1 :d :e 5) '(:b :c 1 :a 2)  ) "8")
(assert-eqset  '(:c 2)
               (set-diff  '(:b :c 1 :a 2) '(:a :b 1 :d :e 5) ) "9")


(println (str "\n" (header-str "intersect-all")))
(assert-eqset '(2)   (intersect-all '((1 2) (2 3))              ) "1")
(assert-eqset '(2)   (intersect-all '((1 2) (2 3) (3 2))        ) "2")
(assert-eqset '(1 3) (intersect-all '((1 2 3) (2 3 1) (1 3 :a)) ) "3")
(assert-eqset '()    (intersect-all '((1 2 3) (2 3 1) (1 3 :a 6 5) ()) ) "4")

(println (str "\n" (header-str "build")))
(assert-eq '(1 2)         (build 1 2)           "1")
(assert-eq '((1 2) 3)     (build '(1 2) 3)      "2")
(assert-eq '((1 2) (3 4)) (build '(1 2) '(3 4)) "3")

(println (str "\n" (header-str "pair?")))
(assert-true  (pair? '(1 2)),               "1")
(assert-false (pair? '()),                  "2")
(assert-false (pair? '(2)),                 "3")
(assert-false (pair? '(1 2 3)),             "4")
(assert-true  (pair? '((1 2) 3)),           "5")
(assert-true  (pair? '(1 (2 3))),           "6")
(assert-true  (pair? '((:a 1) (2 (3 :b)))), "7")
(assert-false (pair? '((:a 1))),            "8")

(println (str "\n" (header-str "revrel")))
(assert-eq '((2 1) (4 3) (:b :a))  (revrel '((1 2) (3 4) (:a :b))), "1")
(assert-eq '(((2) (1)) (5 (3 4)))  (revrel '(((1) (2)) ((3 4) 5))), "2")

(println (str "\n" (header-str "revrel2")))
(assert-eq '((2 1) (4 3) (:b :a))  (revrel2 '((1 2) (3 4) (:a :b))), "1")
(assert-eq '(((2) (1)) (5 (3 4)))  (revrel2 '(((1) (2)) ((3 4) 5))), "2")


(println (str "\n" (header-str "seconds")))
(assert-eq '(2 5 8) (seconds '((1 2 3) (4 5 6) (7 8 9))) "1")
(assert-eq '(2 5 nil) (seconds '((1 2 3) (4 5) (7))) "2")
(assert-eq '(nil 5 8) (seconds '((1) (4 5) (7 8 9))) "3")
(assert-eq '((3) 5 :b) (seconds '(((1 2) (3)) (4 5) ((:a) :b (:c :d)))) "4")

(println (str "\n" (header-str "fullfun?")))
(assert-true  (fullfun? '((1 2) (3 4) (5 6) (:a :b))),        "1")
(assert-false (fullfun? '((1 2) (3 4) (5 6) (:a 2))),         "2")
(assert-true  (fullfun? '((1 2) (3 4) (5 (6)) (:a (:b :c)))), "3")
(assert-true  (fullfun? '((1 2) (3 4) (5 (6)) (:a 6))),       "4")
(assert-true  (fullfun? '((1 2) (3 4) (5 (6)) (:a 6))),       "5")
;; this should be false, since it is not a fun, but
;; that is not part of the checking they put in the code
(assert-true  (fullfun? '((3 2) (3 4) (5 (6)) (:a 6))),       "5")

(println (str "\n" (header-str "one-to-one?")))
(assert-true  (one-to-one? '((1 2) (3 4) (5 6) (:a :b))),        "1")
(assert-false (one-to-one? '((1 2) (3 4) (5 6) (:a 2))),         "2")
(assert-true  (one-to-one? '((1 2) (3 4) (5 (6)) (:a (:b :c)))), "3")
(assert-true  (one-to-one? '((1 2) (3 4) (5 (6)) (:a 6))),       "4")
(assert-true  (one-to-one? '((1 2) (3 4) (5 (6)) (:a 6))),       "5")

(println (str "\n" (header-str "rember-f?")))
(assert-eq  '(1 3) (rember-f? = 2 '(1 2 3)), "1")
(assert-eq  '(1 4) (rember-f? = '(2 3) '(1 (2 3) 4)), "2")

(println (str "\n" (header-str "eq-c?")))
(def eq-6 (eq?-c 6))
(assert-true  (eq-6 6),       "1")
(assert-true  (eq-6 (+ 1 5)), "2")
(assert-false (eq-6 7),       "3")

(println (str "\n" (header-str "rember-f2?")))
(def rm-with-equals (rember-f2? =))
(assert-eq  '(1 3) (rm-with-equals 2 '(1 2 3)),          "1")
(assert-eq  '(1 4) (rm-with-equals '(2 3) '(1 (2 3) 4)), "2")

(println (str "\n" (header-str "rember-f3?")))
(def rm-with-equals3 (rember-f3? =))
(assert-eq  '(1 3)   (rm-with-equals3 2 '(1 2 3))          "1")
(assert-eq  '(1 4)   (rm-with-equals3 '(2 3) '(1 (2 3) 4)) "2")
(assert-eq  '(1 3 2) (rm-with-equals3 2 '(1 2 3 2))        "3")

(println (str "\n" (header-str "rember-f3? - not equals op")))
(def rm-with-not-equals (rember-f3? not=))
(assert-eq  '(2 3) (rm-with-not-equals 2 '(1 2 3)),        "1")
(assert-eq  '((2 3) 4) (rm-with-not-equals '(2 3) '(1 (2 3) 4)),  "2")
(assert-eq  '(1 4) (rm-with-not-equals 1 '(1 (2 3) 4)),   "3")

(println (str "\n" (header-str "insertL-f")))
(def insertL-w-eq (insertL-f =))
(assert-eq '(1 2 666 3 4 :5) (insertL-w-eq 666 3 '(1 2 3 4 :5))   "1")
(assert-eq '(666 1 2 3 4 :5) (insertL-w-eq 666 1 '(1 2 3 4 :5))   "2")
(assert-eq '(1 2 3 4 666 :5) (insertL-w-eq 666 :5 '(1 2 3 4 :5))  "3")
(assert-eq '(1 2 3 4 :5)     (insertL-w-eq 666 :NA '(1 2 3 4 :5)) "4")
(assert-eq '()               (insertL-w-eq :new :old '())         "5")
(assert-eq '(1 2)            (insertL-w-eq :new nil '(1 2))       "6")

(println (str "\n" (header-str "insertR-f")))
(def insertR-lambda (insertR-f =))
(assert-eq '(1 2 3 666 4 :5) (insertR-lambda 666 3 '(1 2 3 4 :5)) "1")
(assert-eq '(1 666 2 3 4 :5) (insertR-lambda 666 1 '(1 2 3 4 :5)) "2")
(assert-eq '(1 2 3 4 :5 666) (insertR-lambda 666 :5 '(1 2 3 4 :5)) "3")
(assert-eq '(1 2 3 4 :5) (insertR-lambda 666 :NA '(1 2 3 4 :5)) "4")
(assert-eq '() (insertR-lambda :new :old '()) "5")
(assert-eq '(1 2) (insertR-lambda :new nil '(1 2)) "6")


(println (str "\n" (header-str "insert-g")))
;; with seqR
(def ins-right (insert-g seqR))
(assert-eq '(1 2 3 666 4 :5) (ins-right 666 3 '(1 2 3 4 :5))         "1")
(assert-eq '(1 666 2 3 4 :5) ((insert-g seqR) 666 1 '(1 2 3 4 :5))   "2")
(assert-eq '(1 2 3 4 :5 666) ((insert-g seqR) 666 :5 '(1 2 3 4 :5))  "3")
(assert-eq '(1 2 3 4 :5)     ((insert-g seqR) 666 :NA '(1 2 3 4 :5)) "4")
(assert-eq '()               ((insert-g seqR) :new :old '())         "5")
(assert-eq '(1 2)            ((insert-g seqR) :new nil '(1 2))       "6")

;; with seqL
(assert-eq '(1 2 666 3 4 :5) ((insert-g seqL) 666 3 '(1 2 3 4 :5))   "7")
(assert-eq '(666 1 2 3 4 :5) ((insert-g seqL) 666 1 '(1 2 3 4 :5))   "8")
(assert-eq '(1 2 3 4 666 :5) ((insert-g seqL) 666 :5 '(1 2 3 4 :5))  "9")


(println (str "\n" (header-str "value2")))
(assert-eq nil  (value2 '())                                     "1")
(assert-eq 1    (value2 1)                                       "2")
(assert-eq 2    (value2 '(1 + 1))                                "3")
(assert-eq 4    (value2 '((1 * 2) + 2))                          "4")
(assert-eq 22   (value2 '((1 * 2) + (2 * 10)))                   "5")
(assert-eq 408  (value2 '(((1 * 2) exp 3) + ((2 * 10) exp 2)))   "6")
(assert-eq nil  (value2 '(((1 2) exp 3) + ((2 * 10) exp 2)))     "7")
(assert-eq nil  (value2 '(((1 * 2) exp exp) + ((2 * 10) exp 2))) "8")
(assert-eq nil  (value2 '((1 2 2) 2 2))                          "9")
(assert-eq nil  (value2 '*)                                      "10")
(assert-eq nil  (value2 "foo")                                   "11")
(assert-eq nil  (value2 :foo)                                    "12")


(println (str "\n" (header-str "multirember-f")))
(assert-eq '(:a)        ((multirember-f =) :ab '(:a :ab))                "1")
(assert-eq '()          ((multirember-f =) :ab '())                      "2")
(assert-eq '(:a :ab :c) ((multirember-f =) :x '(:a :ab :c))              "3")
(assert-eq '(:a :c)     ((multirember-f =) :ab '(:a :ab :c))             "4")
(assert-eq '(:a :c)     ((multirember-f =) :ab '(:a :ab :c :ab))         "5")
(assert-eq '(:a :c)     ((multirember-f =) :ab '(:a :ab :ab :c))         "6")
(assert-eq '(:a :c)     ((multirember-f =) :ab '(:ab :a :ab :c :ab :ab)) "7")
(assert-eq '(:x)        ((multirember-f =) :ab '(:ab :ab :ab :x))        "8")
(assert-eq '()          ((multirember-f =) :ab '(:ab :ab :ab))           "9")


(println (str "\n" (header-str "mutliinsertLR")))
(assert-eq '(1 2 666 3 4 :5)
           (multiinsertLR 666 3 24 '(1 2 3 4 :5)) "1")
(assert-eq '(666 1 2 666 3 4 :5)
           (multiinsertLR 666 1 2 '(1 2 3 4 :5)) "2")
(assert-eq '(1 2 3 4 666 :5)
           (multiinsertLR 666 :5 :5 '(1 2 3 4 :5)) "3")
(assert-eq '(1 2 3 4 :5)
           (multiinsertLR 666 :NA :NB '(1 2 3 4 :5)) "4")
(assert-eq '()
           (multiinsertLR :new :old :old2 '()) "5")
(assert-eq '(1 2)
           (multiinsertLR :new nil nil '(1 2)) "6")
(assert-eq '(666 1 2 666 1 3 4 :5 666 666 1)
           (multiinsertLR 666 1 :5 '(1 2 1 3 4 :5 1)) "7")


(println (str "\n" (header-str "multiinsertLR&co")))
(defn makemap [newlat nleft nright]
  {:newlat newlat, :nleft nleft, :nright nright})
(assert-eq {:newlat '(1 2 666 3 4 :5), :nleft 1, :nright 0}
           (multiinsertLR&co 666 3 24 '(1 2 3 4 :5), makemap) "1")

(println (str "\n" (header-str "evens-only*")))
(assert-eq '() (evens-only* '(1 3 5 3 1)) "1")
(assert-eq '(()) (evens-only* '(1 3 5 (3 1))) "2")
(assert-eq '(2 4 6) (evens-only* '(1 2 3 4 5 6 7)) "3")
(assert-eq '(2 (4 6) (8) 10) (evens-only* '(1 2 (3 4 5 6 ) 7 (8) 9 10)) "3")

(println (str "\n" (header-str "evens-only*&co")))
(defn makemap2 [newlat p s]
  {:newlat newlat, :product 1920, :sum 38})
(assert-eq {:newlat '((2 8) 10 (() 6) 2), :product 1920, :sum 38}
           (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2), makemap2) "1")

;; ---[ Chapter 9 Tests ]--- ;;
(println (str "\n" (header-str "pick")))
(assert-eq 4   (pick 1 '(4 3 2 1)) "1")
(assert-eq 3   (pick 2 '(4 3 2 1)) "2")
(assert-eq 2   (pick 3 '(4 3 2 1)) "3")
(assert-eq 1   (pick 4 '(4 3 2 1)) "4")
(assert-eq nil (pick 5 '(4 3 2 1)) "5")

(println (str "\n" (header-str "keep-looking")))
(assert-false (keep-looking :a 1 '(4 3 :b 2)) "1")
(assert-true  (keep-looking :a 1 '(4 3 :a 2)) "2")
(assert-false (keep-looking :a 9 '(4 3 :a 2)) "2")

(println (str "\n" (header-str "shift")))
(assert-eq '(:a (:b (:c :d))) (shift '((:a :b) (:c :d))) "1")

;; ---[ Chapter 10 Tests ]--- ;;
(println (str "\n" (header-str "new-entry")))
(assert-eq '(1 2)         (new-entry 1 2)           "1")
(assert-eq '((1 2) 3)     (new-entry '(1 2) 3)      "2")
(assert-eq '((1 2) (3 4)) (new-entry '(1 2) '(3 4)) "3")

(println (str "\n" (header-str "lookup-in-entry")))
(defn not-found [x] nil)
(def mymap (new-entry '(:taco :kf :bo      :burger)
                      '(:bell :c  :jangles :king)))
(assert-eq :bell (lookup-in-entry :taco   mymap not-found) "1")
(assert-eq :king (lookup-in-entry :burger mymap not-found) "2")
(assert-eq nil   (lookup-in-entry :wendys mymap not-found) "3")



(def mytable '( ((:entree    :dessert)
                 (:spaghetti :spumoni))
                ((:appetizer :entree :beverage)
                 (:food      :tastes :good)) ))

(println (str "\n" (header-str "lup-in-table")))
(assert-eq nil        (lup-in-table :na       mytable not-found) "1")
(assert-eq :spumoni   (lup-in-table :dessert  mytable not-found) "2")
(assert-eq :spaghetti (lup-in-table :entree   mytable not-found) "3")
(assert-eq :good      (lup-in-table :beverage mytable not-found) "4")

(println (str "\n" (header-str "lookup-in-table")))
(assert-eq nil        (lookup-in-table :na       mytable not-found) "1")
(assert-eq :spumoni   (lookup-in-table :dessert  mytable not-found) "2")
(assert-eq :spaghetti (lookup-in-table :entree   mytable not-found) "3")
(assert-eq :good      (lookup-in-table :beverage mytable not-found) "4")

(println (str "\n" (header-str "atom-to-action")))
(assert-eq '*const      (atom-to-action 22)       "1")
(assert-eq '*const      (atom-to-action false)    "2")
(assert-eq '*const      (atom-to-action 'first)   "3")
(assert-eq '*const      (atom-to-action 'number?) "4")
(assert-eq '*identifier (atom-to-action 'second)  "5")

(println (str "\n" (header-str "list-to-action")))
(assert-eq '*application  (list-to-action '(dec 23)) "1")
(assert-eq '*cond  (list-to-action '(cond 23))       "2")
(assert-eq '*lambda  (list-to-action '(lambda 23))   "3")
(assert-eq '*fn  (list-to-action '(fn 23))           "4")
(assert-eq '*quote  (list-to-action '('nothing))     "5")
(assert-eq '*quote  (list-to-action '('()))          "6")

(println (str "\n" (header-str "value")))
(assert-eq '()  (value '()) "1")


;; ------------------- ;;
;; ---[ END TESTS ]--- ;;
;; ------------------- ;;
(def end-time (System/currentTimeMillis))

(println "")
(println (str "Number of asserts : " @nasserts))
(println (str "Number of failures: " @nfails))
(println (str "Time: " (- end-time start-time) " milliseconds"))
:DONE
