(def fail-tok "\n>>>>> FAIL: ")
(defn header-str [func-name] (str "======= " func-name " tests ======="))

;; ---[ assert functions ]--- ;;
(defn assert-true [bool msg]
  (if bool
    (print ".")
    (println (str fail-tok msg))))

(defn assert-false [bool msg]
  (assert-true (not bool) msg))

(defn assert-eq [exp act msg]
  (if (= exp act)
    (print ".")
    (println (str fail-tok msg "; exp = " exp "; act = " act))))

;; ---[ tests ]--- ;;
(def start-time (System/currentTimeMillis))

(println (header-str "atom?"))
(assert-true  (atom? 1)           "1")
(assert-true  (atom? :1)          "2")
(assert-false (atom? '())         "3")
(assert-false (atom? '(1 2))      "4")
(assert-true  (atom? "taco bell") "5")


(println (str "\n" (header-str "lat?")))
(assert-true  (lat? '(1)) "1")
(assert-true  (lat? '(1 2 3 5)) "2")
(assert-true  (lat? '(1 :2)) "3")
(assert-false (lat? '(1 (1))) "4")
(assert-false (lat? '((1))) "5")
(assert-false (lat? '(())) "6")


(println (str "\n" (header-str "member?")))
(assert-true  (member? 1 '(1)) "1")
(assert-true  (member? 1 '(1 2 3 4)) "2")
(assert-true  (member? 3 '(1 2 3 4)) "3")
(assert-true  (member? 3 '(1 2 3 4 3)) "4")
(assert-false (member? 33 '(1 2 3 4 3)) "5")
(assert-false (member? 1 '()) "6")
(assert-false (member? 1 '((1) (2))) "7")
(assert-false (member? 1 '((1 2 3))) "8")
(assert-true  (member? 1 '((1) 2 1)) "9")
(assert-true  (member? :a '(1 2 3 :a 4)) "10")
(assert-true  (member? '(1) '((1) 2 1)) "11")
(assert-true  (member? '(:a :b :c) '((1 2 3) (:a :b :c))) "12")


(println (str "\n" (header-str "rember")))
(assert-eq '(:a) (rember1 :ab '(:a :ab)) "1")
(assert-eq '(:a) (rember1-alt :ab '(:a :ab)) "1-alt")
(assert-eq '(:a) (rember :ab '(:a :ab)) "1-2")

(assert-eq '() (rember1 :ab '()) "2")
(assert-eq nil (rember1-alt :ab '()) "2-alt")
(assert-eq '() (rember :ab '()) "2-2")

(assert-eq '(:a :ab :c) (rember1 :x '(:a :ab :c)) "3")
(assert-eq '(:a :ab :c) (rember1-alt :x '(:a :ab :c)) "3-alt")
(assert-eq '(:a :ab :c) (rember :x '(:a :ab :c)) "3-2")

(assert-eq '(:a :c :ab) (rember1 :ab '(:a :ab :c :ab)) "4")
(assert-eq '(:a :c :ab) (rember1-alt :ab '(:a :ab :c :ab)) "4-alt")
(assert-eq '(:a :c :ab) (rember :ab '(:a :ab :c :ab)) "4-2")


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
(assert-eq '(1 2 666 4 :5) (subst 666 3 '(1 2 3 4 :5)) "1")
(assert-eq '(666 2 3 4 :5) (subst 666 1 '(1 2 3 4 :5)) "2")
(assert-eq '(1 2 3 4 666) (subst 666 :5 '(1 2 3 4 :5)) "3")
(assert-eq '(1 2 3 4 :5) (subst 666 :NA '(1 2 3 4 :5)) "4")
(assert-eq '() (subst :new :old '()) "5")
(assert-eq '(1 2) (subst :new nil '(1 2)) "6")
;; this one returns the original list, bcs it is not a lat
;; technical subst doesn't have to work on lat's, but that's
;; how the Little Schemer has it, so I'm following it
(assert-eq '((1 2) (3 4) :5)
           (subst '(666 :a) '(3 4) '((1 2) (3 4) :5))
           "7")

(println (str "\n" (header-str "subst2")))
(assert-eq '(1 2 666 4 :5) (subst2 666 3 4 '(1 2 3 4 :5)) "1")
(assert-eq '(666 2 3 4 :5) (subst2 666 1 4 '(1 2 3 4 :5)) "2")
(assert-eq '(666 1 2 3 4 :5) (subst2 666 1 4 '(4 1 2 3 4 :5)) "3")
(assert-eq '(1 2 3 4 666) (subst2 666 :4 :5 '(1 2 3 4 :5)) "4")
(assert-eq '(1 2 3 4 :5) (subst2 666 :NA :NA2 '(1 2 3 4 :5)) "5")
(assert-eq '() (subst2 :new :o1 :o2 '()) "6")
(assert-eq '(1 2) (subst2 :new nil nil '(1 2)) "7")
;; this one returns the original list, bcs it is not a lat
;; technical subst2 doesn't have to work on lat's, but that's
;; how the Little Schemer has it, so I'm following it
(assert-eq '((1 2) (3 4) :5)
           (subst2 '(666 :a) '(3 4) :abc '((1 2) (3 4) :5))
           "8")


(println (str "\n" (header-str "multirember")))
(assert-eq '(:a)        (multirember :ab '(:a :ab)) "1")
(assert-eq '()          (multirember :ab '()) "2")
(assert-eq '(:a :ab :c) (multirember :x '(:a :ab :c)) "3")
(assert-eq '(:a :c)     (multirember :ab '(:a :ab :c)) "4")
(assert-eq '(:a :c)     (multirember :ab '(:a :ab :c :ab)) "5")
(assert-eq '(:a :c)     (multirember :ab '(:a :ab :ab :c)) "6")
(assert-eq '(:a :c)     (multirember :ab '(:ab :a :ab :c :ab :ab)) "7")
(assert-eq '(:x) (multirember :ab '(:ab :ab :ab :x)) "8")
(assert-eq '() (multirember :ab '(:ab :ab :ab)) "9")



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

;;;;;;;;;;;;;;;;;;;;;;;;
(def end-time (System/currentTimeMillis))

(println (str "\nTime: " (- end-time start-time) " milliseconds"))
:DONE
