(ns seasoned-schemer.test.core
  (:use [seasoned-schemer.core])
  (:use [clojure.test]))

;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 11 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

(deftest test-two-in-a-row
  (is (= true (two-in-a-row?-v1 [:a :b [1 2] :c :c :d])))
  (is (= false (two-in-a-row?-v1 [:a :b [1 2] :c :d])))
  (is (= false (two-in-a-row?-v1 [[2]])))
  (is (= false (two-in-a-row?-v1 '())))

  (is (= true  (two-in-a-row? [:a :b [1 2] :c :c :d])))
  (is (= false (two-in-a-row? [:a :b [1 2] :c :d])))
  (is (= false (two-in-a-row? [[2]])))
  (is (= false (two-in-a-row? '())))

  (is (= true  (two-in-a-row-recur? [:a :b [1 2] :c :c :d])))
  (is (= false (two-in-a-row-recur? [:a :b [1 2] :c :d])))
  (is (= false (two-in-a-row-recur? [[2]])))
  (is (= false (two-in-a-row-recur? '())))
  )

(deftest test-sum-of-prefixes
  (is (= [1 2 3 4 5]    (sum-of-prefixes [1 1 1 1 1])))
  (is (= [2 3 12 29 29] (sum-of-prefixes [2 1 9 17 0])))
  (is (= [55]           (sum-of-prefixes [55])))
  (is (= []             (sum-of-prefixes [])))
  
  (is (= [1 2 3 4 5]    (sum-of-prefixes-recur [1 1 1 1 1])))
  (is (= [2 3 12 29 29] (sum-of-prefixes-recur [2 1 9 17 0])))
  (is (= [55]           (sum-of-prefixes-recur [55])))
  (is (= []             (sum-of-prefixes-recur [])))
  )

(deftest test-pick
  (is (= 12   (pick 2 [10 12 :b])))
  (is (= :b   (pick 3 [10 12 :b])))
  (is (= [10] (pick 1 [[10] 12 :b])))
  (try
   (pick 0 [[10] 12 :b])
   (is false "should not get here")
   (catch Throwable t
     (is (= (.toString (class t)) (.toString AssertionError)))
     ))
  )

(deftest test-scramble
  (is (= [1 1 1 1 1 4 1 1 1 9]   (scramble [1 1 1 3 4 2 1 1 9 2])))
  (is (= [1 1 1 1 1 1 1 1 1]     (scramble [1 2 3 4 5 6 7 8 9])))
  (is (= [1 1 1 1 1 1 1 1 2 8 2] (scramble [1 2 3 1 2 3 4 1 8 2 10])))
  )

(deftest test-scramble-recur-list
  (is (= [1 1 1 1 1 4 1 1 1 9]   (scramble-recur-list [1 1 1 3 4 2 1 1 9 2])))
  (is (= [1 1 1 1 1 1 1 1 1]     (scramble-recur-list [1 2 3 4 5 6 7 8 9])))
  (is (= [1 1 1 1 1 1 1 1 2 8 2] (scramble-recur-list [1 2 3 1 2 3 4 1 8 2 10])))
  )

(deftest test-scramble-recur-vec
  (is (= [1 1 1 1 1 4 1 1 1 9]   (scramble-recur-vec [1 1 1 3 4 2 1 1 9 2])))
  (is (= [1 1 1 1 1 1 1 1 1]     (scramble-recur-vec [1 2 3 4 5 6 7 8 9])))
  (is (= [1 1 1 1 1 1 1 1 2 8 2] (scramble-recur-vec [1 2 3 1 2 3 4 1 8 2 10])))
  )

;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 12 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

(deftest test-multirember-ch3
  (is (= [:shrimp :salad :salad :and]
           (multirember-ch3 :tuna [:shrimp :salad :tuna :salad :and :tuna]))))

(deftest test-multirember-ch3-recur
  (is (= [:shrimp :salad :salad :and]
           (multirember-ch3-recur :tuna [:shrimp :salad :tuna :salad :and :tuna]))))

(deftest test-multirember-letrec
  (is (= [:shrimp :salad :salad :and]
           (multirember-letrec :tuna [:shrimp :salad :tuna :salad :and :tuna]))))

(deftest test-member?
  (is (= true  (member? :c [1 2 :c :d])))
  (is (= true  (member? 1  [1 2 :c :d])))
  (is (= false (member? :e [1 2 :c :d])))
  )

(deftest test-member?-recur
  (is (= true  (member?-recur :c [1 2 :c :d])))
  (is (= true  (member?-recur 1  [1 2 :c :d])))
  (is (= false (member?-recur :e [1 2 :c :d])))
  )

;; helper func to quickly run all tests from the REPL
(defn rt []
  (use 'seasoned-schemer.test.core :reload-all)
  (use 'seasoned-schemer.core :reload-all)
  (load-file "test/seasoned_schemer/test/core.clj")
  (run-tests 'seasoned-schemer.test.core))

;; (println (seasoned-schemer.macros/letrec [ev? (fn [n] (if (zero? n) true (od? (dec n))))
;;                                           od? (fn [n] (if (zero? n) false (ev? (dec n))))]
;;                                          (od? 11)))

