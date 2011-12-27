(ns seasoned-schemer.test.core
  (:use [seasoned-schemer.core])
  (:use [clojure.test]))

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

(defn rt []
  (use 'seasoned-schemer.test.core :reload-all)
  (use 'seasoned-schemer.core :reload-all)
  (load-file "test/seasoned_schemer/test/core.clj")
  (run-tests 'seasoned-schemer.test.core))

