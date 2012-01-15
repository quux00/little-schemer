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
(deftest test-member?-letrec
  (is (= true  (member?-letrec :c [1 2 :c :d])))
  (is (= true  (member?-letrec 1  [1 2 :c :d])))
  (is (= false (member?-letrec :e [1 2 :c :d])))
  )

;; union tests
(deftest test-union
  (is (= [1 2 3 4 5] (sort (union [1 2] [3 4 5]))))
  (is (= [1 2 3 4 5] (sort (union [1 2] [3 4 5 1 2]))))
  (is (= [3 4 5]     (sort (union [] [3 4 5]))))
  )

(deftest test-union-recur
  (is (= [1 2 3 4 5] (sort (union-recur [1 2] [3 4 5]))))
  (is (= [1 2 3 4 5] (sort (union-recur [1 2] [3 4 5 1 2]))))
  (is (= [3 4 5]     (sort (union-recur [] [3 4 5]))))
  )

(deftest test-union-letrec
  (is (= [1 2 3 4 5] (sort (union-letrec [1 2] [3 4 5]))))
  (is (= [1 2 3 4 5] (sort (union-letrec [1 2] [3 4 5 1 2]))))
  (is (= [3 4 5]     (sort (union-letrec [] [3 4 5]))))
  )

(deftest test-union-with-fn
  (is (= [1 2 3 4 5] (sort (union-with-fn [1 2] [3 4 5]))))
  (is (= [1 2 3 4 5] (sort (union-with-fn [1 2] [3 4 5 1 2]))))
  (is (= [3 4 5]     (sort (union-with-fn [] [3 4 5]))))
  )

;; sum of prefixes with sum-b as hidden inner method
(deftest test-sum-of-prefixes2
  (is (= [1 2 3 4 5]    (sum-of-prefixes2 [1 1 1 1 1])))
  (is (= [2 3 12 29 29] (sum-of-prefixes2 [2 1 9 17 0])))
  (is (= [55]           (sum-of-prefixes2 [55])))
  (is (= []             (sum-of-prefixes2 [])))
  )

(deftest test-sum-of-prefixes2-with-fn
  (is (= [1 2 3 4 5]    (sum-of-prefixes2-with-fn [1 1 1 1 1])))
  (is (= [2 3 12 29 29] (sum-of-prefixes2-with-fn [2 1 9 17 0])))
  (is (= [55]           (sum-of-prefixes2-with-fn [55])))
  (is (= []             (sum-of-prefixes2-with-fn [])))
  )

(deftest test-scramble2
  (is (= [1 1 1 1 1 4 1 1 1 9]   (scramble2 [1 1 1 3 4 2 1 1 9 2])))
  (is (= [1 1 1 1 1 1 1 1 1]     (scramble2 [1 2 3 4 5 6 7 8 9])))
  (is (= [1 1 1 1 1 1 1 1 2 8 2] (scramble2 [1 2 3 1 2 3 4 1 8 2 10])))
  )

;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 13 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

(deftest test-intersect
  (is (= [:a :c :e] (intersect [:a :b :c :d :e] [1 :a :c :e 3])))
  (is (= [] (intersect [:a :b :c :d :e] [1 2 3 4])))
  (is (= [:a] (intersect [:a] [:f :g :h :a 1])))
  )

(deftest test-intersect-all
  (is (= [:a] (intersect-all [[:a :b :c] [:d :a :e] [:f :g :h :a 1]])))
  (is (= [] (intersect-all [[:a :b :c] [:d :a :e] [:f :g :h 2 1]])))
  )

(deftest test-rember-beyond-first
  (is (= [:noodles :spag :spa :bean-thread]
           (rember-beyond-first :roots [:noodles :spag :spa :bean-thread :roots :po :yam :others :rice])))
  
  (is (= [:noodles :spag :spa :bean-thread :roots :po :yam :others :rice]
           (rember-beyond-first :na [:noodles :spag :spa :bean-thread :roots :po :yam :others :rice])))  

  (is (= [:a :b :c]
           (rember-beyond-first :d [:a :b :c :d :e :f :d :g :h])))
  )

(deftest test-rember-beyond-first-recur
  (is (= [:noodles :spag :spa :bean-thread]
           (rember-beyond-first-recur :roots [:noodles :spag :spa :bean-thread :roots :po :yam :others :rice])))
  
  (is (= [:noodles :spag :spa :bean-thread :roots :po :yam :others :rice]
           (rember-beyond-first-recur :na [:noodles :spag :spa :bean-thread :roots :po :yam :others :rice])))  

  (is (= [:a :b :c]
           (rember-beyond-first-recur :d [:a :b :c :d :e :f :d :g :h])))
  )

(deftest test-rember-upto-last-recur
  (is (= [:d :e :f] (rember-upto-last-recur :c [:a :b :c :d :e :f])))
  (is (= [:g :h :i] (rember-upto-last-recur :c [:a :b :c :d :e :f :c :g :h :i])))
  (is (= [1 2 3 4 5] (rember-upto-last-recur :c [1 2 3 4 5])))
  )


;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 14 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

(deftest test-leftmost
  (is (= :a (leftmost [:a :b [:c :d]])))
  (is (= :a (leftmost [[:a :b] [:c :d]])))
  (is (= :a (leftmost [[] [] [[:a]] :b [:c :d]])))
  (is (= [] (leftmost-recur [[] [['()]]])))
  (is (= [] (leftmost-recur []))))

(deftest test-leftmost-recur
  (is (= :a (leftmost-recur [:a :b [:c :d]])))
  (is (= :a (leftmost-recur [[:a :b] [:c :d]])))
  (is (= :a (leftmost-recur [[] [] [[:a]] :b [:c :d]])))
  (is (= [] (leftmost-recur [[] [['()]]])))
  (is (= [] (leftmost-recur [])))
  )

(deftest test-leftmost-recur-memoize
  (is (= :a (leftmost-recur-memoize [:a :b [:c :d]])))
  (is (= :a (leftmost-recur-memoize [[:a :b] [:c :d]])))
  (is (= :a (leftmost-recur-memoize [[] [] [[:a]] :b [:c :d]])))
  (is (= :a (leftmost-recur-memoize [[] [] [[[:a :55]]] :b [:c :d]])))
  (is (= [] (leftmost-recur-memoize [[] [['()]]])))
  (is (= [] (leftmost-recur-memoize [])))
  )

(deftest test-leftmost-with-flaten
  (is (= :a (leftmost-with-flatten [:a :b [:c :d]])))
  (is (= :a (leftmost-with-flatten [[:a :b] [:c :d]])))
  (is (= :a (leftmost-with-flatten [[] [] [[:a]] :b [:c :d]])))
  (is (= :a (leftmost-with-flatten [[] [] [[:a]] :b [:c :d]])))
  (is (= [] (leftmost-with-flatten [[] [['()]]])))
  (is (= [] (leftmost-with-flatten [])))
  )

(deftest test-rember1*
  (is (= [1 2 3 4]
           (rember1* :na [1 2 3 4])))
  (is (= [1 3 4]
           (rember1* 2 [1 2 3 4])))
  (is (= [1 [3] 4]
           (rember1* 2 [1 [2 3] 4])))
  (is (= [1 [:a :b] [3] 4]
           (rember1* 2 [1 [:a :b] [2 3] 4])))
  (is (= [1 [:a :b] [2 3] 4]
           (rember1* 2 [1 2 [:a :b] [2 3] 4])))
  (is (= [1 [[:a :b]] [[3]] 4]
           (rember1* 2 [1 [[:a :b]] [[2 3]] 4])))
  (is (= [[:pasta] :pasta [:noodles :meat :sauce] :meat :tomatoes]
           (rember1* :meat [[:pasta :meat] :pasta [:noodles :meat :sauce] :meat :tomatoes])))
  (is (= [[:S :r] :F [:m :t] :s]
           (rember1* :s [[:S :r] :F [:m :s :t] :s])))
  )

(deftest test-rember1*-alt
  (is (= [1 2 3 4]
           (rember1*-alt :na [1 2 3 4])))
  (is (= [1 3 4]
           (rember1*-alt 2 [1 2 3 4])))
  (is (= [1 [3] 4]
           (rember1*-alt 2 [1 [2 3] 4])))
  (is (= [1 [:a :b] [3] 4]
           (rember1*-alt 2 [1 [:a :b] [2 3] 4])))
  (is (= [1 [:a :b] [2 3] 4]
           (rember1*-alt 2 [1 2 [:a :b] [2 3] 4])))
  (is (= [1 [[:a :b]] [[3]] 4]
           (rember1*-alt 2 [1 [[:a :b]] [[2 3]] 4])))
  (is (= [[:pasta] :pasta [:noodles :meat :sauce] :meat :tomatoes]
           (rember1*-alt :meat [[:pasta :meat] :pasta [:noodles :meat :sauce] :meat :tomatoes])))
  (is (= [[:S :r] :F [:m :t] :s]
           (rember1*-alt :s [[:S :r] :F [:m :s :t] :s])))
  )

(deftest test-rember1*-recur
  (is (= [1 2 3 4]
           (rember1*-recur :na [1 2 3 4])))
  (is (= [1 3 4]
           (rember1*-recur 2 [1 2 3 4])))
  (is (= [1 [3] 4]
           (rember1*-recur 2 [1 [2 3] 4])))

  (is (= [1 [:a :b] [3] 4]
           (rember1*-recur 2 [1 [:a :b] [2 3] 4])))
  (is (= [1 [:a :b] [2 3] 4]
           (rember1*-recur 2 [1 2 [:a :b] [2 3] 4])))
  (is (= [1 [[:a :b]] [[3]] 4]
           (rember1*-recur 2 [1 [[:a :b]] [[2 3]] 4])))

  (is (= [[:pasta] :pasta [:noodles :meat :sauce] :meat :tomatoes]
           (rember1*-recur :meat [[:pasta :meat] :pasta [:noodles :meat :sauce] :meat :tomatoes])))
  (is (= [[:S :r] :F [:m :t] :s]
           (rember1*-recur :s [[:S :r] :F [:m :s :t] :s])))
  )

(deftest test-depth*
  (is (= 1 (depth* [])))
  (is (= 1 (depth* [1 2 3 4])))
  (is (= 2 (depth* [1 [2 3] 4])))
  (is (= 2 (depth* [1 [2 3] 4 [:a :b] 5])))
  (is (= 3 (depth* [1 [2 3] 4 [:a [:b :c]]])))
  (is (= 3 (depth* [1 [2 3 [:x]] 4 [:a :b] 5])))
  (is (= 4 (depth* [[ [:bitter :butter]
                      [:makes]
                      [:batter [:bitter]]]
                    :butter]
                   )))  
  (is (= 4 (depth* '(() ((:bitter :butter)
                         (:makes)
                         (:batter (:bitter)))
                     :butter))))
  )

(deftest test-depth*-recur
  (is (= 1 (depth*-recur [])))
  (is (= 1 (depth*-recur [1 2 3 4])))
  (is (= 2 (depth*-recur [1 [2 3] 4])))
  (is (= 2 (depth*-recur [1 [2 3] 4 [:a :b] 5])))
  (is (= 3 (depth*-recur [1 [2 3] 4 [:a [:b :c]]])))
  (is (= 3 (depth*-recur [1 [2 3 [:x]] 4 [:a :b] 5])))
  (is (= 4 (depth*-recur [[ [:bitter :butter]
                      [:makes]
                      [:batter [:bitter]]]
                    :butter]
                   )))  
  (is (= 4 (depth*-recur '(() ((:bitter :butter)
                         (:makes)
                         (:batter (:bitter)))
                     :butter))))
  )


;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 15 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

(deftest test-gourmet
  (is (= [:onion :skins] (gourmet :onion)))
  )

(deftest test-gourmand
  (is (= [:potato :potato] (gourmand :potato)))
  (is (= [:rice :rice] (gourmand :rice)))
  )

(deftest test-omnivore
  (is (= [:rice :rice] (omnivore :rice))))

(deftest test-omnivore-1
  (is (= [:rice :rice] (omnivore-1 :rice))))



;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 16 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

(deftest test-deepM-1
  (is (= '("pizza") (deepM-1 1)))
  (is (= "pizza" (get (getN2R) 0)))
  (is (= '("pizza") (get (getN2R) 1)))
  (is (= '([[[[[["pizza"]]]]]]) (deepM-1 7)))
  (is (= "pizza" (get (getN2R) 0)))
  (is (= '("pizza") (get (getN2R) 1)))
  (is (= '(["pizza"]) (get (getN2R) 2)))
  (is (= '([[[[[["pizza"]]]]]]) (get (getN2R) 7)))
  )


(deftest test-deepM
  (is (= '("pizza") (deepM 1)))
  (is (= '([[[[[["pizza"]]]]]]) (deepM 7)))
  )


(deftest test-deepM-2
  (is (= '("pizza") (deepM-2 1)))
  (is (= '([[[[[["pizza"]]]]]]) (deepM-2 7)))
  )


(deftest test-deepM-3
  (is (= '(["pizza"]) (deepM-3 2)))
  (is (= '([[[[[[["pizza"]]]]]]]) (deepM-3 8)))
  )

(deftest test-deepM-4
  (is (= '(["pizza"]) (deepM-4 2)))
  (is (= '([[[[[[[["pizza"]]]]]]]]) (deepM-4 9)))
  )


(deftest test-deep-with-external-memoize
  (is (= '("pizza") (deep-with-external-memoize 1)))
  (is (= "pizza" (get (getN2R) 0)))
  (is (= '("pizza") (get (getN2R) 1)))
  
  (is (= '([[[[[["pizza"]]]]]]) (deep-with-external-memoize 7)))
  (is (= "pizza" (get (getN2R) 0)))
  (is (= '("pizza") (get (getN2R) 1)))
  (is (= '(("pizza")) (get (getN2R) 2)))
  (is (= '([[[[["pizza"]]]]]) (get (getN2R) 6)))
  (is (= '([[[[[["pizza"]]]]]]) (get (getN2R) 7)))
  )


(deftest test-deep-idiomatic-clj
  (is (= '("pizza") (deep-with-external-memoize 1)))
  (is (= '([[[[[["pizza"]]]]]]) (deep-with-external-memoize 7)))
  (is (= [["pizza"]] (deep-with-external-memoize 2)))
  )


(deftest test-sslength-orig
  (is (= 0 (sslength-orig [])))
  (is (= 3 (sslength-orig [:a :b :c])))
  (is (= 4 (sslength-orig [:a [:b :c [:d] :e [:f]] :g :h])))
  )

(deftest test-sslength
  (is (= 0 (sslength [])))
  (is (= 1 (sslength [:a])))
  (is (= 3 (sslength [:a :b :c])))
  (is (= 4 (sslength [:a [:b :c [:d] :e [:f]] :g :h])))
  )

(deftest test-sslength2
  (is (= 0 (sslength2 [])))
  (is (= 1 (sslength2 [:a])))
  (is (= 3 (sslength2 [:a :b :c])))
  (is (= 4 (sslength2 [:a [:b :c [:d] :e [:f]] :g :h])))
  )

(deftest test-sslength3
  (is (= 0 (sslength3 [])))
  (is (= 1 (sslength3 [:a])))
  (is (= 3 (sslength3 [:a :b :c])))
  (is (= 4 (sslength3 [:a [:b :c [:d] :e [:f]] :g :h])))
  )

(deftest test-Ydepth*
  (is (= 1 (Ydepth* [])))
  (is (= 1 (Ydepth* [1 2 3 4])))
  (is (= 2 (Ydepth* [1 [2 3] 4])))
  (is (= 2 (Ydepth* [1 [2 3] 4 [:a :b] 5])))
  (is (= 3 (Ydepth* [1 [2 3] 4 [:a [:b :c]]])))
  (is (= 3 (Ydepth* [1 [2 3 [:x]] 4 [:a :b] 5])))
  (is (= 4 (Ydepth* [[ [:bitter :butter]
                       [:makes]
                       [:batter [:bitter]]]
                     :butter]
                    )))  
  (is (= 4 (Ydepth* '(() ((:bitter :butter)
                          (:makes)
                          (:batter (:bitter)))
                      :butter))))
  )

;; if you do ((Y! biz) 5) you will get an infinite loop
;; that contintually returns a function
(deftest Ybiz
  (is (= 0 ((Y biz) 5)))
  )


;; helper func to quickly run all tests from the REPL
(defn rt []
  (use 'seasoned-schemer.test.core :reload-all)
  (use 'seasoned-schemer.core :reload-all)
  (load-file "src/seasoned_schemer/core.clj")
  (load-file "test/seasoned_schemer/test/core.clj")
  (run-tests 'seasoned-schemer.test.core))


;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 17 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

(deftest test-deepM-128
  (is (= '(["pizza"]) (deepM-128 2)))
  (is (= '([[[[[[["pizza"]]]]]]]) (deepM-128 8)))
  )


(deftest test-deepM-129
  (is (= '(["pizza"]) (deepM-129 2)))
  (is (= '([[[[[[["pizza"]]]]]]]) (deepM-129 8)))
  )

(deftest test-deepM-129-2
  (is (= '([["pizza"]]) (deepM-129-2 3)))
  (is (= '([[[[[[["pizza"]]]]]]]) (deepM-129-2 8)))
  )

(deftest test-deep-with-consC
  (is (= '([["pizza"]]) (deep-with-consC 3)))
  (is (= '([[[[[[["pizza"]]]]]]]) (deep-with-consC 8)))
  )

(deftest test-deepM-129-3
  (is (= "pizza" (deepM-129-3 0)))
  (is (= '([[["pizza"]]]) (deepM-129-3 4)))
  (is (= '([[[[[[["pizza"]]]]]]]) (deepM-129-3 8)))
  )


;;; --------------------------------------------------- ;;;
;;; -----------------[ Chapter 18 ] ------------------- ;;;
;;; --------------------------------------------------- ;;;

(deftest test-lots
  (is (= (lots 3) '(:egg :egg :egg)))
  (is (= (lots 2) '(:egg :egg)))
  
  )

(deftest test-lenkth
  (is (= 3 (lenkth '(:a :b :c))))
  (is (= 1 (lenkth '(:a))))
  )

(deftest test-add-at-end
  (is (= '(:egg :egg :egg) (add-at-end '(:egg :egg))))
  )


