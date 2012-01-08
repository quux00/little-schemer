(def else true)

( ((fn [f] (f f)) 
   (fn [fact] 
     (fn [n] 
       (cond 
        (= 0 n) 1 
        else (* n (fact (- n 1))))))) 0) 

( ((fn [f] (f f)) 
   (fn [fact] 
     (fn [n] 
       (cond 
        (= 0 n) 1 
        else (* n ((fact fact) (- n 1))))))) 5)

(((fn [f] (f f)) 
   (fn [fact]
     ((fn [fact2]
        (fn [n] 
          (cond 
           (= 0 n) 1 
           else (* n (fact2 (- n 1)))))) (fn [x] ((fact fact) x))))   ) 6 )

(((fn [f] (f f)) 
   (fn [Y]
     ((fn [fact]
        (fn [n] 
          (cond 
           (= 0 n) 1 
           else (* n (fact (- n 1)))))) (fn [x] ((Y Y) x))))   ) 7 )


(((fn [r]
   ((fn [f] (f f)) 
    (fn [Y]
      (r (fn [x] ((Y Y) x))))))
  (fn [fact]
   (fn [n] 
     (cond 
      (= 0 n) 1 
      else (* n (fact (- n 1)))))) ) 5)

(def Y
  (fn [r]
    ((fn [f] (f f)) 
     (fn [Y]
       (r (fn [x] ((Y Y) x)))))))

((Y
  (fn [fact]
    (fn [n] 
      (cond 
       (= 0 n) 1 
       else (* n (fact (- n 1)))))) 
  ) 5)

((Y
  (fn [fib]
    (fn [n]
      (if (or (= n 1) (= n 0))
        n
        (+ (fib (- n 1)) (fib (- n 2)) )
        )))) 8)

;; 8 7 6 5 4 3 2  1  0
;; 0 1 1 2 3 5 8 13 21

;;(7 )

;; ( 5)

;; ( ((fn [f] (f f)) 
;;    (fn [fact] 
;;      ((fn [fact2]
;;         (fn [n] 
;;           (cond 
;;            (= 0 n) 1 
;;            else (* n (fact2 (- n 1)))))
;;         ) ; fact2
;;       )   ; invokes fact2 
;;      )    ; fact
;;    ) 5    ; invokes fact with 5
;;   )



