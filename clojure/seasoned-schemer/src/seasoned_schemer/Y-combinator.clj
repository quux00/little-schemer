;; From http://bolusmjak.posterous.com/applicative-order-y-combinator-clojure-versio

;;                    Stumbling towards Y (Clojure Version) 
;; 
;; (this tutorial can be cut & pasted into your IDE / editor)  
;; 
;; The applicative-order Y combinator is a function that allows one to create a 
;; recursive function without using define. 
;; This may seem strange, because usually a recursive function has to call 
;; itself, and thus relies on itself having been defined. 
;; 
;; Regardless, here we will stumble towards the implementation of the Y combinator. 
 
;; This was largely influenced by material in "The Little Schemer". 
;; http://www.ccs.neu.edu/home/matthias/BTLS/ 
;; 
;; I wrote this because I needed the excersise of arriving at Y on my own terms. 
;; As such, it is likely not the best resource to learn this. 
;; 
;; Mark Bolusmjak 2009 
 
(def else true) 
 
;; Let's start with the Factorial function. 
(def fact 
  (fn [n] 
    (cond 
      (= 0 n) 1 
      else (* n (fact (- n 1)))))) 

;; Since we can't use define, let's strip that off. 
;; For the recursive call, let's just put in a placeholder 
;; to see how things look. (commented out because it won't compile) 
(comment 
  (fn [n] 
    (cond 
      (= 0 n) 1 
      else (* n (myself (- n 1)))))) 
   
;; Ok, we need a way for the function to call itself via "myself". 
;; How will we get a handle on that if we can't use define? 
;; Maybe something can pass it in for us. Let's hope so and 
;; keep going. 
(fn [myself] 
  (fn [n] 
    (cond 
      (= 0 n) 1 
      else (* n (myself (- n 1)))))) 
 
;; That looks a lot like our original factorial function. 
;; Let's replace "myself" with fact and take a look (below). 
;; 
;; From the outside, it looks like something that makes the factorial function. 
;; Strangely, this function that creates the factorial function, relies on the 
;; function "fact" to be supplied to itself. 
;; Where will that come from? Does't that bring us back to square one? 
(fn [fact] 
  (fn [n] 
    (cond 
      (= 0 n) 1 
      else (* n (fact (- n 1)))))) 
 
;; Well, we have a function that builds the factorial function. That seems 
;; nearly as useful as a factorial function. Maybe we could pass that to itself 
;; and try to work with that. 
;; It's pretty easy to pass a function to itself if we have it defined. 
;; e.g. (f f) 
;; What about an anonymous function? 
;; No problem, just write a helper function that takes any function and applies 
;; it to itself. 
(fn [f] (f f)) 
 
;; Ok, let's throw that in and take a look. 
;; We'll pass fact to itself 
((fn [f] (f f)) 
  (fn [fact] 
    (fn [n] 
      (cond 
        (= 0 n) 1 
        else (* n (fact (- n 1))))))) 
 
;; What happens if we try this out? 
( ((fn [f] (f f)) 
   (fn [fact] 
     (fn [n] 
       (cond 
        (= 0 n) 1 
        else (* n (fact (- n 1))))))) 0) 
 
;; Hey it works for 0, but not for anything else. 
;; What's going on? 
;; 
;; With 0, (= 0 n) is true, and we get 1 back. 
;; 
;; If we try any other number we get a ClassCastException. Huh? 
;; We got a bit ahead of ourselves. 
;; Recall, fact is not the factorial funtion anymore. It now *builds* the 
;; factorial function. 
;; Also, we set it up to expecting a function as an argument before it returns 
;; the actual function we want. 
;; 
;; This may be getting strange, but let's try to make a useful function out of 
;; fact by calling (fact fact). This is good for one more step into our 
;; function, at which point (fact fact) can be used again to get the next step. 
(((fn [f] (f f)) 
  (fn [fact] 
    (fn [n] 
      (cond 
        (= 0 n) 1 
        else (* n ((fact fact) (- n 1))))))) 5) 
 
;; Wow! That actually worked. 
;; Take a breather and think about this for a second. We took a few shots in the 
;; dark and just implemented a recursive function without using define. 
;; Pretty cool, but it's kind of messy. 
;; 
;; We have something that kind of looks like our original factorial definition 
;; in there. Let's try to refactor that out and see what we're left with. 
;; 
;; Our original fact function didn't have anything looking like a (fact fact) in 
;; it, so let's try to fix that first. 
;; 
;; As the next step, we'll pull out the (fact fact) and give it a name. 
;; What should we call it? 
;; fact is kind of like factorial builder at this point, so (fact fact) is more 
;; like the factorial function. 
;; Hmm ... let's just call (fact fact) fact2, see how things look and go from 
;; there. 
;; 
;; We're going to be clever and instead of simply passing (fact fact) in as fact2, 
;; were going to wrap it up as a closure to prevent (fact fact) from getting 
;; evaluated before it gets passed in. 
;; 
;; So we'll pass in (fn [x] ((fact fact) x)) instead. 
((fn [f] (f f)) 
  (fn [fact] 
    ((fn [fact2] 
      (fn [n] 
        (cond 
          (= 0 n) 1 
          else (* n (fact2 (- n 1)))))) (fn [x] ((fact fact) x))))) 
 
;; If we try this out, we see it still works. 
(((fn [f] (f f)) 
  (fn [fact] 
    ((fn [fact2] 
      (fn [n] 
        (cond 
          (= 0 n) 1 
          else (* n (fact2 (- n 1)))))) (fn [x] ((fact fact) x))))) 12) 
 
;; Taking a closer look, we see that (fn [fact2] ... ) looks exactly like 
;; our original (fn (fact) ... ). 
;; Let's simply rename fact to y, and fact2 to fact to make this more clear. 
((fn [f] (f f)) 
  (fn [y] 
    ((fn [fact] 
      (fn [n] 
        (cond 
          (= 0 n) 1 
          else (* n (fact (- n 1)))))) (fn [x] ((y y) x))))) 
 
;; Let's pull out (fn [fact] ... ) and pass it in as a parameter r (for 
;; recursive function). 
((fn [r] 
   ((fn [f] (f f)) 
    (fn [y] 
      (r (fn [x] ((y y) x)))))) 
 (fn [fact] 
   (fn [n] 
     (cond 
      (= 0 n) 1 
      else (* n (fact (- n 1))))))) 
 
;; Still works! Try it. 
(((fn [r] 
  ((fn [f] (f f)) 
    (fn [y] 
      (r (fn [x] ((y y) x)))))) 
  (fn [fact] 
    (fn [n] 
      (cond 
        (= 0 n) 1 
        else (* n (fact (- n 1))))))) 4) 
 
;; Now we've isolated the scaffolding we've built to allow the creation of a 
;; recursive function without define. 
;; 
;; Finally, we've lived long enough without define, and that thing we built 
;; looks pretty useful. 
;; Let's use define and call it Y. 
(def Y 
  (fn [r] 
    ((fn [f] (f f)) 
      (fn [y] 
        (r (fn [x] ((y y) x))))))) 
 
;; That's it. We've built the applicative-order Y combinator in Clojure. 
 
;; Try it with another recursive function. Here it is with the Fibonacci 
;; number generator. 
;; (which, incidentally, makes 2 recursive calls to itself). 
((Y 
  (fn [fib] 
    (fn [n] 
      (cond 
        (< n 2) n 
        else (+ (fib (- n 1)) (fib (- n 2))))))) 8)
