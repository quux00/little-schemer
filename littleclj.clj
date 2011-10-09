(ns little.cljr
  (:use clojure.core))

;; ------------------- ;;
;; ---[ Chapter 1 ]--- ;;
;; ------------------- ;;
(defn atom?                       ; Ch.1, p.10
  "Predicate test for whether an entity is atom
   where atom is defined as not a list (not (list? x))
   using the Clojure built-in list? predicate"
  [x] (not (list? x)))

;; ------------------- ;;
;; ---[ Chapter 2 ]--- ;;
;; ------------------- ;;
(defn lat?                        ; Ch.2, p.16
  "Predicate test for whether an entity is a \"list-of-atoms\"
   where atom is defined as not a list (not (list? x))
   using the Clojure built-in list? predicate."
  [l]
  (if (empty? l)
    true
    (if (not (atom? (first l)))
      false
      (lat? (rest l)))))

(defn member?                     ; Ch.2, p.22
  "Predicate test for whether an entity is a member of a \"lat\"
   where lat = \"list-of-atoms\".  See atom? and lat? doc."
  [a lat]
  (if (empty? lat)
    false
    (or (= a (first lat)) (member? a (rest lat)))))

;; ------------------- ;;
;; ---[ Chapter 3 ]--- ;;
;; ------------------- ;;

;; this version returns lat when lat is empty, so
;; better matches the book version
(defn rember1 [a lat]             ; Ch.3, p.37
  (if (empty? lat)
    lat
    (if (= a (first lat))
      (rest lat)
      (cons (first lat) (rember1 a (rest lat))))))

;; this version returns nil when lat is empty
(defn rember1-alt [a lat]         ; Ch.3, p.37
  (when (not (empty? lat))
    (if (= a (first lat))
      (rest lat)
      (cons (first lat) (rember1-alt a (rest lat))))))

;; this is the improved if/elsif/else version from the book
(defn rember                    ; Ch.3, p.41
  "Remove a member of a lat, where lat = \"list-of-atoms\".
   See atom? and lat? doc.  It removes the first occurrence
   of +a+ from lat. Use multirember to remove all occurrences."
  [a lat]
  (cond
   (empty? lat) lat
   (= a (first lat)) (rest lat)
   :else (cons (first lat) (rember a (rest lat)))))

;; firsts is an "extract-col" method
(defn firsts
  "firsts is an \"extract-col\" method. It will extract the
   first \"column\" s-expression (element) from each list in l
   [l] = list of lists"
  [l]
  (if (empty? l)
    l
    (cons (first (first l)) (firsts (rest l)))))


(defn insertR                    ; Ch.3, p.50
  "insertR searches for a match to +old+ and if found inserts
   +new+ value to the right of in the list (producing and
   returning a new list of course)
   [new] = new element to insert to the right of old
   [old] = element to find in list
   [lat] = list of atoms to search
   returns +lat+ if +old+ cannot be found in the list"
  [new old lat]
  (cond
   (not (lat? lat)) lat  ; short circuit passing in non-lat
   (empty? lat) lat
   (= old (first lat)) (cons old (cons new (rest lat)))
   :else (cons (first lat) (insertR new old (rest lat)))))

(defn insertL                    ; Ch.3, p.51
  "inserts +new+ to the left of the first occurance of +old+ in the
  list +lat+. Very similar to insertR - see its doc for more details"
  [new old lat]
  (cond
   (not (lat? lat)) lat
   (empty? lat) lat
   (= old (first lat)) (cons new lat)
   :else (cons (first lat) (insertL new old (rest lat)))))

(defn subst                      ; Ch.3, p.51-52
  "replaces the first occurence +old+ with +new+ in list +lat+
   very similar to insertR so see its doc for more details"
  [new old lat]
  (cond
   (not (lat? lat)) lat
   (empty? lat) lat
   (= old (first lat)) (cons new (rest lat))
   :else (cons (first lat) (subst new old (rest lat)))))

(defn subst2                     ; Ch.3, p.52
  "Returns a new list with the first occurence of either +o1+
  or +o2+, whichever occurs first, replaced with +new+ in the list
  +lat+.  Returns +lat+ if it is empty or neither +o1+ nor +o2+
  can be found."
  [new o1 o2 lat]
  (cond
   (not (lat? lat)) lat
   (empty? lat) lat
   (or (= o1 (first lat)) (= o2 (first lat))) (cons new (rest lat))
   :else (cons (first lat) (subst2 new o1 o2 (rest lat)))))


(defn multirember                ; Ch.3, p.53
  "version of rember (see its doc) that removes all elements
   in a list that match +a+, rather than just the first one"
  [a lat]
  (cond
   (empty? lat) lat
   (= a (first lat)) (multirember a (rest lat))
   :else (cons (first lat) (multirember a (rest lat)))))

(defn multiinsertR              ; Ch.3, p.56
  "version of insertR (see its doc) that inserts +new+ after all
   occurrences of +old+ , rather than just the first one"
  [new old lat]
  (cond
   (not (lat? lat)) lat
   (empty? lat) lat
   (= old (first lat)) (cons old (cons new (multiinsertR new old (rest lat))))
   :else (cons (first lat) (multiinsertR new old (rest lat)))))

(defn multiinsertL              ; Ch.3, p.56
  "version of insertL (see its doc) that inserts +new+ before all
   occurrences of +old+ , rather than just the first one"
  [new old lat]
  (cond
   (not (lat? lat)) lat
   (empty? lat) lat
   (= old (first lat)) (cons new (cons old (multiinsertL new old (rest lat))))
   :else (cons (first lat) (multiinsertL new old (rest lat)))))

(defn multisubst                ; Ch.3, p.57
  "version of subst (see its doc) that substibutes +new+ for
   all occurrences of +old+, rather than just the first one"
  [new old lat]
  (cond
   (not (lat? lat)) lat
   (empty? lat) lat
   (= old (first lat)) (cons new (multisubst new old (rest lat)))
   :else (cons (first lat) (multisubst new old (rest lat)))))


;; ------------------- ;;
;; ---[ Chapter 4 ]--- ;;
;; ------------------- ;;

(defn o+                        ; Ch.4, p.60
  "Arithmetic plus operator. Requires two and only two args"
  [n m]
  (if (zero? m)
    n
    (inc (o+ n (dec m)))))

;; alternate version that is more intuitive to me
(defn o2+                       ; Ch.4, p.60
  "Arithmetic plus operator. Requires two and only two args"
  [n m]
  (if (zero? m)
    n
    (o+ (inc n) (dec m))))

(defn o-                        ; Ch.4, p.61
  "Arithmetic minus operator. Requires two and only two args"
  [n m]
  (if (zero? m)
    n
    (dec (o- n (dec m)))))

(defn addtup                    ; Ch.4, p.64
  "Add all numbers in a tuple (defined to be a list of numbers)
   and return the result"
  [tup]
  (if (empty? tup)
    0
    (o+ (first tup) (addtup (rest tup)))))

(defn o*                        ; Ch.4, p.65
  "Arithmetic multiplication operator. Requires two and only two args.
   Note this will get a StackOverflow error for larger values of m."
  [n m]
  (if (zero? m)
    0
    (o+ n (o* n (dec m)))))

(defn tup+-orig                 ; Ch.4, p.69
  "Adds each \"column\" of two tuples (list of numbers) together
   returning a new list with the sum of each column of the original
   tuples.  This function requires that +tup1+ and +tup2+ be of the
   same length (you will get a NullPointException if they are not)."
  [tup1 tup2]
  (if (and (empty? tup1) (empty? tup2))
    '()
    (cons (o+ (first tup1) (first tup2)) (tup+-orig (rest tup1) (rest tup2)))))

(defn tup+                      ; Ch.4, p.71
  "Adds each \"column\" of two tuples (list of numbers) together
   returning a new list with the sum of each column of the original
   tuples.  +tup1+ and +tup2+ may be of different lengths."
  [tup1 tup2]
  (cond
   (empty? tup1) tup2
   (empty? tup2) tup1
   :else (cons (o+ (first tup1) (first tup2)) (tup+ (rest tup1) (rest tup2)))))


(defn o>
  "Greater than comparison for two non-negative numbers.
   Returns true if n > m, false otherwise."
  [n m]
  (cond
   (zero? n) false                  ; n <= m
   (zero? m) true                   ; n > m
   :else (o> (dec n) (dec m))))


(defn o<
  "Less than comparison for two non-negative numbers.
   Returns true if n < m, false otherwise."
  [n m]
  (cond
   (zero? m) false                  ; n >= m
   (zero? n) true                   ; n < m
   :else (o< (dec n) (dec m))))


(defn o=
  "Returns true if two non-negative numbers passed in are equal
   false otherwise"
  [n m]
  (cond
   (o> n m) false
   (o< n m) false
   :else true))

(defn exp
  "Return n raised to the exponent of m. n and m must be >= 0."
  [n m]
  (if (zero? m)
    1
    (o* n (exp n (dec m)))))

(defn exp2     ; more verbose version added by me
  "Return n raised to the exponent of m. n and m must be >= 0."
  [n m]
  (cond
   (zero? m) 1
   (= m 1) n   ; stops the recursion stack from going to zero unecessarily
   :else (o* n (exp2 n (dec m)))))

(defn quotient
  "Integer division on non-negative numbers. Divides m into n, ignoring
   any remainder."
  [n m]
  (if (o< n m)
    0
    (inc (quotient (o- n m) m))))

(defn length
  "Returns the length of a list."
  [lat]
  (if (empty? lat)
    0
    (inc (length (rest lat)))))

(defn pick
  "Using 1-based indexing of lists, return the nth element of list +lat+
   Returns nil if +n+ is larger than the size of +lat+ or +lat+ is empty.
   +n+ must be greater than 0"
  [n lat]
  (if (= n 1)
    (first lat)
    (pick (dec n) (rest lat))))

(defn rempick
  "Using 1-based indexing of lists, remove the nth element of list
   +lat+ returning a new list with that element removed. See also
   pick func notes."
  [n lat]
  (cond
   (empty? lat) lat
   (= n 1) (rest lat)
   :else (cons (first lat) (rempick (dec n) (rest lat)))))

(defn no-nums
  "Removes all numbers from a lat (list of atoms), returning that
   new list without numbers"
  [lat]
  (cond
   (empty? lat) lat
   (number? (first lat)) (no-nums (rest lat))
   :else (cons (first lat) (no-nums (rest lat)))))

(defn all-nums
  "Selects out all numbers from a lat (list of atoms), returning
   that new list-of-numbers (tuple, in Little Schemer lingo)"
  [lat]
  (cond
   (empty? lat) lat
   (number? (first lat)) (cons (first lat) (all-nums (rest lat)))
   :else (all-nums (rest lat))))

;; Note: I did not implement eqan? since Clojure's = function already
;; works to compare numbers and non-numbers and I intend to use Clojure's
;; = rather than my o= function


(defn occur
  "Counts the number of times the atom +a+ occurs in +lat+"
  [a lat]
  (cond
   (empty? lat) 0
   (= a (first lat)) (inc (occur a (rest lat)))
   :else (occur a (rest lat))))

(defn one?
  "Predicate that evaluates if the atom passed is a number equal to 1"
  [n]
  ;; technically number? is not required here in Clojure, but it would
  ;; be in Scheme (and they left it out in the book, tsk tsk)
  (and (number? n) (= n 1)))


;; ------------------- ;;
;; ---[ Chapter 5 ]--- ;;
;; ------------------- ;;

(defn rember*
  "A version of rember (remove member) that will remove all occurences
   of +a+ in +l+, no matter how deeply nested in inner lists it is.
   Returns the new list."
  [a l]
  (if (empty? l)
    l
    (if (atom? (first l))
      (if (= a (first l))
        (rember* a (rest l))
        (cons (first l) (rember* a (rest l))))
      (cons (rember* a (first l)) (rember* a (rest l))))))

(defn insertR*
  "A version of insertR (insert +new+ to the right of +old+) that
   that will insert after all occurences of +a+ in +l+, no matter
   how deeply nested in inner lists it is. Returns the new list."
  [new old l]
  (if (empty? l)
    l
    (if (atom? (first l))
      (if (= old (first l))
        (cons old (cons new (insertR* new old (rest l))))
        (cons (first l) (insertR* new old (rest l))))
      (cons (insertR* new old (first l)) (insertR* new old (rest l))))))

(defn insertL*
  "A version of insertL (insert +new+ to the left of +old+) that
   that will insert before all occurences of +a+ in +l+, no matter
   how deeply nested in inner lists it is. Returns the new list."
  [new old l]
  (if (empty? l)
    l
    (if (atom? (first l))
      (if (= old (first l))
        (cons new (cons old (insertL* new old (rest l))))
        (cons (first l) (insertL* new old (rest l))))
      (cons (insertL* new old (first l)) (insertL* new old (rest l))))))


(defn occur*
  "Counts and return the number of times the atom +a+ occurs in
   the list +l+, regardless of how deeply nested in sub-lists
   it is."
  [a l]
  (if (empty? l)
    0
    (if (atom? (first l))
      (if (= a (first l))
        (inc (occur* a (rest l)))
        (occur* a (rest l)))
      (+ (occur* a (first l)) (occur* a (rest l))))))


(defn subst*
  "Version of subst that substitutes +new+ for +old+ no matter
   how deeply nested +old+ is in lists and sublists of list +l+.
   Returns the new list."
  [new old l]
  (if (empty? l)
    l
    (if (atom? (first l))
      (if (= old (first l))
        (cons new (subst* new old (rest l)))
        (cons (first l) (subst* new old (rest l))))
      (cons (subst* new old (first l)) (subst* new old (rest l))))))


;; NOTE: I modified the name to have a '?' - not in the Little
;; Schemer version for some reason (maybe Scheme doesn't allow
;; two non-alpha chars in a function name
(defn member?*
  "Predicate test for whether an entity is a member of a list +l+.
   Looks for +a+ anywhere in the list including sublists"
  [a l]
  (if (empty? l)
    false
    (if (atom? (first l))
      (or (= a (first l)) (member?* a (rest l)))
      (or (member?* a (first l)) (member?* a (rest l))))))

(defn leftmost*
  "Returns the leftmost atom (element) of a list. It will recurse
   down into a sublist if that is the first S-expression in the list
   +l+.  If that initial sublist or +l+ is empty, it will return nil."
  [l]
  (if (empty? l)
    nil
    (if (atom? (first l))
      (first l)
      (leftmost* (first l)))))

;; this is my version, which differs (and I think is more
;; elegant) than the book's version on p. 92
;; I did not define a separate equal? function as they
;; document bcs both equal? and eqlist? are dependent on
;; the other, which seems bad circular design to me.
;; In additon, in Clojure we don't need to define a general
;; equal? method to handle any S-expression since Clojure's
;; built-in = function already handles that - see my rember
;; implementation to demonstrate that.  My Clojure version
;; rember exactly matches the version on p. 95 using Clojure's
;; = functional instead of a self-defined equal? function.
(defn eqlist?
  "Compares two lists. If the two lists have exact value equivalence
   it returns true, otherwise false."
  [l1 l2]
  (cond
   (and (empty? l1) (empty? l2)) true

   (and (atom? (first l1)) (atom? (first l2)))
   (if (not (= (first l1) (first l2)))
     false
     (eqlist? (rest l1) (rest l2)))

   (and (list? (first l1)) (list? (first l2)))
   (if (not (eqlist? (first l1) (first l2)))
     false
     (eqlist? (rest l1) (rest l2)))

   :else false))

;; ------------------- ;;
;; ---[ Chapter 6 ]--- ;;
;; ------------------- ;;

(defn numbered?
  "Predicate function that checks whether a single number (not in a
   list) is passed in or an arithmetic expression with in-fix
   notation in a list, such as (3 * (4 + 1)).  My version allows
   the four basic arithmetic operations but :exp for exponentiation.
   Note that this method is flawed in that it assumes you lists
   have an odd number of entries and in in-fix notation.
   For example: ((1 + 2) * 4 4) will return true, so this is NOT
   a general purpose in fix arithmetic AST parser."
  [aexp]
  (cond 
   (atom? aexp) (number? aexp)
   (and (numbered? (first aexp))
        (numbered? (first (rest (rest aexp))))
        (or (= (first (rest aexp)) '*)
            (= (first (rest aexp)) '+)
            (= (first (rest aexp)) '-)
            (= (first (rest aexp)) '/)
            (= (first (rest aexp)) 'exp))) true))

(defn bk-numbered?
  "book version of numbered?, which skips checking the
   second (middle) value as to whether it is a valid operator."
  [aexp]
  (if (atom? aexp)
    (number? aexp)
    (and (bk-numbered? (first aexp))
         (bk-numbered? (first (rest (rest aexp)))))))

(defn value
  "My version of the first value function. It uses number? instead
   of atom? as its primary check and then checks the whole expression
   is numbered? using that function.  Only then does it recurse into
   the subexpressions to calculate their value.  Returns nil if +nexp+
   is not a valid numbered arithmetic expression. It suffers the same
   robustness flaws that numbered? does (see its doc)."
  [nexp]
  (if (number? nexp) 
    nexp
    (if (numbered? nexp)
      (cond
       (= (first (rest nexp)) '*) 
       (* (value (first nexp)) (value (first (rest (rest nexp)))))
       
       (= (first (rest nexp)) '+) 
       (+ (value (first nexp)) (value (first (rest (rest nexp)))))
       
       (= (first (rest nexp)) 'exp) 
       (exp (value (first nexp)) (value (first (rest (rest nexp))))))
      nil)))

(defn bk-value
  "Book version of value function. This version is prone to null
   pointer exceptions if you throw mal-formed expressions such as:
   ((1 2) exp 3)."
  [nexp]
  (cond 
   (atom? nexp) nexp

   (= (first (rest nexp)) '+) 
   (+ (value (first nexp)) (value (first (rest (rest nexp)))))

   (= (first (rest nexp)) '*) 
   (* (value (first nexp)) (value (first (rest (rest nexp)))))
    
   (= (first (rest nexp)) 'exp) 
   (exp (value (first nexp)) (value (first (rest (rest nexp)))))))


(defn pf-1st-sub-exp
  "returns the first sub expression for prefix (pf) notation
   arithmetic expressions"
  [aexp]
  (first (rest aexp)))

(defn pf-2nd-sub-exp
  "returns the second sub expression for prefix (pf) notation
   arithmetic expressions"
  [aexp]
  (first (rest (rest aexp))))

(defn pf-operator
  "returns the operator expression for prefix (pf) notation
   arithmetic expressions"  
  [aexp]
  (first aexp))

(defn pf-value
  "pre-fix arithmetic expression evaluator:
   (+ (* 1 1) 1) results in 2"
  [nexp]
  (cond
   (atom? nexp) nexp
   
   (= (pf-operator nexp) '+)
   (+ (pf-value (pf-1st-sub-exp nexp))
      (pf-value (pf-2nd-sub-exp nexp)))

   (= (pf-operator nexp) '*)
   (* (pf-value (pf-1st-sub-exp nexp))
      (pf-value (pf-2nd-sub-exp nexp)))
   
   (= (pf-operator nexp) 'exp)
   (exp (pf-value (pf-1st-sub-exp nexp))
        (pf-value (pf-2nd-sub-exp nexp)))))

(defn sero?
  "Version of zero? to do math with empty lists"
  [nl]
  (empty? nl))

(defn edd1
  "Version of inc (add1) to do math with empty lists"
  [nl]
  (cons '() nl))

(defn zub1
  "Version of dec (sub1) to do math with empty lists"
  [nl]
  (rest nl))

(defn nl+
  "addition of two \"nl\" empty list expressions to do math
   with empty lists"
  [nl ml]
  (if (sero? ml)
    nl
    (edd1 (nl+ nl (zub1 ml)))))

(defn nl-lat?
  "A lat? method for lists-of-empty lists for doing the math
   on empty lists at the end of Ch. 6"
  [l]
  (cond
   (not (list? l)) false
   (empty? l) true
   :else (and (nl-lat? (first l)) (nl-lat? (rest l)))))

;; this is the book version of nl-lat and it is severely broken
;; unless you redefine an atom? (nl-atom?), which I did informally
;; in my version above
;; (defn nl-lat?
;;   ""
;;   [l]
;;   (cond
;;    (empty? l) true
;;    (atom? (first l)) (nl-lat? (rest l))
;;    :else false))

(defn strict-nl-lat?
  "My strict version of nl-lat? that doesn't allow empty lists
   within the empty lists - the 'primitive unit' is () and cannot
   have more empty lists in it.  The Book version of nl-lat? fails
   this test."
  [l]
  (cond
   (not (list? l)) false
   (empty? l) true
   (or (not (list? (first l))) (not (empty? (first l)))) false
   :else (strict-nl-lat? (rest l))))



;; ------------------- ;;
;; ---[ Chapter 7 ]--- ;;
;; ------------------- ;;

;; can't use set? since that is part of clojure.core
;; so I renamed it isset?
(defn isset?
  "Predicate determining whether the list passed in is a set, 
   where set is defined as a list that has no duplicate entries.
   Returns true for the empty list."
  [lat]
  (cond
   (empty? lat) true
   (member? (first lat) (rest lat)) false
   :else (isset? (rest lat))))


(defn makeset-1
  "makes a set from a list - it filters out any duplicates in the
   list +lat+ and returns a new list"
  [lat]
  (cond 
   (empty? lat) lat
   (member? (first lat) (rest lat)) (makeset-1 (rest lat))
   :else (cons (first lat) (makeset-1 (rest lat)))))

(defn makeset
  "makes a set from a list - it filters out any duplicates in the
   list +lat+ and returns a new list"
  [lat]
  (if (empty? lat) 
    lat
    (cons (first lat) (makeset (multirember (first lat) (rest lat))))))


(defn subset?
  "Predicate that determins whether all members +set1+ are also in
  +set2+. Returns true if set1 is the empty set."
  [set1 set2]
  (if (empty? set1) 
    true
    (and (member? (first set1) set2) (subset? (rest set1) set2))))

(defn eqset?
  ""
  [set1 set2]
  (and (subset? set1 set2) (subset? set2 set1)))

;; left off p. 115 => intersect is next
