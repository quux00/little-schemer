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
  "Remove a member of a lat, where lat = 'list-of-atoms'.
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

;; I have two versions of eqlist?, both of which differ (and
;; I think are more elegant) than the book's version on p. 92
;; I did not define a separate equal? function as they
;; document bcs both equal? and eqlist? are dependent on
;; the other, which seems bad circular design to me.
;; In additon, in Clojure we don't need to define a general
;; equal? method to handle any S-expression since Clojure's
;; built-in = function already handles that - see my rember
;; implementation to demonstrate that.  My Clojure version
;; rember exactly matches the version on p. 95 using Clojure's
;; = functional instead of a self-defined equal? function.
(defn eqlist2?
  "Compares two lists. If the two lists have exact value equivalence
   it returns true, otherwise false."
  [l1 l2]
  (cond
   (and (empty? l1) (empty? l2)) true

   (and (atom? (first l1)) (atom? (first l2)))
   (if (not (= (first l1) (first l2)))
     false
     (eqlist2? (rest l1) (rest l2)))

   (and (list? (first l1)) (list? (first l2)))
   (if (not (eqlist2? (first l1) (first l2)))
     false
     (eqlist2? (rest l1) (rest l2)))

   :else false))

(defn eqlist?
  "Compares two lists. If the two lists have exact value equivalence
   it returns true, otherwise false."
  [l1 l2]
  (cond
   (or (empty? l1) (empty? l2)) (and (empty? l1) (empty? l2))

   (or (atom? (first l1)) (atom? (first l2)))
   (if (and (atom? (first l1)) (atom? (first l2)) 
            (= (first l1) (first l2)))
     (eqlist? (rest l1) (rest l2))
     false)

   :else (and (eqlist? (first l1) (first l2))
              (eqlist? (rest l1) (rest l2)))))
     


;; ------------------- ;;
;; ---[ Chapter 6 ]--- ;;
;; ------------------- ;;

(defn numbered?
  "Predicate function that checks whether a single number (not in a
   list) is passed in or an arithmetic expression with in-fix
   notation in a list, such as (3 * (4 + 1)).  My version allows
   the four basic arithmetic operations but also :exp for exponentiation.
   Note that this method is flawed in that it assumes your lists
   have an odd number of entries and in in-fix notation.
   For example: ((1 + 2) * 4 4) will return true, so this is NOT
   a general purpose in-fix arithmetic AST parser."
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

;; changed to nvalue for "numeric value" since the name "value"
;; gets overloaded in Chapter 10 as a Scheme interpreter function
(defn nvalue
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
       (* (nvalue (first nexp)) (nvalue (first (rest (rest nexp)))))
       
       (= (first (rest nexp)) '+) 
       (+ (nvalue (first nexp)) (nvalue (first (rest (rest nexp)))))
       
       (= (first (rest nexp)) 'exp) 
       (exp (nvalue (first nexp)) (nvalue (first (rest (rest nexp))))))
      nil)))

(defn bk-nvalue
  "Book version of value function. This version is prone to null
   pointer exceptions if you throw mal-formed expressions such as:
   ((1 2) exp 3)."
  [nexp]
  (cond 
   (atom? nexp) nexp

   (= (first (rest nexp)) '+) 
   (+ (nvalue (first nexp)) (nvalue (first (rest (rest nexp)))))

   (= (first (rest nexp)) '*) 
   (* (nvalue (first nexp)) (nvalue (first (rest (rest nexp)))))
    
   (= (first (rest nexp)) 'exp) 
   (exp (nvalue (first nexp)) (nvalue (first (rest (rest nexp)))))))


(defn nf-1st-sub-exp
  "returns the first sub expression for infix (nf) notation
   arithmetic expressions"
  [aexp]
  (first aexp))

(defn nf-2nd-sub-exp
  "returns the second sub expression for infix (nf) notation
   arithmetic expressions"
  [aexp]
  (first (rest (rest aexp))))

(defn nf-operator
  "returns the operator expression for infix (nf) notation
   arithmetic expressions"  
  [aexp]
  (first (rest aexp)))

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
  "Predicate that determines whether all members +set1+ are also in
  +set2+. Returns true if set1 is the empty set."
  [set1 set2]
  (if (empty? set1) 
    true
    (and (member? (first set1) set2) (subset? (rest set1) set2))))

(defn eqset?
  "Predicate to determine whether the two sets contain the same entities
   (regardless of order, since sets do not define an order)"
  [set1 set2]
  (and (subset? set1 set2) (subset? set2 set1)))

(defn intersect?
  "Predicate to determine whether the two set intersect - have any
   one entry in common.  Returns false if they do not (including
   when one of the sets is empty)."
  [set1 set2]
  (if (empty? set1) 
    false
    (or (member? (first set1) set2) (intersect? (rest set1) set2))))
  
;; to make this an intersect that would work on lists (with possibly
;; redundant entries, try writing it with multirember ...
(defn intersect
  "Calculates and returns the insersection of elements between the
   two sets.  This assumes that the sets are sets (have unique values)
   otherwise it may return duplicate entries."
  [set1 set2]
  (cond
   (empty? set1) set1

   (member? (first set1) set2) 
   (cons (first set1) (intersect (rest set1) set2))

   :else (intersect (rest set1) set2)))
 

(defn union
  "Calculates and returns the union of two sets (assumed to have
   non-redundant entries)."
  [set1 set2]
  (cond
   (empty? set1) set2
   (member? (first set1) set2) (union (rest set1) set2)
   :else (cons (first set1) (union (rest set1) set2))))


(defn set-diff
  "Returns a set (list) of all elements in +set1+ that are not
   in +set2+"
  [set1 set2]
  (cond
   (empty? set1) '()
   (member? (first set1) set2) (set-diff (rest set1) set2)
   :else (cons (first set1) (set-diff (rest set1) set2))))

(defn intersect-all
  "Finds intersection between multiple sets.  Required input
   is a list of sets-of-atoms (or set of sets-of-atoms)"
  [l-set]
  (if (empty? (rest l-set))
    (first l-set)
    (intersect (first l-set) (intersect-all (rest l-set)))))

(defn pair?
  "Predicate evaluates whether the argument is
   a pair of s-expressions."
  [x]
  (cond
   (atom? x) false
   (empty? x) false
   (empty? (rest x)) false
   (empty? (rest (rest x))) true
   :else false))

(defn build
  "Builds a pair from two s-expressions."
  [s1 s2]
  (cons s1 (cons s2 '())))


(defn fun?
  "Predicate that evaluates whether the list of the first
   s-expression from a rel (a set of pairs) comprises a set.
   Example: '((a b) (c d) (e f)) is a fun, but
            '((a b) (c d) (a f)) is not a fun since firsts
            of it is not a set
   Note: no validity checking is done to ensure the argument
   is a rel, so if it is not, the answer is not trustworthy."
  [rel]
  (isset? (firsts rel)))

(defn revrel
  "Reverses the order of each pair in a rel, where
   rel is defined as a set of pairs.
   Note: no validity checking is done to ensure the argument
   is a rel, so if it is not, the answer is not trustworthy."
  [rel]
  (if (empty? rel)
    rel
    (cons (build (second (first rel)) (first (first rel))) (revrel (rest rel)))))

(defn revpair
  "Reverses the elements in a pair"
  [pair]
  (build (second pair) (first pair)))

(defn revrel2
  "Reverses the order of each pair in a rel, where
   rel is defined as a set of pairs, this time using
   the revpair helper function"
  [rel]
  (if (empty? rel)
    rel
    (cons (revpair (first rel)) (revrel2 (rest rel)))))


(defn seconds
  "An 'extract-col' method where it extracts the second element
   of each sublist.  Argument +l+ = list of lists."
  [l]
  (if (empty? l)
    l
    (cons (first (rest (first l))) (seconds (rest l)))))

(defn fullfun?
  ""
  [fun]
  (isset? (seconds fun)))

(defn one-to-one?
  "Predicate that evaluates whether the second of the first
   s-expression from a fun (a set of pairs where the first
   element of the list forms a set) comprises a set
   Example: '((a b) (c d) (e f)) is a fullfun, but
            '((a b) (c d) (e d)) is not a fullfun
   Note: no validity checking is done to ensure the argument
   is a fun, so if it is not, the answer is not trustworthy."
  [fun]
  (fun? (revrel2 fun)))


    

;; ------------------- ;;
;; ---[ Chapter 8 ]--- ;;
;; ------------------- ;;

(defn rember-f?
  "A rember function that takes a function +f+ to invoke
   to test whether the s-expr +s+ is in the list +l+.
   Note that while rember-f will take any s-expr, it will
   not recursively search down into the sub lists of l to
   find +s+, so it is similar to rember, not rember*."
  [f a l]
  (cond
   (empty? l) l
   (f a (first l)) (rest l)
   :else (cons (first l) (rember-f? f a (rest l)))))

(defn eq?-c
  "Functions that curries the '=' function by taking
   one element to compare to and returns a prediate func
   that will return true if the argument passed to it
   matches the argument originally passed to eq?-c."
  [a]
  (fn [x] (= a x)))

(defn rember-f2?
  "Partial application version of rember (or rember-f) that 
   takes a comparison/equality predicate operator and returns 
   an anonymous function/lambda that takes an atom and list
   to act like rember does (depending on how the predicate
   operator works"
  [f]
  (fn [a l]
    (cond
     (empty? l) l
     (f a (first l)) (rest l)
     :else (cons (first l) ((rember-f2? f) a (rest l))))))
      
(defn rember-f3?
  "My version of rember-f2 that I suspect is more efficient
   than recalling the outer method - instead we keep recalling
   the one inner closure we created, but I have to give it a
   name now"
  [f]
  (fn rem-closure [a l]
    (cond
     (empty? l) l
     (f a (first l)) (rest l)
     :else (cons (first l) (rem-closure a (rest l)))))) ;; efficient?
      

(defn insertL-f
  "Partial application version of insertL that takes a comparison
   predicate function first and returns a lambda that acts like
   insertL (the original version)"
  [f]
  (fn intern-closure [new old lat]
    (cond
     (empty? lat) lat
  
     (f old (first lat))
     (cons new (cons old (intern-closure new old (rest lat))))
     
     :else (cons (first lat) (intern-closure new old (rest lat))))))


(defn insertR-f
  "Partial application version of insertR that takes a comparison
   predicate function first and returns a lambda that acts like
   insertR (the original version)"
  [f]
  (fn intern-closure [new old lat]
    (cond
     (empty? lat) lat
  
     (f old (first lat))
     (cons old (cons new (intern-closure new old (rest lat))))
     
     :else (cons (first lat) (intern-closure new old (rest lat))))))

(defn seqL
  "Takes two elements to cons onto list +l+.
   Prepends in the order: +new+, +old+"
  [new old l]
  (cons new (cons old l)))

(defn seqR
  "Takes two elements to cons onto list +l+.
   Prepends in the order: +old+, +new+"
  [new old l]
  (cons old (cons new l)))

(defn insert-g
  ""
  [seq-f]
  (fn insg-closure [new old l]
    (cond
     (empty? l) l
     (= old (first l)) (seq-f new old (insg-closure new old (rest l)))
     :else (cons (first l) (insg-closure new old (rest l))))))

(defn atom-to-function
  "matches an atom that represents a mathematical function
   and returns the corresponding function"
  [x]
  (cond
   (= x '+) +
   (= x '-) -
   (= x '*) *
   (= x '/) /
   (= x 'exp) exp
   :else nil))

(defn value2
  "Rewrite of the value function as a higher order function
   to keep code base DRY. See doc of value function."
  [nexp]
  (if (number? nexp)
    nexp
    (if (numbered? nexp)
      ((atom-to-function (nf-operator nexp))
       (value2 (nf-1st-sub-exp nexp)) (value2 (nf-2nd-sub-exp nexp)))
      nil)))


(defn multirember-f
  "Partial application version of multirember that takes a test predicate
   function and returns a multirember function using that test predicate.
   The returned function takes atom +a+ and lat +lat+ as arguments."
  [f]
  (fn mrm-closure [a lat]
    (cond
     (empty? lat) lat
     (f a (first lat)) (mrm-closure a (rest lat))
     :else (cons (first lat) (mrm-closure a (rest lat))))))

(defn multirember&co
  "continuation-style passing function that collects all atoms
   that are not found in +lat+ in the first list and all atoms
   that are found in +lat+ in the second list and finally calls
   the function +col+ with those lists.  The return value of
   col is the return value of multirember&co. "
  [a lat col]
  (cond
   (empty? lat) (col '() '())

   (= a (first lat)) 
   (multirember&co a (rest lat) (fn [newlat seen]
                                  (col newlat (cons (first lat) seen))))

   :else (multirember&co a (rest lat) (fn [newlat seen]
                                        (col (cons (first lat) newlat) seen)))))

(defn multiinsertLR
  "Version of multiinsert that inserts +new+ to the left of
   oldL and to the right of oldR.  If oldL == oldR, then it
   will only insert to the left.  It returns the new lat."
  [new oldL oldR lat]
  (cond
   (empty? lat) lat
  
   (= oldL (first lat)) 
   (cons new (cons oldL (multiinsertLR new oldL oldR (rest lat))))
   
   (= oldR (first lat))
   (cons oldR (cons new (multiinsertLR new oldL oldR (rest lat))))
   
   :else (cons (first lat) (multiinsertLR new oldL oldR (rest lat)))))


(defn multiinsertLR&co
  "Version of multiinsertLR that takes a collector function
   that will be applied to the new lat, the number of left insertions
   and the number of right insertions"
  [new oldL oldR lat col]
  (cond
   (empty? lat) (col '() 0 0)

   (= oldL (first lat))
   (multiinsertLR&co new oldL oldR (rest lat) 
                     (fn [newlat nleft nright]
                       (col (cons new (cons oldL newlat)) (inc nleft) nright)))
                                                
   (= oldR (first lat))
   (multiinsertLR&co new oldL oldR (rest lat) 
                     (fn [newlat nleft nright]
                       (col (cons oldR (cons new newlat)) nleft (inc nright))))
          
   :else (multiinsertLR&co new oldL oldR (rest lat)
                           (fn [newlat nleft nright]
                             (col (cons (first lat) newlat) nleft nright)))))

(defn evens-only*
  "Removes all odd numbers from a list, including nested lists. 
   Returns the new list with only evens."
  [l]
  (cond
   (empty? l) l
   (list? (first l)) (cons (evens-only* (first l)) (evens-only* (rest l)))
   (even? (first l)) (cons (first l) (evens-only* (rest l)))
   :else (evens-only* (rest l))))
  

(defn evens-only*&co
  "A version of evens only that also multiplies the even numbers
   and sums up the odd numbers, then calls the function +col+
   passed in"
  [l col]
  (cond
   (empty? l) (col l 1 0)
   
   (list? (first l)) (evens-only*&co (first l)
                                     (fn [al ap as]
                                       (evens-only*&co (rest l)
                                                       (fn [newl eprod sodd]
                                                         (col (cons al newl) (* ap eprod) (+ as sodd))))))

   (even? (first l)) (evens-only*&co (rest l)
                                     (fn [newl eprod sodd]
                                       (col (cons (first l) newl) (* (first l) eprod) sodd)))

   :else (evens-only*&co (rest l)
                         (fn [newl eprod sodd]
                           (col newl eprod (+ (first l) sodd))))))




;; ------------------- ;;
;; ---[ Chapter 9 ]--- ;;
;; ------------------- ;;
   
(defn keep-looking
  "Looks through a lat for +a+. If +sorn+ is a number, it 
   uses it as an index into +lat+ and looks up that index.
   If that position in the +lat+ is a number, then it follows
   it to that index, etc., until it gets to a value that is 
   not a number, which it then compares to a, return true
   if the non-number matches a.  There, +a+ cannot be a number.
   This function is prone to infinite recursion if the 'follow 
   path' in the numbers of the lat are circular/form a loop."
  [a sorn lat]
  (if (number? sorn)
    (keep-looking a (pick sorn lat) lat)
    (= a sorn)))

(defn shift
  ""
  [pair-of-pairs]
  (build (first (first pair-of-pairs))
         (build (second (first pair-of-pairs)) (second pair-of-pairs))))

(defn length-gen*
  "Counts the number of atoms in +pora+"
  [pora]
  (empty? pora) 0
  (atom? (first pora)) (inc (length-gen* (rest pora)))
  :else (+ (length-gen* (first pora)) (length-gen* (rest pora))))


(defn length*
  "Counts the number of atoms in +pora+"
  [pora]
  (if (atom? (first pora))  ; don't need to check for empty list, since it
    1                       ; doesn't recuse on rest, but just first and second
    (+ (length* (first pora)) (length* (second pora)))))

(defn eternity
  "Eternally loops on itself"
  [x]
  (eternity x))

;; (defn length
;;   "Returns the length of a list."
;;   [lat]
;;   (if (empty? lat)
;;     0
;;     (inc (length (rest lat)))))

;; Define length0 as an anonymous function (p. 160)
;; two ways in Clojure to define anonymous functions
;; ... with fn
(fn [l] (if (empty? l) 0 (eternity (rest l))))
;; ... with #() reader feature
#(if (empty? %) 0 (inc (eternity (rest %))))

;; now test it
(println ((fn [l] (if (empty? l) 0 (eternity (rest l)))) '()))
(println (#(if (empty? %) 0 (inc (eternity (rest %))))   '()))


;; Define length<=1 (p. 161)
(fn [l]
  (cond
   (empty? l) 0
   :else (inc ((fn [l]
                 (cond
                  (empty? l) 0
                  :else (inc (eternity (rest l)))))
               (rest l)))))

; test it
(println ((fn [l]
           (cond
            (empty? l) 0
            :else (inc ((fn [l]
                          (cond
                           (empty? l) 0
                           :else (inc (eternity (rest l)))))
                        (rest l))))) '()))
(println ((fn [l]
           (cond
            (empty? l) 0
            :else (inc ((fn [l]
                          (cond
                           (empty? l) 0
                           :else (inc (eternity (rest l)))))
                        (rest l))))) '(2)))

;; page 168 - runs but blows the stack
;; (((fn [mk-length]
;;     (mk-length mk-length))
;;   (fn [mk-length]
;;     ((fn [length]
;;        (fn [l]
;;          (cond
;;           (empty? l) 0
;;           :else (inc (length (rest l))))))
;;        (mk-length mk-length))))
;;   '(:apples))

;; the Y-Combinator
(defn Y
  "The Y-Combinator"
  [le]
  ((fn [ff] (ff ff))
   (fn [fx]
     (le (fn [x] ((fx fx) x))))))


;; ------------------- ;;
;; ---[ Chapter 10]--- ;;
;; ------------------- ;;

(def new-entry build)

(defn lookup-in-entry-help
  "Helper method for lookup-in-entry-help
   that does the actual work"
  [name names values entry-f]
  (cond
   (empty? names) (entry-f name)
   (= name (first names)) (first values)
   :else (lookup-in-entry-help name 
                               (rest names) 
                               (rest values)
                               entry-f)))

(defn lookup-in-entry
  "+entry+ is defined as a pair of lists, where the first
   list is a set and the pairs have equal lengths - a 
   form of a map data structure.  This method looks for 
   +name+ is the entry set (names) and returns the value
   associated with that key (name).  If +name+ is not a key,
   then the entry-f method is invoked, passing it +name+."
  [name entry entry-f]
  (lookup-in-entry-help name
                        (first entry)
                        (second entry)
                        entry-f))
  
(defn lookup-in-table
  "+table+ is defined as a list of entries (see lookup-
   in-entry for definition of entry."
  [name table table-f]
  (if (empty? table) 
    (table-f name)
    (lookup-in-entry name
                     (first table)
                     (fn [name]
                       (lookup-in-table name
                                        (rest table)
                                        table-f)))))

;; this is my original version of lookup-in-table
;; and its helper method
;; before looking at the book answer
(defn lup-in-table-help
  [name names values table table-f]
  (cond
   (empty? table) (table-f name)
   (empty? names) (lup-in-table-help name
                                     (first  (first (rest table)))
                                     (second (first (rest table)))
                                     (rest table)
                                     table-f)
   (= name (first names)) (first values)
   :else (lup-in-table-help name
                            (rest names)
                            (rest values)
                            table
                            table-f)))

(defn lup-in-table
  [name table table-f]
  (lup-in-table-help name
                     (first  (first table))
                     (second (first table))
                     table
                     table-f))
  

(defn atom-to-action
  ""
  [a]
  (if (or (number? a) 
          (= a true)
          (= a false)
          (= a 'cons)
          (= a 'first)
          (= a 'rest)
          (= a 'atom?)
          (= a 'empty?)
          (= a '=)
          (= a 'zero)
          (= a 'inc)
          (= a 'dec)
          (= a 'number?)) 
    '*const
    '*identifier))
        

(defn list-to-action
  ""
  [l]
  (cond 
   (symbol? (first l))
   (cond
    (= (first l) 'cond)   '*cond
    (= (first l) 'lambda) '*lambda  ;; not in Clojure.core
    (= (first l) 'fn)     '*fn      ;; added for Clojure
    :else '*application)

   (list? (first l))
   (if (= (first (first l)) 'quote)
     '*quote
     '*application)
   :else '*application))

(defn expression-to-action
  ""
  [e]
  (if (atom? e)
    (atom-to-action e)
    (list-to-action e)))

(defn meaning
  ""
  [e table]
  ((expression-to-action e) e table))

(defn value
  ""
  [e]
  (meaning e '()))

