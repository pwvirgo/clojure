;; May 2016 teaching myself clojure - and I find it very hard!
;;  this file is an accumlation as I work my way through through
;; the problems found at 4clojure.com
;;  user id: philv  password 7th
;;--------------------------------------------------------------

;; A nil key
;;Write a function which, given a key and map, returns true iff the map 
;;contains an entry with that key and its value is nil.
;;test not run	
;;
;;(true?  (__ :a {:a nil :b 2}))
;;test not run	
;;
;;(false? (__ :b {:a nil :b 2}))
;;test not run	
;;
;;(false? (__ :c {:a nil :b 2}))
;;------------------------------------------------------------

(fn [x y] (and (contains? y x) (= nil (x y))))
 
;;------------------------------------------------------------
;; #166 Comparisons  ???? I donot understand the instructions!
;;------------------------------------------------------------
(defn c [f x y]
  (cond (< x y) :lt
        (= x y) :eq
        (> x y) :gt
        :else :wtf?))

 (= :gt (c < 5 1))

;;------------------------------------------------------------
;; #156 Map defaults When retrieving values from a map, you can specify
;; default values in case the key is not found:
;; 
;; (= 2 (:foo {:bar 0, :baz 1} 2))
;; 
;; However, what if you want the map itself to contain the default
;; values? Write a function which takes a default value and a sequence of
;; keys and constructs a map.
;; 
;; (= (__ 0 [:a :b :c]) {:a 0 :b 0 :c 0})
;; 
;; (= (__ "x" [1 2 3]) {1 "x" 2 "x" 3 "x"})
;; 
;; (= (__ [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]})
;;------------------------------------------------------------

;; leifp
(fn [default ks] (into {} (for [k ks] [k default])))

;; cgrand
#(zipmap %2 (repeat %))

(fn [default keyz] (reduce (fn [m k] (assoc m k default)) {} keyz) )

;; ckirkendall's solution:
((fn [def keyz] (reduce #(assoc %1 %2 def) {} keyz)) 5 [2 3 4])

;;philV
(#(loop [zz %2  maap {}]
    (if (= ()  zz) 
      maap
      (recur (rest zz) (merge  maap  { (first zz) %1} ))
      )
    ) 5 [:a :s :q]) 


((fn [defy keyz]
    (loop [zz keyz  maap {}]
      (if (= ()  zz) 
        maap
        ;;(println zz maap)
        (recur (rest zz) (merge  maap  { (first zz) defy} ))
      )
    )) 5 [:a :s :q] )


;;
(= (__ (sort (rest (reverse [2 5 4 1 3 6]))))
   (-> [2 5 4 1 3 6] (reverse) (rest) (sort) (__)) (x)
   5)    

(= (__ '(4 5 6 7) 2) 6)

;;------------------------------------------------------------
;; problem #21 
;; Write a function which returns the Nth element from a sequence.
;;------------------------------------------------------------
; others
#(first (drop %2 %1))

#((vec %) %2)

; philV
((fn [urcol urcnt]  
   (loop [col  urcol cnt (inc urcnt)]
     (if (<= cnt 1) ;; should not be <  1! infinite loop safeguard
       (first col)
       (recur (rest col) (dec cnt))
     ) 
   )
) [1 2 3] 0)

;; --------------------------------------------
;; Count a Sequence (without using count)
;;------------------------------------------------------------
; phil v 
(defn myf [sqnc]
  (loop [s sqnc n 0] (if (empty? s) n (recur (rest s) (inc n))))
)


; chouser's solution:
reduce #(do %2 (+ 1 %)) 0

;(reduce #(do (println %2) %2 (+ 1 %)) 6  "hello")


;; ----------------------------------------------
;; #23 reverse a sequence (without using reverse)
;;------------------------------------------------------------
(reduce #(apply str (cons %2  %1)) "" "hello" ) ; reverses a str

(reduce #(println  %2 "!" %1 "!") "xx" "hello" ) ; examine reduce parms

;; ----------------------------------------------
;; #27 Palindrome Detector
;;------------------------------------------------------------
;; good answer:
#(= (seq %) (reverse %))

;; my answer: 
((fn [sqnc] (loop [s sqnc]
      (if (<= (count s) 1) true
         (do
            (if (not= (first s) (last s)) false
               (recur (drop-last 1 (drop 1 s))))))
              )) "aabaa")

;; ----------------------------------------------
;; #26 Fibonacci Sequence -
;;  Write a function which returns the first X fibonacci numbers.
;;------------------------------------------------------------
((fn [n]
   ;; r1 is the last entry in fibon; r2 is the 2nd to last
    (loop [r2 0  r1 1  cnt 2 fibon [1]] 
      (if (> cnt n) (seq fibon)
          (do
            (recur r1 (+ r1 r2) (inc cnt) (concat fibon [(+ r1 r2)]))))
    )) 6)



(defn fib
   "Fibonacci with algorithm from SICP lecture 1B MIT 1986
     SLOW - oder of (fibonacci) space wasting - order of (n)"
     [n]
   (if ( < n 2)
     n
     (+ (fib (- n 1)) (fib (- n 2)) )))
   

;;-----------------------------------------------
;; #38 Maximum Value
;; function to find  the max value of variable number of parmeters
;;  without using max
;;------------------------------------------------------------

;; philV solution 1
((fn [& args]
      (loop [ps (rest args) curr (first args) imax (first args) ]
        (if (= ps []) (if (> curr imax) curr imax)
            (recur (rest ps) (first ps)
                   (if (> curr imax) curr imax))))
 ) 2 7 8 16 29 100)

;; philV solution 2

(defn max0 [s]
  (defn getmax [big more]
    (if (empty?  more) big
        (recur (if (> big (first more)) big (first more)) (rest more))
     ;; or without tail call optimization
     ;; (getmax (if (> big (first more)) big (first more)) (rest more))
    )
  )
  (cond (empty? s) nil
        (= 1 (count s)) (first s)
        :else (getmax (first s) (rest s))))


;; -- others
(fn [& xs] (reduce (fn [mx x] (if (> x mx) x mx)) xs))

#(Last (sort %&))

;;------------------------------------------------------------
;; #29 Get the Caps
;; Write a function which takes a string and returns a new
;;  string containing only the capital letters.
;;------------------------------------------------------------

;; philv solution after about 2 hours of work!
(#(apply str (re-seq #"[A-Z]+" %)) "NiCe TEEth!")

;; other solutions
(fn [s] (apply str (filter #(Character/isUpperCase %) s)))

;;------------------------------------------------------------
;; #32 Duplicate a Sequence
;;------------------------------------------------------------

;; hey! This took me less that 5 minutes!
#(reduce (fn [a b ] (conj a b b) ) [] %)

;; other's solutions
#(for [x % y [x x]] y)

mapcat #(list % %)

#(interleave % %)

;;------------------------------------------------------------
;; #34 Implement a Range
;;------------------------------------------------------------

;; only took a few minutes
(fn [start end] (take (- end start) (iterate inc start)))
 
;; other solutions
#(take (- %2 %) (iterate inc %))  ;; same as mine

(fn [l u] (take-while #(< % u) (iterate inc l)))

;;------------------------------------------------------------
;; #30 Compress a Sequence
;; Write a function which removes consecutive duplicates from a sequence.
;;------------------------------------------------------------
;; this took 30 minutes because I got errors for ((empty? a) that 
;; ( changed to (nil? a) )  that I thought where acbout other parts
;; of the program 
(fn z [sqnc]
  (loop [a (first sqnc) s (rest sqnc) nodup []]
   (if (nil? a) nodup  
        (if (not= a (last nodup))
          (recur (first s) (rest s) (into nodup (vector a)))
          (recur (first s) (rest s) nodup)))))

;; other solutions
#(map first (partition-by identity %))

#(map last (partition-by max %))

#(mapcat distinct (partition-by identity %))

;;------------------------------------------------------------
;; #28 Flatten a Sequence
;; a function to remove consecutive duplicates from a sequence.
;;------------------------------------------------------------
;; oops I cannot erase this solution which I plagerized!
(fn flatten [x]
  (filter (complement sequential?)
          (rest (tree-seq sequential? seq x))))

;; this is the source of tree-seq 
(defn tr-seq
   [branch? children root]
   (let [walk (fn walk [node]
                (lazy-seq
                 (cons node
                  (when (branch? node)
                    (mapcat walk (children node))))))]
     (walk root)) )

;;------------------------------------------------------------
;; #19 Last Element
;; a function to return the last element of a seq (cant' use last)
;;------------------------------------------------------------

(fn z [s]
  (if (next s) (z (next s)) (first s) ))

;; ----------------- other's solutions
#(first (reverse %))
#(peek(vec %))
(fn [[x & xs]] (if xs (recur xs) x))

;;------------------------------------------------------------
;; #39 Interleave Two Seqs
;; (= (__ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
;; (= (__ [1 2] [3 4 5 6]) '(1 3 2 4))
;;------------------------------------------------------------ 
(defn x [s1 s2]
  (loop [r [] a s1 b s2] 
    (if (and (first a) (first b)) 
      (recur (concat r [(first a)] [(first b)])
                         (rest a) (rest b) )
      r)))

;------------ other's solutions
mapcat list

;;------------------------------------------------------------
;; #33 Replicate a Sequence (easy)
;; (= (__ [1 2 3] 2) '(1 1 2 2 3 3))
;; (= (__ [:a :b] 4) '(:a :a :a :a :b :b :b :b))
;;------------------------------------------------------------ 

;; horrible 45 minute first solution
((fn [seq reps]
   (let [rslt
         (for [j seq :let [x (repeat reps j)]] x )]
     (for [e1 rslt e2 e1] e2)

     )) [:a :b :c] 2)

;; better! solution plagerized
(fn [seq reps] (for [j seq e (repeat reps j)] e))


;;------------------------------------------------------------
;; #40 Replicate a Sequence (easy)
;a function which separates the items of a sequence by an arbitrary value.
;; (= (__ 0 [1 2 3]) [1 0 2 0 3])
;; (= (apply str (__ ", " ["one" "two" "three"])) "one, two, three")
;;------------------------------------------------------------

;; my clumsy 30 minute solution!
(defn z [x seq]
  (loop [s seq rslt []] 
    (if (next s) 
      (recur (rest s) (concat rslt (list (first s)  x)))
      (concat rslt (list (first s))))  ))

;; variations on better solutions
#(rest (mapcat list (repeat %1) %2))

(fn z [x s] (drop-last (mapcat #(list % x) s)))

;;------------------------------------------------------------
;; #31 Pack a Sequence (easy)
;; (= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
;; (= (__ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))
;; a function which packs consecutive duplicates into sub-lists.
;;------------------------------------------------------------

;; --- philv solution after about 3 hours!
(fn z [s]
  (loop [s s  rslt '()]
    (if (empty? s)
      rslt
      (let [c (count (take-while #(= (first s) %) s))]
        (recur (drop c s)
               (concat rslt (list (take c s)) ))) )))

;; --- other solutions
partition-by identity

partition-by #(do %)


;;------------------------------------------------------------
;; #41 Drop every nth item from a sequence
;;(= (__ [:a :b :c :d :e :f] 2) [:a :c :e])
;;------------------------------------------------------------
(defn z [s n]
  (doseq [ m  (range (count s))]
    (if (> (mod (inc m) n) 0) (prn (nth s m))))
  )

(fn z [seq n]
  (loop [cnt 0 rslt [] ]
    (if (<= (count seq) cnt)
      rslt 
      (recur (inc cnt) (if (> (mod (inc cnt) n) 0) 
                 (concat rslt [(nth seq cnt)])
                 rslt)))))

;;------------------------------------------------------------
;; #49 Slpit a sequence in 2 at the index (cannot use split-at)
;;------------------------------------------------------------
;; (= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])

#(cons (take %1 %2) [(drop %1 %2)])

;; by others
(juxt take drop)

;;------------------------------------------------------------
;; #83 half truth
;; Write a function which takes a variable number of booleans.
;; return true if some, but not all, of the parameters are true
;; of the parameters are true. Otherwise  return false.
;;------------------------------------------------------------

((fn [ & z]
   (let [x (vec z)]
     (if (and (some true? x) (some false? x)) true false))  )  false true)

;; others' solutions

#(= 2 (count (group-by true? %&)))

not=

#(= 2 (count (set %&)))

;;------------------------------------------------------------
;; #61 map construction: take a vector of keys and a vector
;;  of values and construct a map from them.  Cannot use
;; zipmap
;;------------------------------------------------------------

((fn [keys vals]
   (loop [k (seq keys) v (seq vals) rslt {}]
     (if (or (empty? k) (empty? v)) rslt
         (recur (rest k)  (rest v) 
                (assoc  rslt (first k) (first v))))
     )) [:foo :bar] ["foo" "bar" "baz"] )

;;--- other solutions

(fn my-zipmap [ks vs] (into {} (map vector ks vs)))

#(into {} (map vector %1 %2))

#(reduce into (map hash-map % %2))


;;------------------------------------------------------------
;; #66 greatest common denominator
;;------------------------------------------------------------
((fn gcd [x  y]
   (if (or (<= x 0) (<= y 0))  "ERROR positive integers!"
     (loop [b (max x y) s (min x y)]
       (if (>= 0 (mod b s))
         s
         (recur s (mod b s))))
)) 16  2144 )

;;------------------------------------------------------------
;; #81 Intersection of 2 sets without using intersection
;;------------------------------------------------------------

((fn is [s1 s2]
   (set (mapcat (fn z [s1] (if (contains? s2 s1) [s1])
                  ) s1)) ) #{1 2 3 5} #{3 4 5})

(comp set filter)


;;------------------------------------------------------------
;; #62 Re-implement Iterate
;;------------------------------------------------------------
;; I copied this from clojure iterate!
(fn it [f x] (cons x (lazy-seq (it f (f x)))))

 
;;------------------------------------------------------------
;; #107 Simple closures
;; @param exp the exponent
;; @return a function to raise a base to the exp
;;------------------------------------------------------------
(fn pow [exp] (fn [base] (biginteger (Math/pow base exp))))


;;------------------------------------------------------------
;; #99 Product digits
;; multiply 2 numbers returning a sequence of its digits.
;;------------------------------------------------------------

(fn [x y] 
  (loop [j (* x y) rslt [] ]
    (if (= 0 j)  rslt
      (recur (quot j 10) (cons (rem j 10) rslt)))))

;;------------------------------------------------------------
;; #90 Cartesian product of 2 sets
;;------------------------------------------------------------
(fn [a b]  (set (for [j a k b]  [j k] )))

;;------------------------------------------------------------
;; #63 Group a Sequence
;;------------------------------------------------------------

(
  (fn [f s] (for [e s] [(f e) e] ))

   #(> % 5) [1 3 6 8])


;;---  demo of a working way to accumulate results in map m
(def m {:a [1 2]})
(vec (conj (:a m ) 8))

(conj m {:a (vec (conj (:a m) 9))})
(conj m {:b (vec (conj (:b m) 9))})
;;---  end demo of a working way to accumulate results in map m

((fn [f seq]
   (loop [s seq  el (first s) rslt {}]
     (if (empty? s)
       rslt
       (recur (rest s) (first (rest s))
           (conj rslt {(f el) (vec (conj (get rslt (f el)) el))})
                    )))) #(> % 5) [1 3 6 8]  )


;; --- other solutions
#(apply merge-with into (map (fn [x] {(% x) [x]}) %2))

#(apply merge-with into
        (for [x %2]
          {(% x) [x]}))

#(reduce
(fn [m x] (assoc m (% x) (conj (m (% x) []) x))) 
{} %2)

(fn [f s]
  (let [
    ; Create seq of maps ( {(f x1) -> [x1]}, ...)
    y-vx-map (map #(hash-map (f %1) (vector %)) s) ]
    
    ; Merge maps, combining vals of dup keys
    (apply merge-with concat y-vx-map)))

;;------------------------------------------------------------
;; #88 Symmetric Difference
;;  
;;------------------------------------------------------------

(fn symdif [set1 set2]
  (clojure.set/difference
   (clojure.set/union set1 set2)
   (clojure.set/intersection set1 set2))
) 

;; --- other solutions
#(set (concat (remove % %2) (remove %2 %)))

#(into (set (remove %2 %)) (remove % %2))

;;------------------------------------------------------------
;; #122 Read a binary number
;;  
;;------------------------------------------------------------

;; pwv  version 
(fn rbin [strng] 
  (loop [s strng ans 0]
    (if s
      (recur (next s) 
             (+ (* 2 ans) (Character/digit (first s) 2))) 
      (int ans))))

;;  --- other solutions

;;  this works but I do not understand it!
reduce #(+ % % ({\0 0} %2 1)) 0

#(Integer/parseInt % 2)

#(read-string (str "2r" %))

(fn [s]
  (apply + (map #({\1 %2} % 0) 
                 (reverse s) 
                 (iterate #(* 2 %) 1))))

;;;;;;;;;;;; study this! ;;;;;;;;;;;;;
(reductions #(+ % % %2) 0 [1 1 1 1])
;;;;;;;;;;;; study this! ;;;;;;;;;;;;;

;;------------------------------------------------------------
;; #143 Dot Product (the sum of the elements with the same
;; offset in 2 vectors
;;------------------------------------------------------------

((fn dp [v1 v2]
   (reduce + (map * v1 v2))) [1 2 3] [1 2 3])

;;------------------------------------------------------------
;; #126 Through the looking class
;;  enter a value that satisfies this:
;; (let [x __] (and (= (class x) x) x))
;;------------------------------------------------------------

(let [x java.lang.Class]
  (and (= (class x) x) x))

;;------------------------------------------------------------
;; #135
;;  infix calculator
;;------------------------------------------------------------

((fn [a & args] 
   (loop [ret a,  s args]
     (if (empty? s)  ret
         (recur  ((first s)  ret (second s)) 
                (rest (rest s))) ))) 1 + 2 * 7 / 3)

;; ---- other's solutions
(fn [x & xs]
  (reduce (fn [x [f y]] (f x y)) x
    (partition 2 xs)))

(fn f [a o b & c]
  (if c
    (apply f (o a b) c)
    (o a b)))

;;------------------------------------------------------------
;; #157
;;  Indexing Sequences
;; Transform a sequence into a sequence of pairs containing
;;  the original elements along with their index.
;;------------------------------------------------------------

((fn [s1]
   (loop [s s1, n 0, rslt []]
     (if (empty? s) rslt 
         (recur (rest s) (inc n) (concat rslt [[(first s) n]]))))
) [:a :b :c])

#(map list % (range))

map-indexed #(list %2 %)

#(map-indexed (fn [a b] [b a]) %)

;;------------------------------------------------------------
;; #97 Pascal's Triangle - 
;; a function to produce the sequence of numbers in a row.
;;
;; The first row is 1.
;; - Each successive row is computed by adding together adjacent
;;  numbers in the row above, and adding a 1 to the beginning and 
;;  end of the row.
;;  
;;------------------------------------------------------------

(defn rowOfpascalTriangle  "loops thru columns of a single row" [row]
   (loop [col 0, z 1, ret []]
     (cond  
      (>= col row) ret
      (= col 0 )  (recur (inc col) z [z])
      :else  (let [q  (* z (/ (- row col) col))]
               (recur (inc col) q (conj ret (int q)) )))))

(map pascal (range 1 12))

;;------------------------------------------------------------
;; #118 Re-implement Map 
;;------------------------------------------------------------
((fn mmap [f sqnc]
   (lazy-seq
    (when-let [s (seq sqnc)]
      (cons  (f (first s)) (mmap f (rest s)))))
) inc [1 2 3])

;;----------- this will time out if passed something big
;;  probably an infinite loop on (range)
((fn map [f sqnc]
   (lazy-seq
    (loop [s sqnc ret []]
      (if (empty? s)
        ret
        (recur (next s) (conj ret (f (first s))))
        )))
) inc [1 2 3])

;;------------------------------------------------------------
;; #95 To Tree, or not to Tree
;; Write a predicate which checks whether or not a given
;; sequence represents a binary tree. Each node in the tree 
;; must have a value, a left child, and a right child.
;;------------------------------------------------------------
;; 1-  (not (coll? (first s)))
;; 2-  (= 3 (count s))
;; 3-  each branch must be nill or btree
;;
;;  this took me several hours over 2 days after looking
;;  up a java recursion to traverse a binary tree
;;  assumed to be valid.

;; pre-order recursive traversal
(defn po1 [s]
  (cond (= s nil)  true 
        (and (coll? s) (= 3 (count s))) 
           (if  (po1 (nth s 1))  (po1 (nth s 2)) false) 
        :else false))

(po1 [0 [12 nil nil] [11 [22 nil [31 nil nil] ] [21 nil nil]] ])


;;------------------------------------------------------------
;; #120 Sum of Square of digits
;;
;; Write a function which takes a collection of integers as an
;; argument. Return the count of how many elements are smaller than
;; the sum of their squared component digits. For example: 10 is
;; larger than 1 squared plus 0 squared; whereas 15 is smaller than 1
;; squared plus 5 squared.
;;----------------------------------------------------
(defn ssd [seq]
  (let [
      ; @param n  an integer
      ; @return  a vector of ints containing the digits
      ;  of the int parameter (eg 245 -> [2 4 5])
      digits (fn [n] (vec (map #(Integer/parseInt (str %)) (str n))))

      ; @param v a vector of integers
      ; @return the sum of the squares of the contents of v
      sumsq (fn [v] (reduce  #(+ % (Math/pow %2 2)) 0 v))]

    (loop [s seq n 0] ;; for every number
      (if (empty? s) n  ;; if no more numbers return the count
          (if (< (first  s) ((comp sumsq digits) (first s)) ) 
            (recur (rest s) (inc n))
            (recur (rest s) n))))))

(ssd [10 15 25 356 512])
(time  (ssd (range 1000)))



;;  the logic above is:
for every number in a sequence 
{
    find the digits
    find the ss of digits
    compare ssd to number
    add to count if number is smaller than the square of its digits
}
return the count after processing the last number

;;---  other's solutions
(time ((fn [xs]
    (count (keep (fn [x]
       (if (< x 
           (reduce + (map #(Math/pow (- (int %) 48) 2) (str x)))
           ) 1)  ) xs))) (range 1000)))

(time ((fn prob-0120 [ns]
          (count
           (for [n ns
                 :let [sqd-digs (map #(* % %)
                    (map #(Long/parseLong (str %)) (seq (str n))))]
                 :when (< n (apply + sqd-digs)) ]
             n))) (range 1000)))

;;------------------------------------------------------------
;; #128 Recognize Playing Cards
; Difficulty:	Easy
; Topics:	strings game

;A standard American deck of playing cards has four suits - spades,
;hearts, diamonds, and clubs - and thirteen cards in each suit. Two is
;the lowest rank, followed by other integers up to ten; then the jack,
;queen, king, and ace.

;It's convenient for humans to represent these cards as suit/rank
;pairs, such as H5 or DQ: the heart five and diamond queen
;respectively. But these forms are not convenient for programmers, so
;to write a card game you need some way to parse an input string into
;meaningful components. For purposes of determining rank, we will
;define the cards to be valued from 0 (the two) to 12 (the ace)

;Write a function which converts (for example) the string "SJ" into a
;map of {:suit :spade, :rank 9}. A ten will always be represented with
;the single character "T", rather than the two characters "10".

#_(comment
(= {:suit :diamond :rank 10} (__ "DQ"))

(= {:suit :heart :rank 3} (__ "H5"))

(= {:suit :club :rank 12} (__ "CA"))
test not run	

(= (range 13) (map (comp :rank __ str)
                   '[S2 S3 S4 S5 S6 S7
                     S8 S9 ST SJ SQ SK SA]))
)
;----------------------------------------------------------

(defn card [[suit rank]]
  {:suit ((zipmap  [\C \D \H \S] [:club :diamond :heart :spade]) suit)
   :rank ((zipmap (seq "23456789TJQKA") (range 13)) rank)})

(:rank (card "C3"))

(= (range 13) (map (comp :rank card str)
                   '[S2 S3 S4 S5 S6 S7
                     S8 S9 ST SJ SQ SK SA]))

;----------------------------------------------------------
; #100 Least Common Multiple
; Write a function which calculates the least common multiple. Your
; function should accept a variable number of positive integers or
; ratios.
;
; 3 5 7 => 105;  3/4 1/6 => 3/2;  7 5/7 2 3/5 => 210
;----------------------------------------------------------

(defn lcm 
  "Least common multiple"
  ([x y]
   (let [mx (max x y), mn (min x y)]
     (loop [times 1]
       (let [ret (* times mx)]
         (if  (= 0 (rem ret mn)) ret 
              (recur  (inc times) ))) )))
  ([x y & z] 
      (reduce #(lcm % %2) (into [x y]  (into []  z)))))


(lcm 7 5/7 2 3/5)

;------------- other's solutions

  
(fn f [a & b]
  (loop [i a]
    (if (every? #(zero? (mod i %)) b)
      i
      (recur (+ i a)))))

(fn lcm [& args]
  (/ (reduce * args)
     (reduce (fn [m n] (if (zero? n) m (recur n (mod m n)))) args)))

;----------------------------------------------------------
; #147 Pascal's Trapezoid Write a function that, for any given input
;; vector of numbers, returns an infinite lazy sequence of vectors, where
;; each next one is constructed from the previous following the rules
;; used in Pascal's Triangle. For example, for [3 1 2], the next row
;; is [3 4 3 2].

;; Beware of arithmetic overflow! In clojure (since version 1.3 in 2011),
;; if you use an arithmetic operator like + and the result is too large
;; to fit into a 64-bit integer, an exception is thrown. You can use +'
;; to indicate that you would rather overflow into Clojure's slower,
;; arbitrary-precision bigint.

(= (second (__ [2 3 2])) [2 5 5 2])

(= (take 5 (__ [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])

(= (take 2 (__ [3 1 2])) [[3 1 2] [3 4 3 2]])

(= (take 100 (__ [2 4 2])) (rest (take 101 (__ [2 2]))))
;----------------------------------------------------------

;------ here is a great sloution that others used!
iterate #(vec (map + (conj % 0) (cons 0 %)))

;---------- below is what I wrote
(take 5 (
(fn z [seq ] 
   (iterate 
     (fn pt [seq]
       (if (= 1 (count seq)) (conj [] (first seq ) (first seq))
         (loop [left (first seq), s (rest seq),
                right (first s), ret []]
             (if (empty? s) (conj ret left)
                 (if (empty? ret) 
                   (recur left,  s, right, [left])
                   (recur right, (rest s),  (first (next s)),  
                          (conj ret (+' left right))))))))
            seq)) [3 1 2]))

;----------------------------------------------------------
;; #96 Beauty is Symmetry 
;; Let us define a binary tree as symmetric if the left half of
;; the tree is the mirror image of the right half of
;; the tree. Write a predicate to determine whether or not a
;; given binary tree is symmetric. 



(= (__ '(:a (:b nil nil) (:b nil nil))) true)

(= (__ '(:a (:b nil nil) nil)) false)

(= (__ '(:a (:b nil nil) (:c nil nil))) false)

(= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
   true)

(= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
   false)

(= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] nil]] nil]])
   false)
;----------------------------------------------------------
;; my solution (not great!) 
;; Do a pre-order traversal of the tree going first left and
;; then right and create a vector of each node and leaf as they
;; are encountered.  Then do it again going first right and then
;; left.  compare the vectors - if they match we have symmetry. 
;;
;  this took many hours over several days to accomplish
;----------------------------------------------------------

(defn pz [s]
  (let [ret1 (atom [])  ret2 (atom [])
        trvrs (fn pozd [s l-r ret] 
                (cond (= s nil)  (do (swap! ret conj s) @ret) 
                      (and (coll? s) (= 3 (count s))) 
                      (do  
                        (swap! ret conj (first s))
                        (if  (pozd (nth s (l-r 0)) l-r ret) 
                          (pozd (nth s (l-r 1)) l-r ret)
                          false)) 
                      :else false))]
    (= (trvrs s [1 2] ret1) (trvrs s [2 1] ret2))
    ;  (println @ret1)  (println @ret2)
   ))


(= (pz '(:a (:b nil nil) (:b nil nil))) true)

(= (pz '(:a (:b nil nil) nil)) false)

(= (pz '(:a (:b nil nil) (:c nil nil))) false)

(= (pz [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
   true)

(= (pz [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
   false)

(= (pz [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
          [2 [3 nil [4 [6 nil nil] nil]] nil]])
   false)

;;--------------------------------------------------------
;; here is a much nicer solution by leifp.  Which Pablo also
;; outlined  when I told him the problem
;; -------------------------------------------------------
(fn symmetric? [tree]
  (letfn
      [(flip [tree]
         (if (nil? tree)
           nil
           (let [[v l r] tree]
             [v (flip r) (flip l)])))
       ]
    (= tree (flip tree))))

;-----------------------------------------------------------
; #153 pairwise Disjoint sets
; Given a set of sets, create a function which returns true if no two
; of those sets have any elements in common1 and false otherwise.
; Some of the test cases are a bit tricky, so pay a little more
; attention to them.
;
; 1 Such sets are usually called pairwise disjoint
;   or mutually disjoint.
;-----------------------------------------------------------

(= (__ #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
   true)

(= (__ #{#{:a :b :c :d :e}
         #{:a :b :c :d}
         #{:a :b :c}
         #{:a :b}
         #{:a}})
   false)

(= (__ #{#{[1 2 3] [4 5]}
         #{[1 2] [3 4 5]}
         #{[1] [2] 3 4 5}
         #{1 2 [3 4] [5]}})
   true)

(= (__ #{#{'a 'b}
         #{'c 'd 'e}
         #{'f 'g 'h 'i}
         #{''a ''c ''f}})
   true)

(= (__ #{#{'(:x :y :z) '(:x :y) '(:z) '()}
         #{#{:x :y :z} #{:x :y} #{:z} #{}}
         #{'[:x :y :z] [:x :y] [:z] [] {}}})
   false)

(= (__ #{#{(= "true") false}
         #{:yes :no}
         #{(class 1) 0}
         #{(symbol "true") 'false}
         #{(keyword "yes") ::no}
         #{(class '1) (int \0)}})
   false)

(= (__ #{#{distinct?}
         #{#(-> %) #(-> %)}
         #{#(-> %) #(-> %) #(-> %)}
         #{#(-> %) #(-> %) #(-> %)}})
   true)

(= (__ #{#{(#(-> *)) + (quote mapcat) #_ nil}
         #{'+ '* mapcat (comment mapcat)}
         #{(do) set contains? nil?} 
         #{, , , #_, , empty?}})
   false)

(= (__ #{#{(#(-> *)) + (quote mapcat) #_ nil}
         #{'+ '* mapcat (comment mapcat)}
         #{(do) set contains? nil?}
         #{, , , #_, , empty?}})
   false)


(use '[clojure.set :as sett])

;;- - my solution took an hour!
(fn z [s]
  (= (count (apply clojure.set/union s))
     (apply + (for [a s] (count a)) )))

;; -- another (better)  solution
#(apply distinct? (mapcat seq %))

;;-----------------------------------------------------------
;; #46 Flipping out
;; Write a higher-order function which flips the order of the
;; arguments of an input function.
;;-----------------------------------------------------------
(= 3 ((__ nth) 2 [1 2 3 4 5]))

(= true ((__ >) 7 8))

(= 4 ((__ quot) 2 8))

(= [1 2 3] ((__ take) [1 2 3 4 5] 3))

;--- solution took about 15 minutes

(defn f [g] (fn [& args] (apply g (reverse args))))

;;-----------------------------------------------------------
;; #44 Rotate Sequence
;; Difficulty:	Medium Topics:	seqs
;; Write a function which can rotate a sequence in either direction.
;;-----------------------------------------------------------
(= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))

(= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))

(= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))

(= (__ 1 '(:a :b :c)) '(:b :c :a))

(= (__ -4 '(:a :b :c)) '(:c :a :b))
;;-------------------------------------------------------------

;--------- my solution
(defn rs [ndx seq]
  (let [f (fn [n] (concat (drop n seq) (take n seq)))
        n (rem ndx (count seq))]
    (if (pos? n) (f n) (f (+ 1 (- n)))))) 

;-------- but clojure has a very odd (to me) mod operator which
;--- is perfect fo this problem
#(let [i (mod % (count %2))] (concat (drop i %2) (take i %2)))


;;-------------------------------------------------------------
;; #43 Reverse Interleave
;; Difficulty:	Medium  Topics:	seqs
;;
;; Write a function which reverses the interleave process into x
;; number of subsequences.

(= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))

(= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))

(= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))
;;-------------------------------------------------------------

(defn ri [seq cnt]
  (loop [s seq  ret (repeat cnt [])]
    (if (empty? s) ret
        (recur (drop cnt s) 
           (for [j (range cnt)]
              (conj (nth ret j) (nth s j)))))))


;;----------  better solutions

(fn [coll n] 
  (for [i (range n)] (take-nth n (nthnext coll i))))

#(apply map list (partition %2 %))


;;-------------------------------------------------------------
;; # 50 Split by Type  Difficulty: Medium Topics: seqs

;;Write a function which takes a sequence consisting of items with
;;different types and splits them up into a set of homogeneous
;;sub-sequences. The internal order of each sub-sequence should be
;;maintained, but the sub-sequences themselves can be returned in any
;;order (this is why 'set' is used in the test cases).

(= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})

(= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})

(= (set (__ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})


;;----  this may be the 1st time I used a simple prebiult fn?
#(vals (group-by type %))


;;-------------------------------------------------------------
Count Occurrences  Difficulty: Medium Topics: seqs core-functions
Special Restrictions: frequencies


;;Write a function which returns a map containing the number of
;;occurences of each distinct item in a sequence.

(= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})

(= (__ [:b :a :b :a :b]) {:a 2, :b 3})

(= (__ '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})
;;-------------------------------------------------------------

(defn frq [s]
  (let [g (group-by identity s)] 
    (reduce #(assoc % (key %2) (count (val %2))) {} g) ))

;;----------- other's solutions

;;  wow, this is clever!
reduce #(assoc % %2 (+ 1 (% %2 0))) {}

(fn [s]
  (into {} 
    (map (fn [[k v]] [k (count v)]) 
         (group-by identity s))))

;;-------------------------------------------------------------
Find Distinct Items Difficulty: Medium Topics: seqs core-functions

;; Write a function which removes the duplicates from a
;; sequence. Order of the items must be maintained.

(= (__ [1 2 1 3 1 2 4]) [1 2 3 4])

(= (__ [:a :a :b :b :c :c]) [:a :b :c])

(= (__ '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))

(= (__ (range 50)) (range 50))
;;-------------------------------------------------------------

;;  2 solutions with same logic:

(defn z [s]
  (loop [s s ret []]
    (if (empty? s)
      ret
      (recur (next s)
             (if  (some #(= (first s) %) ret) 
               ret
               (conj ret (first s)))))))

(reduce (fn [ret x] (if (some #(= x %) ret) ret (conj ret x))) [] [1 2 2 3])

;;-------------------------------------------------------------

#58 Function Composition
 Difficulty: Medium Topics: higher-order-functions core-functions

;; Write a function which allows you to create function
;; compositions. The parameter list should take a variable number of
;; functions, and create a function that applies them from
;; right-to-left.

(= [3 2 1] ((z rest reverse)  [1 2 3 4]))

(= 5 ((comp (partial + 3) second)  [1 2 3 4]))  

(= true ((z zero? #(mod % 8) +) 3 5 7 9))

(= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world"))
;;-------------------------------------------------------------
(defn z [& fns]
  (fn [& args]
    (loop [s (reverse fns) ret (apply (first s) args)]
      (if (next s) 
        (recur (next s) ((first (next s)) ret) )
        ret))))
;;-------------------------------------------------------------
;; #54 Partition a Sequence  Difficulty: Medium  Topics: seqs core-functions

;; Write a function which returns a sequence of lists of x items
;; each. Lists of less than x items should not be returned.

(= (z  3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))

(= (z 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))

(= (z 3 (range 8)) '((0 1 2) (3 4 5)))
;;-------------------------------------------------------------

(defn z [n seq]
  (loop [s seq ret []]
    (if (empty? (drop (- n 1) s )) ret
        (recur (drop n s), (conj ret (take n s)) ))))

;; other solutions
(fn p [n x]
  (if (>= (count x) n)
    (cons (take n x) (p n (drop n x)))))

;;-------------------------------------------------------------
;; #59 Juxtaposition
 
Difficulty: Medium Topics: higher-order-functions core-functions
Special Restrictions juxt

;; Take a set of functions and return a new function that takes a
;; variable number of arguments and returns a sequence containing the
;; result of applying each function left-to-right to the argument
;; list.

(= [21 6 1] ((z + max min) 2 3 5 1 6 4))

(= ["HELLO" 5] ((z #(.toUpperCase %) count) "hello"))

(= [2 6 4] ((z :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))
;;-------------------------------------------------------------

(defn z [& fns]
  (fn [& args]
    (loop [s fns ret [(apply (first s) args)]]
      (if (next s) 
        (recur (next s) (conj ret (apply (first (next s)) args) ) )
        ret))))

;----- other's solutions
(fn [& o] (fn [& a] (map #(apply % a) o)))

(fn [& s] #(for [f s] (apply f %&)))

;;-------------------------------------------------------------
;; #70 Word Sorting Difficulty: Medium Topics: sorting

;; Write a function that splits a sentence up into a sorted list of
;; words. Capitalization should not affect sort order and punctuation
;; should be ignored.

(= (z  "Have a nice day.")
   ["a" "day" "Have" "nice"])

(= (z  "Clojure is a fun language!")
   ["a" "Clojure" "fun" "is" "language"])

(= (__  "Fools fall for foolish follies.")
   ["fall" "follies" "foolish" "Fools" "for"])
;;-------------------------------------------------------------

(defn z [s]
  (sort-by clojure.string/upper-case (clojure.string/split  s #"[\W+]")))

;;-------------------------------------------------------------
Prime Numbers Difficulty: Medium Topics: primes

;; Write a function which returns the first x number of prime numbers.

(= (__ 2) [2 3])

(= (__ 5) [2 3 5 7 11])

(= (last (__ 100)) 541)
;;-------------------------------------------------------------
;; I hadn't done any programming for several weeks and had to
;;  remember and re-research clojure - this problem took a few
;;  hours.

(defn p4 [n]
  (let [prime? (fn [n]
    (let [q (Math/sqrt n)]
      (loop [j 2]
        (cond (> j q)          true  # no divisor found
              (= 0 (mod n j))  false # divisor found
              :else (recur (if (= j 2) (+ j 1) (+ j 2)))))))]


    (loop [anum 2, ret (transient [])]
      (if (> n (count ret))
        (recur (inc anum), (if (prime? anum) (conj! ret anum) ret))
        (persistent! ret)))))

;;------------- other's solutions
(fn n-primes [n]
  (letfn
    [(step [[i ps]]
      (if (some #(= 0 (mod i %)) (take-while #(>= i (* % %)) ps)) ;i not prime
        (recur [(inc i) ps])
        [(inc i) (conj ps i)]))]
    (second (last (take n (iterate step [3 [2]]))))))
;;--------------------------

#(take %2 (remove (set (for [i % j (range (+ i i) 999 i)] j)) %))
(range 2 999) 


;;-------------------------------------------------------------
;; #65  Black Box Testing Difficulty: Medium Topics: seqs testing

;; Clojure has many sequence types, which act in subtly different
;; ways. The core functions typically convert them into a
;; uniform "sequence" type and work with them that way, but it can be
;; important to understand the behavioral and performance differences
;; so that you know which kind is appropriate for your application.

;;Write a function which takes a collection and returns one
;;of :map, :set, :list, or :vector - describing the type of collection
;;it was given.

;; You won't be allowed to inspect their class or use the built-in
;; predicates like list? - the point is to poke at them and understand
;; their behavior.

(= :map (z {:a 1, :b 2}))

(= :list (z (range (rand-int 20))))

(= :vector (z [1 2 3 4 5 6]))

(= :set (z #{10 (rand-int 5)}))

(= [:map :set :vector :list] (map z [{} #{} [] ()]))
;;-------------------------------------------------------------

(defn z [s]
  (let [q1 (conj s [:x 2]), q2 (conj q1 [:x 2]),
        q3 (conj q2 [:x 3])] 
       (println q3 " " (count q1) " " (count q2) " "
                (count q3))
       (cond (= (count q1) (count q3)) :map
             (= (count q1) (count q2)) :set
             (= [:x 3] (last q3))      :vector
             :else                     :list)))

(defn zall [s1 s2 s3 s4] 
  (println (z s1) " " (z s2) " " (z s3) " "(z s4)))


;;--------- other solutons
(fn [coll]
  (let [e (empty coll)]
    (cond 
      (identical? e ()) :list
      (= e []) :vector
      (= e #{}) :set
      (= e {}) :map
      )))

#(cond
   (= (conj % nil) %) :map 
   (= (conj % 0) (conj % 0 0)) :set
   (= (conj % 0 1) (cons 1 (cons 0 %))) :list
   :else :vector)

#((zipmap (map str [{} #{} () []])
          [:map :set :list :vector]) (str (empty %)))


;;-------------------------------------------------------------
# 74 Filter Perfect Squares Difficulty: Medium	

;; Given a string of comma separated integers, write a function which
;; returns a new comma separated string that only contains the numbers
;; which are perfect squares.

(= (__ "4,5,6,7,8,9") "4,9")

(= (__ "15,16,25,36,37") "16,25,36")
;;-------------------------------------------------------------

(defn fps [s]
  (let [pars (map #(Integer/parseInt %) (clojure.string/split s #","))
        ps? (fn [x] 
              (if (= (* (int (Math/sqrt x)) (int (Math/sqrt x))) x)
                true false)) ]
    (clojure.string/join "," (filter ps? pars))))

;;-------------------------------------------------------------

#76 Intro to Trampoline Difficulty: Medium Topics: recursion
;; The trampoline function takes a function f and a variable number of
;; parameters. Trampoline calls f with any parameters that were
;; supplied. If f returns a function, trampoline calls that function
;; with no arguments. This is repeated, until the return value is not
;; a function, and then trampoline returns that non-function
;; value. This is useful for implementing mutually recursive
;; algorithms in a way that won't consume the stack.
;;-------------------------------------------------------------

(= [1 3 5 7 9 11]
   (letfn
     [(foo [x y] #(bar (conj x y) y))
      (bar [x y] (if (> (last x) 10)
                   x
                   #(foo x (+ 2 y))))]
     (trampoline foo [] 1)) )

;;-------------------------------------------------------------
Anagram Finder  Difficulty: Medium  Topics:

;; Write a function which finds all the anagrams in a vector of
;; words. A word x is an anagram of word y if all the letters in x can
;; be rearranged in a different order to form y. Your function should
;; return a set of sets, where each sub-set is a group of words which
;; are anagrams of each other. Each sub-set should have at least two
;; words. Words without any anagrams should not be included in the
;; result.

(= (__ ["meat" "mat" "team" "mate" "eat"])
   #{#{"meat" "team" "mate"}})

(= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
   #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})
;;-------------------------------------------------------------

(defn z [v]
  (for [x  v]
      [ (clojure.string/join (sort x)) x]))

(z ["abc" "e1g" "acca" "bca"])


(def gb (group-by first  (z ["abc" "zz" "q5" "bac"])))

(for [x  (gb "abc")] (second x))  

(set (for [x (keys gb)] (set (for [y (gb x)] (second y))))) ;!!!

;; putting all that together in a single function:

;; this took me > 10 hours!  I doubt I'll understand it if I
;; try to read it in the future! 
(defn z [v]
  (let [gb (group-by first (for [x  v]
             [(clojure.string/join (sort x)) x]))]
    (set (filter #(> (count %) 1)  (for [y (keys gb)] (set (for [r (gb y)] (second r))))))))

(z ["meat" "mat" "team" "mate" "eat"])

(z ["veer" "lake" "item" "kale" "mite" "ever"])
