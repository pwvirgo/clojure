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
;; 23 reverse a sequence (without using reverse)
;;------------------------------------------------------------
(reduce #(apply str (cons %2  %1)) "" "hello" ) ; reverses a str

(reduce #(println  %2 "!" %1 "!") "xx" "hello" ) ; examine reduce parms

;; ----------------------------------------------
;; 27 Palindrome Detector
;;------------------------------------------------------------
;; good answer:
#(= (seq %) (reverse %))

;; my answer: 
((fn [sqnc] (loop [s sqnc]
      (if (<= (count s) 1) truex
         (do
            (if (not= (first s) (last s)) false
               (recur (drop-last 1 (drop 1 s))))))
              )) "aabxaa")

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
        :else (getmax (first s) (rest s)))
)


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
;; Write a function which removes consecutive duplicates from a sequence.
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
;;------------------------------------------------------------
(defn z [x y] 
  ((fn mkseq [j]
      (loop [j j rslt [] ]
        (if (= 0 j) 
          rslt
          (recur (quot j 10) (cons (rem j 10) rslt)))))
   (* x y)))

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


(   (fn [f s] (for [e s] [(f e) e] ))   #(> % 5) [1 3 6 8])


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

;; first try
(defn rbin [strng] 
  (loop [s (reverse strng) n 0 ans 0]
    (if s 
        (recur (next s) (inc n)
           (+ ans (* (Math/pow 2 n)
               (Character/digit (first s) 2)))  ) 
        (int ans))))

;; revised version 
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
;; #95 Binary Tree?
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

(defn po1 [s]
  (prn s)
  (cond (= s nil) true 
        (and (coll? s) (= 3 (count s))) 
           (if  (po1 (nth s 1))  (po1 (nth s 2)) false) 
        :else false))

(po1 [0 [12 nil nil] [11 [31 nil nil ] [51 nil nil]] ])
