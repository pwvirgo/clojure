(def vowel? (set "aeiou"))

(defn pig-latin [word] ; defines a function
  ; word is expected to be a string
  ; which can be treated like a sequence of characters.
  (let [first-letter (first word)] ; assigns a local binding
    (if (vowel? first-letter)
      (str word "ay") ; then part of if
      (str (subs word 1) first-letter "ay")))
) ; else part of if

(println (pig-latin "red"))
(println (pig-latin "orange"))

;;------------------------------------------------------------
;; #66 greatest common denominator
;;------------------------------------------------------------
(defn gcd [x  y]
    (if (or (<= x 0) (<= y 0))  "ERROR positive integers!"
        (loop [b (max x y) s (min x y)]
          (prn "b: " b " s:" s " (mod b s): "  (mod b s))
          (if (>= 0 (mod b s))
            s
            (recur s (mod b s))))))

(gcd 126 (* 2 49)) 
(println (newline) (gcd 33 2) )

(println (newline) (/ 66 (gcd 66 4)) )

(defn commonD
 "find all common denominators"
 [x y]
    (loop [a x, b y,  ret []]
      (let [g (gcd a b)]
        (if (= g 1) (into ret [a b])
            (recur (/ a g) (/ b g) (into ret [g]) )))))

(print (newline) (commonD (* 11 3) (* 11 12)))

(defn primes [n] 
  (loop [x n,  p 2, ret [] stop 0]
    (prn (newline) "n: " n " x: " x "  p: " p " :ret " ret) 
    (if  (or (= n (reduce * ret)) (> stop 500))  ret
        (if (= (rem x p) 0)
          (recur (/ n (reduce * p ret)), 2, (into ret [p]) (inc stop))
          (recur x, (if (= p 2) 3 (+ 2 p)), ret, (inc stop))))))

(primes 43) 


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




