;; -------------------    #1
;; the function below is a very bad approach to a problem
;; BUT the behavior of Clojure is even worse!

;; the fn correctly creates an atom "rslt" which
;; contains the cartesian product of the paramters - BUT I am 
;; not allowed to return that value not allowed to (prn @rslt)
;; both of these destroy set rslt to #{}!!!  

(defn cart [a b]
  (def  rslt (atom #{}))
  (for [j a k b] 
    (swap! rslt conj [j k]) )
    ;; (prn @rslt) -> this resets @rslt to #{} !!!
  ) 

;;user> (cart [1 2 3] [4 5])

;;(#{[1 4]} #{[1 4] [1 5]} #{[2 4] [1 4] [1 5]} #{[2 4] [2 5] [1 4] [1 5]} #{[3 4] [2 4] [2 5] [1 4] [1 5]} #{[3 4] [2 4] [3 5] [2 5] [1 4] [1 5]})

;;user> @rslt
;;#{[3 4] [2 4] [3 5] [2 5] [1 4] [1 5]}

;; END -------------------    #1

;; -------------------    #2

;;  here is the proof of the working code for repl
(def m {:a [1 2]})
(conj m {:a (vec (conj (:a m) 9))})


;; here is that exact logic in a function and it fails with
;; a bogus error:  "Boolean cannot be cast to fn"
((fn [f seq]
   (loop [s seq  el (first s) rslt {}]
     (if (empty? s)
       rslt
       (recur (rest s) (first (rest s))
           (conj rslt {(f el) (vec (conj ((f el) rslt) el))})
                    )))) #(> % 5) [1 3 6 8]  )

;; ----  the code below works as the code above should work!
((fn [f seq]
   (loop [s seq  el (first s) rslt {}]
     (if (empty? s)
       rslt
       (recur (rest s) (first (rest s))
           (conj rslt {(f el) (vec (conj (get rslt (f el)) el))})
                    )))) #(> % 5) [1 3 6 8]  )

;; "(conj ((f el) rslt) el)" fails but "(conj (get rslt (f el)) el)"
;; but they *should be* identical

;; END -------------------    #2
