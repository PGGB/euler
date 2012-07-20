(ns euler.core)

(defn int-to-digits [n]
  (for [character (str n)]
    (Integer/parseInt (str character))))

(defn palindrome? [n]
  (if (= (int-to-digits n) (reverse (int-to-digits n)))
    true
    false))

(defn divisible? [n d]
  (zero? (rem n d)))

(defn fib 
  ([] (lazy-seq (cons 1 (fib 2 3))))
  ([a b] (lazy-seq (cons a (fib b (+ a b))))))

(defn triangle-numbers 
  ([] (lazy-seq (cons 1 (triangle-numbers 3 2))))
  ([a b] (lazy-seq (cons a (triangle-numbers (+ a b 1) (inc b))))))

(defn lazy-primes []
  (letfn [(append-under-key [h key val]
         (assoc h key (conj (h key) val)))
      (run-iterators [h n]
         (dissoc (reduce #(append-under-key %1 (+ n %2) %2) h (h n)) n))
      (next-prime [h n]
         (if (h n) ;; If h has any items, it is not prime
           (recur (run-iterators h n) (+ n 2))
           (cons n (lazy-seq
            (next-prime (append-under-key h (* n n) (+ n n)) (+ n 2))))))]
    (cons 2 (lazy-seq (next-prime {} 3)))))

(defn prime?
  "Tests whether a given number is prime."
  [n]
  (cond
    (or (= n 2) (= n 3))          true
    (or (divisible? n 2) (< n 2)) false
    :else                         (let [sqrt-n (Math/sqrt n)]
                                    (loop [i 3]
                                      (cond
                                        (divisible? n i) false
                                        (< sqrt-n i)     true
                                        :else            (recur (+ i 2)))))))

(defn euler001 []
   (reduce + 
           (filter #(or (divisible? % 3) (divisible? % 5)) (range 1 1000))))

(euler001)

(defn euler002 []
  (reduce + (filter even? (take-while #(< % 4000000) (fib 1 2)))))

(euler002)

(defn euler003 []
  (loop [x 2]
    (if (and (divisible? 600851475143 x) (prime? (/ 600851475143 x)))
      (/ 600851475143 x) 
      (recur (inc x)))))

(euler003) 

(defn euler004 []
  (last 
    (sort 
      (filter palindrome? 
              (for [x (range 1 1000) y (range 1 1000)] (* x y)))))) 

(euler004)

(defn euler005 []
  (loop [x 20]
    (if (every? true? (map #(divisible? x %) (range 1 20)))
    x 
    (recur (+ x 20)))))

(euler005)

(defn sumofsquares [n]
  (reduce + (map #(* % %) (range 1 (+ n 1)))))

(defn squareofsums [n]
  (let [x (reduce + (range 1 (+ n 1)))]
    (* x x)))

(defn euler006 []
  (-  (squareofsums 100) (sumofsquares 100)))

(euler006)

(defn euler007 []
  (nth (lazy-primes) 10000))

(euler007)

(def number008 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450)

(defn euler008 []
  (reduce max (for [x (range 0 996)
                    :let [y (reduce * (take 5(drop x (int-to-digits number008))))]
                    :when (> y 0)]
                y)))

(euler008)

(defn euler009 []
                 (for [c (range 1 1000)
                       b (range 1 c)
                       a (range 1 b)
                       :when (and (= (+ a b c) 1000) (= (+ (* a a) (* b b)) (* c c)))]
                   (* a b c)))

(euler009)

(defn euler010 []
  (reduce + (take-while #(< % 2000000) (lazy-primes))))

(euler010)

(def grid011 
  [[ 8  2 22 97 38 15  0 40  0 75  4  5  7 78 52 12 50 77 91  8] 
   [49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48  4 56 62  0] 
   [81 49 31 73 55 79 14 29 93 71 40 67 53 88 30  3 49 13 36 65] 
   [52 70 95 23  4 60 11 42 69 24 68 56  1 32 56 71 37  2 36 91] 
   [22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80] 
   [24 47 32 60 99  3 45  2 44 75 33 53 78 36 84 20 35 17 12 50] 
   [32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70] 
   [67 26 20 68  2 62 12 20 95 63 94 39 63  8 40 91 66 49 94 21] 
   [24 55 58  5 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72] 
   [21 36 23  9 75  0 76 44 20 45 35 14  0 61 33 97 34 31 33 95] 
   [78 17 53 28 22 75 31 67 15 94  3 80  4 62 16 14  9 53 56 92] 
   [16 39  5 42 96 35 31 47 55 58 88 24  0 17 54 24 36 29 85 57] 
   [86 56  0 48 35 71 89  7  5 44 44 37 44 60 21 58 51 54 17 58] 
   [19 80 81 68  5 94 47 69 28 73 92 13 86 52 17 77  4 89 55 40] 
   [04 52  8 83 97 35 99 16  7 97 57 32 16 26 26 79 33 27 98 66] 
   [88 36 68 87 57 62 20 72  3 46 33 67 46 55 12 32 63 93 53 69] 
   [04 42 16 73 38 25 39 11 24 94 72 18  8 46 29 32 40 62 76 36] 
   [20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74  4 36 16] 
   [20 73 35 29 78 31 90  1 74 31 49 71 48 86 81 16 23 57  5 54] 
   [01 70 54 71 83 51 54 69 16 92 33 48 61 43 52  1 89 19 67 48]])

(defn euler011 []
  (reduce max (map #(apply max %) (for [x (range 20)
        y (range 17)]
    (cond
      (< x 3) [(* (get-in grid011 [x y]) (get-in grid011 [(+ x 1) y]) (get-in grid011 [(+ x 2) y]) (get-in grid011 [(+ x 3) y]))
               (* (get-in grid011 [x y]) (get-in grid011 [x (+ y 1)]) (get-in grid011 [x (+ y 2)]) (get-in grid011 [x (+ y 3)]))
               (* (get-in grid011 [x y]) (get-in grid011 [(+ x 1) (+ y 1)]) (get-in grid011 [(+ x 2) (+ y 2)]) (get-in grid011 [(+ x 3) (+ y 3)]))]
      (> x 16) [(* (get-in grid011 [x y]) (get-in grid011 [x (+ y 1)]) (get-in grid011 [x (+ y 2)]) (get-in grid011 [x (+ y 3)]))
                (* (get-in grid011 [x y]) (get-in grid011 [(- x 1) (+ y 1)]) (get-in grid011 [(- x 2) (+ y 2)]) (get-in grid011 [(- x 3) (+ y 3)]))]
      :else [(* (get-in grid011 [x y]) (get-in grid011 [(+ x 1) y]) (get-in grid011 [(+ x 2) y]) (get-in grid011 [(+ x 3) y]))
             (* (get-in grid011 [x y]) (get-in grid011 [x (+ y 1)]) (get-in grid011 [x (+ y 2)]) (get-in grid011 [x (+ y 3)]))
             (* (get-in grid011 [x y]) (get-in grid011 [(+ x 1) (+ y 1)]) (get-in grid011 [(+ x 2) (+ y 2)]) (get-in grid011 [(+ x 3) (+ y 3)]))
             (* (get-in grid011 [x y]) (get-in grid011 [(- x 1) (+ y 1)]) (get-in grid011 [(- x 2) (+ y 2)]) (get-in grid011 [(- x 3) (+ y 3)]))])))))

(euler011)

(defn euler012 []
  (first (filter (fn [n] (< 5 (count (filter #(divisible? n %) (range 1 (inc n)))))) (triangle-numbers))))

(euler012)









