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

(defn fib [a b]
  (lazy-seq (cons a (fib b (+ a b)))))

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

