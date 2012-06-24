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

(range 10 1 -1)

(defn euler004 []
  (last 
    (sort 
      (filter palindrome? 
              (for [x (range 1 1000) y (range 1 1000)] (* x y)))))) 

(euler004)

(defn euler005 []
  (loop [x 1]
    (if (every? true? (map (fn [n] divisible? x n) (range 1 10)))
    x 
    (recur (+ x 20)))))

(euler005)
