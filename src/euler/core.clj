(ns euler.core)

(defn fib [a b]
  (lazy-seq (cons a (fib b (+ a b)))))

(defn euler001 []
   (reduce + 
           (filter #(or (zero? (mod % 3)) (zero? (mod % 5))) (range 1 1000))))

(euler001)

(fib)

(take-while #(< % 4000000) (fib 1 2))

(defn euler 002 []
  (reduce + (filter even? (take-while #(< % 4000000) (fib 1 2)))))

