(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [res base exp]
                 (if (zero? exp)
                   res
                   (recur (* res base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [ans a-seq]
                 (if (empty? a-seq)
                 ans
                 (recur (first a-seq) (rest a-seq))))]
    (helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond
                      (and (empty? seq1) (empty? seq2)) true
                      (or (empty? seq1) (empty? seq2)) false
                      (= (first seq1) (first seq2))
                      (recur (rest seq1) (rest seq2))
                      :else false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [n 0
         kek a-seq]
    (if (empty? kek)
        nil
        (if (pred (first kek))
            n
            (recur (inc n) (rest kek))))))

(defn avg [a-seq]
  (loop [n 0
         kek a-seq
         sum 0]
    (if (empty? kek)
      (/ sum n)
      (recur (inc n) (rest kek) (+ sum (first kek))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [kek a-seq
         kak (set [])]
    (if (empty? kek)
      kak
      (recur (rest kek) (toggle kak (first kek))))))

(defn fib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1))(fib (- n 2)))))

(defn fast-fibo [n]
  (loop [x 0
         y 1
         z n]
    (if (= z 0)
      x
      (recur y (+ x y) (dec z)))))

(defn cut-at-repetition [a-seq]
  (loop [x #{}
         p []
         joku a-seq]
    (if (empty? joku)
      p
      (if (contains? x (first joku))
        p
        (recur (conj x (first joku)) (conj p (first joku)) (rest joku))))))
