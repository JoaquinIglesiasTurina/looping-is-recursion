(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp storage]
                 (if (zero? exp)
                   storage
                   (recur base (dec exp) (* base storage))))]
    (helper base exp 1)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (println seq1)
  (println seq2)
  (cond (and (empty? seq1) (empty? seq2)) true
        (or (empty? seq1) (empty? seq2)) false
        (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
        :else false))

(defn find-first-index [pred a-seq]
  (loop [k 0
         pred pred
         a-seq a-seq]
    (println a-seq)
    (cond (empty? a-seq) nil
          (pred (first a-seq)) k
          :else
          (recur (inc k) pred (rest a-seq)))))

(defn avg [a-seq]
  (loop [k 0
         csum 0
         a-seq a-seq]
      (if (empty? a-seq)
        (/ csum k)
        (recur (inc k) (+ csum (first a-seq)) (rest a-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))
(defn parity [a-seq]
  (loop [a-set #{}
         a-seq a-seq]
    (if (empty? a-seq)
      a-set
      (recur (toggle a-set (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (loop [fk-1 1
         fk-2 0
         k 2
         n n]
    (cond (< n 2) n
          (= k n) (+ fk-1 fk-2)
          :else (recur (+ fk-1 fk-2) fk-1 (inc k) n))))

(defn cut-at-repetition [a-seq]
  (loop [a-set #{}
         end-seq []
         a-seq a-seq]
    (let [new-seq (conj end-seq (first a-seq))
          new-set (set new-seq)]
      (if (or (empty? a-seq) (= a-set new-set))
        end-seq
        (recur new-set new-seq (rest a-seq))))))

