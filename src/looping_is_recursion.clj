(ns looping-is-recursion)

(defn singleton? [coll]
  (if (empty? coll)
    false
    (if (empty? (rest coll))
      true
      false)))

(defn power [base exp]
  (let [power-helper (fn [acc base exp]
              (if (= exp 0)
                acc
                (recur (* acc base) base (dec exp))))]
    (power-helper 1 base exp)))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (recur (rest a-seq))))


(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1)
         (empty? seq2)) true
    (or (empty? seq1)
        (empty? seq2)) false
    (not (= (first seq1)
            (first seq2))) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         a-seq a-seq]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) index
      :else (recur (inc index) (rest a-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         size 0
         a-seq a-seq]
    (cond
      (empty? a-seq) nil
      (singleton? a-seq) (/ (+ sum (first a-seq)) (inc size))
      :else (recur (+ sum (first a-seq)) (inc size) (rest a-seq)))))

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
  (loop [f0 0
         f1 1
         n n]
    (cond
      (= n 0) f0
      (= n 1) f1
      :else (recur f1 (+ f1 f0) (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [seen #{}
         return []
         a-seq a-seq]
    (cond
      (empty? a-seq) return
      (contains? seen (first a-seq)) return
      :else (recur (conj seen (first a-seq))
                   (conj return (first a-seq))
                   (rest a-seq)))))


