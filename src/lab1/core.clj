(ns lab1.core)

(defn moving-variance [window-size data-stream]
  (let [window (atom [])]
    (letfn [(update-window [w x]
              (if (< (count w) window-size)
                (conj w x)
                (conj (rest w) x)))

            (calculate-variance [w]
              (let [n (count w)
                    mean (/ (apply + w) n)
                    squared-diffs (map #(Math/pow (- % mean) 2) w)
                    variance (/ (apply + squared-diffs) n)]
                (Math/sqrt variance)))

            (moving-variance-helper [w data]
              (lazy-seq
                (when-let [x (first data)]
                  (let [updated-window (swap! window #(update-window % x))
                        current-variance (calculate-variance updated-window)]
                    (cons current-variance (moving-variance-helper updated-window (rest data)))))))]

      (moving-variance-helper @window data-stream))))


(defn print-moving-variance [n data-stream]
  (let [last-result (last (take n (moving-variance 3 data-stream)))]
    (println "Дисперсия окна в" n "элементов" last-result)))

(defn print-first-n [n data-stream]
  (prn (take n data-stream)))

(def n 20)                                                  ; размер окна

(println "Дисперсия арифметической последовательности:")
(def infinite-data-stream (range))

(print-first-n n infinite-data-stream)
(print-moving-variance n infinite-data-stream)

(println "Дисперсия случайной последовательности:")

(defn infinite-randoms []
  (repeatedly #(rand-int 100)))

(print-first-n n (take 100 (infinite-randoms)))

(print-moving-variance n (take 100 (infinite-randoms)))
