(ns p-p-p-pokerface)


(defn rank->number [rank]
  (cond
    (= rank \A) "14"
    (= rank \K) "13"
    (= rank \Q) "12"
    (= rank \J) "11"
    (= rank \T) "10"
    :else rank))


(defn rank [[rank]]
  (Integer/valueOf (str (rank->number rank))))


(defn suit [[_ suit]]
  (str suit))


(defn rank-frequencies [hand]
  (vals (frequencies (map rank hand))))


(defn max-freq [hand]
  (apply max (rank-frequencies hand)))


(defn pair? [hand]
  (= 2 (max-freq hand)))


(defn three-of-a-kind? [hand]
  (= 3 (max-freq hand)))


(defn four-of-a-kind? [hand]
  (= 4 (max-freq hand)))


(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))


(defn full-house? [hand]
  (= [3 2] (vals (frequencies (map rank hand)))))


(defn two-pairs? [hand]
  (let [hand-frequencies (rank-frequencies hand)]
    (or (= [4 1] hand-frequencies)
        (= [2 2 1] hand-frequencies))))


(defn straight? [hand]
  (let [ranks (sort (map rank hand))
        ranks-low-ace (sort (replace {14 1} ranks))
        min-rank (apply min ranks)
        max-rank (apply max ranks)
        max-rank-low-ace (apply max ranks-low-ace)
        possible-high-straight (range min-rank (+ 1 max-rank))
        possible-low-straight (range 1 (+ 1 max-rank-low-ace))]
    (or (= ranks-low-ace possible-low-straight)
        (= ranks possible-high-straight))))


(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))


(defn value [hand]
  (cond (straight-flush? hand) 8
        (four-of-a-kind? hand) 7
        (full-house? hand) 6
        (flush? hand) 5
        (straight? hand) 4
        (three-of-a-kind? hand) 3
        (two-pairs? hand) 2
        (pair? hand) 1
        :else 0))
