(ns day7-clojure.core
  (:gen-class)
  (:require [clojure.string :as str]))

#_{:clj-kondo/ignore [:refer-all]}
(require '[clojure.repl :refer :all])

(defn init []
  (dotimes [_ 50]
    (println)))


(def HAND-TO-PTS {:high_card 0
                  :pair 1
                  :two_pair 2
                  :three_of_a_kind 3
                  :full_house 4
                  :four_of_a_kind 5
                  :five_of_a_kind 6})

(def CARD-RANK {:2 0
                :3 1
                :4 2
                :5 3
                :6 4
                :7 5
                :8 6
                :9 7
                :T 8
                :J 9
                :Q 10
                :K 11
                :A 12})

(defn get-highest-freq
  "Given {:3 2, :2 1, :T 1, :K 1}, return `2`"
  [hand-map]
  (apply max (vals hand-map)))

(defn does-freq-appear-once?
  [hand-map freq]
  (= 1 (count (filter #(= freq %) (vals hand-map)))))

;; Five of a kind, where all five cards have the same label: AAAAA
(defn is-five-of-a-kind?
  [hand-map]
  (= [5] (vals hand-map)))

(defn is-four-of-a-kind?
  "Four of a kind, where four cards have the same label
   and one card has a different label: AA8AA"
  [hand-map]
  (= [1 4] (sort (vals hand-map))))

(defn is-full-house?
  "Full house, where three cards have the same label,
   and the remaining two cards share a different label:
   23332"
  [hand-map]
  (= [2 3] (sort (vals hand-map))))


(defn is-three-of-a-kind?
  "Three of a kind, where three cards have the same label,
   and the remaining two cards are each different from any
   other card in the hand: TTT98"
  [hand-map]
  (let [highest-freq (get-highest-freq hand-map)
        appears-once? (does-freq-appear-once? hand-map highest-freq)]
    (and (= 3 highest-freq)
         appears-once?)))

(defn is-two-pair?
  "Two pair, where two cards share one label,
   two other cards share a second label, and
   the remaining card has a third label: 23432"
  [hand-map]
  (= [1 2 2] (sort (vals hand-map))))

(defn is-pair?
  "One pair, where two cards share one label,
   and the other three cards have a different
   label from the pair and each other: A23A4"
  [hand-map]
  (let [highest-freq (get-highest-freq hand-map)
        appears-once? (does-freq-appear-once? hand-map highest-freq)]
    (and (= 2 highest-freq)
         appears-once?)))


(defn is-high-card?
  "High card, where all cards' labels are distinct: 23456"
  [hand-map]
  (= [1 1 1 1 1] (vals hand-map)))

(defn update-card-map
  "Given a map of cards to counts (e.g. {:A 1 :3 2}),
   add new-card (example `A`) to the map"
  [cards new-card]
  (update cards
          (keyword new-card)
          (fn [card] (if (nil? card) 1 (inc card)))))

(defn get-hand-map
  "Given a hand str (e.g. `A23A4`), return a map 
   of card type to count"
  [hand-str]
  (let [cards (str/split hand-str #"")]
    (reduce update-card-map {} cards)))


(defn parse-hand
  "Given a string (for example, `32T3K 765`), parse out the hand and the corresponding bid and return it in a map"
  [hand-map]
  (cond
    (= true (is-pair? hand-map)) "pair"
    (= true (is-two-pair? hand-map)) "two_pair"
    (= true (is-three-of-a-kind? hand-map)) "three_of_a_kind"
    (= true (is-full-house? hand-map)) "full_house"
    (= true (is-four-of-a-kind? hand-map)) "four_of_a_kind"
    (= true (is-five-of-a-kind? hand-map)) "five_of_a_kind"
    :else "high_card"
    ;;
    ))

(defn parse-line
  [str]
  (let [tokens (str/split str #" ")
        hand (get tokens 0)
        hand-map (get-hand-map hand)
        bid (Integer/parseInt (get tokens 1))]
    {:hand-map hand-map :bid bid :str hand}))

(defn better-card-comparator
  [hand-1-str hand-2-str]
  (let [hand-1-vec (str/split hand-1-str #"")
        hand-2-vec (str/split hand-2-str #"")]
    (loop [vec-1 hand-1-vec
           vec-2 hand-2-vec]
      (if (or
           (= (count vec-1) 0)
           (= (count vec-2) 0))
        0
        (let [letter-1 (first vec-1)
              letter-2 (first vec-2)
              keyword-1 (keyword (str letter-1))
              keyword-2 (keyword (str letter-2))
              card-rank-1 (keyword-1 CARD-RANK)
              card-rank-2 (keyword-2 CARD-RANK)]
          (cond
            (< card-rank-1 card-rank-2) -1
            (> card-rank-1 card-rank-2) 1
            (= card-rank-1 card-rank-2) (recur (rest vec-1) (rest vec-2))))))))



(defn hand-comparator
  "Given two different hands return -1 if the first hand is higher,
  and 1 if the second hand is higher, and 0 if they are the same."
  [hand-bid-1 hand-bid-2]
  (let [hand-1-type (parse-hand (:hand-map hand-bid-1))
        hand-2-type (parse-hand (:hand-map hand-bid-2))
        hand-1-key (keyword hand-1-type)
        hand-2-key (keyword hand-2-type)
        hand-1-pts (hand-1-key HAND-TO-PTS)
        hand-2-pts (hand-2-key HAND-TO-PTS)]
    (cond
      (< hand-1-pts hand-2-pts) -1
      (> hand-1-pts hand-2-pts) 1
      (= hand-1-pts hand-2-pts) (better-card-comparator
                                 (:str hand-bid-1)
                                 (:str hand-bid-2)))))


(defn get-input
  [path]
  (str/split-lines (slurp path)))


;; (def INPUT-VEC (get-input "./input/short.txt"))
;; (def INPUT-VEC (get-input "./input/temp.txt"))
(def INPUT-VEC (get-input "./input/long.txt"))

(defn print-hand-bid
  [hand-bid]
  (prn (:hand-map hand-bid))
  (prn (:bid hand-bid))
  (prn (:str hand-bid)))

(defn rank-bid-reducer
  [total rank-bid]
  (let [rank-amt (:rank rank-bid)
        bid-amt (:bid rank-bid)]
    (+ total (* rank-amt
                bid-amt))))

(defn task
  []
  (let [all-hand-bids (into [] (map parse-line INPUT-VEC))
        all-hand-bids-sorted (sort hand-comparator all-hand-bids)
        rank-and-bids-list (map-indexed
                            (fn [idx hand-bid] {:rank (+ 1 idx)
                                                :bid (:bid hand-bid)}) all-hand-bids-sorted)
        final-score (reduce rank-bid-reducer 0 rank-and-bids-list)
        ;;
        ]
    (println "Final Score: " final-score)))








#_{:clj-kondo/ignore [:unused-binding]}
(defn -main
  [& args]
  (init)
  (task)
  (println "\n✅ Done!"))
(-main)
