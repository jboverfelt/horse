(ns horse.core
  (:require [clojure.string :as s :refer [split join]]
            [clojure.java.io :refer [resource]]))

(def words
  (-> (resource "tweets-text")
      (slurp)
      (s/replace "\n" ":end")
      (split #"\s")))

(defn transform [words]
  (apply merge-with
         #(merge-with + %1 %2)
         (map (fn [w nw] {w {nw 1}})
              words
              (next words))))

(defn markov-chain [xformed]
  (let [start (rand-nth (keys xformed))]
    (loop [curr start
           acc [start]]
      (if-not (and (xformed curr) (not (.contains curr ":end")))
        (first (split (join " " acc) #":end"))
        (let [next-word (rand-nth (keys (xformed curr)))]
          (recur next-word (conj acc next-word)))))))

(markov-chain (transform words))
