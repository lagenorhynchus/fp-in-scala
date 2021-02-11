(ns fpinclojure.datastructures.list
  (:refer-clojure :exclude [list])
  (:require
   [clojure.spec.alpha :as s])
  (:import
   (clojure.lang
    ISeq
    Sequential)))

(deftype Cons
  [head tail]
  ISeq
  (first [_] head)
  (next [_] tail)
  (more [_] tail)
  (cons [this o] (Cons. o this))
  (equiv [this o]
    (and (sequential? o)
         (= (first this) (first o))
         (= (next this) (next o))))
  (seq [this] this)
  Sequential)

(s/fdef cons?
  :args (s/cat :x any?)
  :ret boolean?)

(defn cons? [x]
  (and (instance? Cons x)
       (or (nil? (.-tail x))
           (cons? (.-tail x)))))

(s/def ::list
  (s/nilable cons?))

(s/fdef list
  :args (s/cat :args (s/* any?))
  :ret ::list)

(defn list [& args]
  (if (empty? args)
    nil
    (Cons. (first args)
           (apply list (rest args)))))

(s/fdef sum
  :args (s/cat :ns (s/and ::list
                          #(every? integer? %)))
  :ret integer?)

(defn sum [ns]
  (if (empty? ns)
    0
    (+ (first ns)
       (sum (rest ns)))))

(s/fdef product
  :args (s/cat :ns (s/and ::list
                          #(every? integer? %)))
  :ret integer?)

(defn product [ns]
  (cond
    (empty? ns) 1
    (zero? (first ns)) 0
    :else (* (first ns)
             (product (rest ns)))))

(s/fdef append
  :args (s/cat :a1 ::list
               :a2 ::list)
  :ret ::list)

(defn append [a1 a2]
  (if (empty? a1)
    a2
    (Cons. (first a1)
           (append (rest a1) a2))))

(s/fdef fold-right
  :args (s/cat :f ifn?
               :z any?
               :as ::list)
  :ret any?)

(defn fold-right [f z as]
  (if (empty? as)
    z
    (f (first as)
       (fold-right f z (rest as)))))

(s/fdef sum2
  :args (s/cat :ns (s/and ::list
                          #(every? integer? %)))
  :ret integer?)

(defn sum2 [ns]
  (fold-right + 0 ns))

(s/fdef product2
  :args (s/cat :ns (s/and ::list
                          #(every? integer? %)))
  :ret integer?)

(defn product2 [ns]
  (fold-right * 1 ns))

;; Exercise 3.1

;; TODO

;; Exercise 3.2

;; TODO

;; Exercise 3.3

;; TODO

;; Exercise 3.4

;; TODO

;; Exercise 3.5

;; TODO

;; Exercise 3.6

;; TODO

;; Exercise 3.7

;; TODO

;; Exercise 3.8

;; TODO

;; Exercise 3.9

;; TODO

;; Exercise 3.10

;; TODO

;; Exercise 3.11

;; TODO

;; Exercise 3.12

;; TODO

;; Exercise 3.13

;; TODO

;; Exercise 3.14

;; TODO

;; Exercise 3.15

;; TODO

;; Exercise 3.16

;; TODO

;; Exercise 3.17

;; TODO

;; Exercise 3.18

;; TODO

;; Exercise 3.19

;; TODO

;; Exercise 3.20

;; TODO

;; Exercise 3.21

;; TODO

;; Exercise 3.22

;; TODO

;; Exercise 3.23

;; TODO

;; Exercise 3.24

;; TODO

(comment
  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument)

  (sum (list 1 2 4))
  (sum (list 2))
  (sum (list))

  (product (list 1 2 4))
  (product (list 2))
  (product (list))

  (append (list 1 2) (list 3 4 5))
  (append nil (list 3 4 5))
  (append (list 1 2) nil)

  (sum2 (list 1 2 4))
  (sum2 (list 2))
  (sum2 (list))

  (product2 (list 1 2 4))
  (product2 (list 2))
  (product2 (list))
  )
