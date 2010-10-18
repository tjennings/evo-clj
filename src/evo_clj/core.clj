(ns evo-clj.core
  (:require [clojure.walk :as walk]))

(defn defgrammar [args] args)

(defn- terminals [grammar]
  (concat (:constants grammar)
          (:variables grammar)))

(defn- eval-if-fn [terminal]
  (if (fn? terminal)
    (terminal)
    terminal))

(defn random-terminal [grammar]
  (let [terminals (terminals grammar)]
    (eval-if-fn (nth terminals (rand-int (count terminals))))))

(defn random-function [grammar]
  (let [functions (:functions grammar)]
    (nth functions (rand-int (count functions)))))

(defn rand-const [] (fn [] (rand)))

(defn- givens [criteria]
  (first criteria))

(defn- result [criteria]
  (second criteria))

(defn- evaluate-given [expression criteria]
  (- (eval (walk/postwalk
          (fn [form]
              (if (= form (first (keys (givens criteria))))
                  (first (vals (givens criteria)))
                  form))
          expression))
     (result criteria)))

(defn defevaluator [all-criteria]
  (let [vars-vals (keys all-criteria)
        variables (flatten (map keys vars-vals))]
    (fn [expression]
      (apply +
        (map
          #(evaluate-given expression %)
          all-criteria)))))

