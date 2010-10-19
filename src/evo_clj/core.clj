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

;; TODO CWJ split out namespace for evaluator stuff?
(defn- givens [criteria]
  (first criteria))

(defn- result [criteria]
  (second criteria))

(defn- value-from-criteria [criteria form]
  (let [givens-map (givens criteria)
        this-value (givens-map form)]
    (or this-value form)))

(defn- evaluate-given [expression criteria]
  (- (eval
       (walk/postwalk
         (partial value-from-criteria criteria)
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

