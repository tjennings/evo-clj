(ns evo-clj.core)

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
