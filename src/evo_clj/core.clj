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

;; TODO CWJ split out namespace for evaluator stuff?
(defn- givens [criteria]
  (first criteria))

(defn- expected-result [criteria]
  (second criteria))

(defn- value-from-criteria [criteria form]
  (let [givens-map (givens criteria)
        this-value (givens-map form)]
    (or this-value form)))

(defn map-to-bindings [m]
  (vec (mapcat identity (first m))))

(defn internal-context-eval [context expression]
  (let [form `(let ~(map-to-bindings context) ~expression)]
  (eval form)))

(def context-eval (memoize internal-context-eval))

(defn- evaluate-given [expression criteria]
  (- (context-eval criteria expression)
     (expected-result criteria)))

(defn- index-coll [coll]
  (map (fn [idx c] [idx c]) (iterate inc 0) coll))

(defn- strip-index [coll]
  (map (fn [[idx c]] c) coll))

(defn- extract-evaluation [document]
  (apply hash-map (strip-index (filter #(odd? (first %)) (index-coll document)))))

(defn- evaluation-fn [expression]
  (fn [bindings] (evaluate-given expression bindings)))

(defn- eval-and-sum-error [expression criterias]
  (apply + (map (evaluation-fn expression) criterias)))

(defn defevaluator [all-criteria]
  (let [criterias (extract-evaluation all-criteria)]
    (fn [expression] (eval-and-sum-error expression criterias))))

(defn defevolver [grammar evaluator options])

(defn solve [evolver])
