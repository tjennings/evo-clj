(ns evo-clj.test.core
  (:use [evo-clj.core])
  (:use [lazytest.describe :only (describe it)]))

(defn unique [coll]
  (= (sort (seq (set coll))) (sort coll)))

(defn include-all [all coll]
  (let [all  (set all)
        coll (set coll)]
    (= (clojure.set/intersection all coll))))

(describe "the grammar"
  (it "can contain functions"
    (let [grammar (defgrammar {:functions [+ - /]})]
      (= + (first (:functions grammar)))))
  (it "can contain variables"
    (let [grammar (defgrammar {:variables '[x y]})]
      (= 'x (first (:variables grammar)))))
  (it "can contain constants"
    (let [grammar (defgrammar {:constants [3.14]})]
      (= 3.14 (first (:constants grammar)))))
  (it "pulls out a random terminal"
    (let [grammar (defgrammar {:variables '[x y]})]
      (some #{(random-terminal grammar)} '[x y])))
  (it "pulls out a random terminal - of constants"
    (let [grammar (defgrammar {:constants [3.14]})]
      (some #{(random-terminal grammar)} [3.14])))
  (it "pulls out a random function"
    (let [grammar (defgrammar {:functions [- * /]})]
      (some #{(random-function grammar)} [- * /])))
  (it "pulls out a random constant"
    (let [grammar (defgrammar {:constants [(rand-const)]})]
      (unique (map (fn [_] (random-terminal grammar)) (range 0 10000)))))
  (it "allows mixed static constants and random constants"
    (let [grammar (defgrammar {:constants [3.14 10 (rand-const)]})
          random-constants (map (fn [_] (random-terminal grammar)) (range 0 10000))]
      (include-all #{3.14 10} random-constants))))

(describe "the evaluator"
  (it "returns an error of 0 for a perfect solution"
    (let [evaluate (defevaluator {{'X 0} 0})]
      (= 0 (evaluate '(do X)))
      (= 0 (evaluate '(* X X)))
      (= 0 (evaluate '(+ X X)))))
  (it "returns the actual error amount for imperfect solutions to a 1-sample problem"
    (let [evaluate (defevaluator {{'X 0} 0})]
      (= 1 (evaluate '(+ X 1)))
      (= 4 (evaluate '(* (+ X 2) (+ X 2))))))
  (it "returns the correct error amount where y = 1"
    (let [evaluate (defevaluator {{'y 1} 2})]
      (= 0 (evaluate '(+ y 1)))
      (= 0 (evaluate '(* (+ y 1) y)))))
  (it "returns the correct error amount with multiple variables"
    (let [evaluate (defevaluator {{'x 2 'y 3} 6})]
      (= 0 (evaluate '(* x 3)))
      (= 0 (evaluate '(* y 2)))
      (= 0 (evaluate '(+ x y 1)))))
  (it "returns the correct error amount with multiple variables and criteria"
    (let [evaluate (defevaluator {{'x 2 'y 3} 6
                                  {'x 4 'y 6} 11})]
      (= 1 (evaluate '(* x 3)))
      (= 1 (evaluate '(* y 2)))
      (= 0 (evaluate '(+ x y 1))))))

