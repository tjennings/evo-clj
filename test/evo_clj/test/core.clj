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
      (include-all #{3.14 10} random-constants)))
)

