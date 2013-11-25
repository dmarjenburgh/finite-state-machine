(ns finite-state-machine.examples-test
  (:require [midje.sweet :refer :all]
            [finite-state-machine.examples :refer :all]))

(defn- make-neighbours [[r11 r12 r13 r21 r22 r23 r31 r32 r33]]
  [[r11 r12 r13]
   [r21 r22 r23]
   [r31 r32 r33]])

(facts "about `game-of-life-cell-transition`"
       (fact "Any live cell with fewer than 2 live neighbours dies"
             (game-of-life-cell-transition
               [[nil nil nil]
                [nil 1 nil]
                [nil nil nil]]) => falsey
             (game-of-life-cell-transition
               [[nil nil nil]
                [1 1 nil]
                [nil nil nil]]) => falsey)
       (fact "Any live cell with two or three live neighbours lives on
             to the next generation"
             (game-of-life-cell-transition
               [[nil nil nil]
                [1 1 nil]
                [nil 1 nil]]) => truthy
             (game-of-life-cell-transition
               [[nil 1 nil]
                [1 1 nil]
                [nil 1 nil]]) => truthy)
       (fact "Any live cell with more than three live neighbours dies"
             (game-of-life-cell-transition
               [[nil 1 1]
                [1 1 nil]
                [nil 1 nil]]) => falsey
             (game-of-life-cell-transition
               [[1 1 1]
                [1 1 nil]
                [nil 1 1]]) => falsey)
       (fact "Any dead cell with exactly 3 live neighbours becomes a live cell"
             (game-of-life-cell-transition
               [[1 1 1]
                [nil nil nil]
                [nil nil nil]]) => truthy
             (game-of-life-cell-transition
               [[1 nil 1]
                [nil nil nil]
                [nil nil 1]]) => truthy)
       (fact "Dead cells not with 3 neighbours stay dead"
             (game-of-life-cell-transition
               [[1 1 1]
                [nil nil nil]
                [nil 1 nil]]) => falsey
             (game-of-life-cell-transition
               [[nil nil nil]
                [nil nil nil]
                [nil nil nil]]) => falsey
             (game-of-life-cell-transition
               [[nil 1 1]
                [nil nil nil]
                [nil nil nil]]) => falsey
             (game-of-life-cell-transition
               [[nil 1 1]
                [nil nil nil]
                [1 nil 1]]) => falsey
             (game-of-life-cell-transition
               [[1 1 1]
                [1 nil 1]
                [1 1 1]]) => falsey))

(facts "about `cell-neighbours`"
       (fact "Cells of the grid are nil"
             (let [configuration [[1 nil nil]
                                  [nil 1 1]
                                  [1 nil 1]]]
               (cell-neighbours 0 0 configuration) => [[nil nil nil]
                                                       [nil 1 nil]
                                                       [nil nil 1]]
               (cell-neighbours 1 1 configuration) => configuration)
             )
       )
