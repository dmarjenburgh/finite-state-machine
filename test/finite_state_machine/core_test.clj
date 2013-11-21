(ns finite-state-machine.core-test
  (:require [midje.sweet :refer :all]
            [finite-state-machine.core :refer :all]))

(facts "about finite-state-machine"
       (-> (finite-state-machine)
           (init :A)
           (next-state (constantly :B))
           (transition :something)
           :state) => :B)



