(ns finite-state-machine.core)

(defprotocol IFSM
  (init [this state] "Returns the state machine in the given state")
  (next-state [this transition-fn] "Register the transition function: (current-state, event) -> new-state. If the transitions are conditional or if there are side effects, they should be put here.")
  (transition [this event] "Evaluates (transition-fn current-state event) and sets the new state to the return value."))

(defrecord FSM [state transition-fn]
  IFSM
  (init [this new-state] (assoc this :state new-state))
  (next-state [this trans-fn] (assoc this :transition-fn trans-fn))
  (transition [this event] (assoc this :state (transition-fn state event))))

(defn finite-state-machine
  "Creates a new instance of FSM."
  ([] (finite-state-machine nil (constantly nil)))
  ([initial-state transition-fn]
   (->FSM initial-state transition-fn)))
