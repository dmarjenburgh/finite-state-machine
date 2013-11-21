(ns finite-state-machine.examples
  (:require [finite-state-machine.core :refer :all]
            [seesaw.core :refer :all]))

;;;;;;;;;;;;;;;;;
;; Simple finite state random-walk
;;;;;;;;;;;;;;;;;
;;
;; -n <-> ... <-> -1 <-> 0 <-> 1 <-> 2 <-> ... <-> n

(def random-walk (finite-state-machine 0
                   (fn [current _]
                     (if (zero? (rand-int 2))
                       (dec current)
                       (inc current)))))

;; Lazy-seq of the visited states after 100 steps
#_(map :state (reductions transition random-walk (range 100)))

;;;;;;;;;;;;;;;;;
;; 4 digit code lock
;;;;;;;;;;;;;;;;;
;;
;; states are _ _ _ _, d _ _ _, d d _ _, d d d _, d d d d and :open
;; where d is any single digit
;; events are :enter-nr when the code has not been entered fully yet
;; and :push-button when all 4 numbers are entered

(defn apply-if [pred f x & args]
  (if (pred x) (apply f x args) x))

(letfn [(enter-nr [current-state nr]
          (apply-if #(< (count %) 4) conj current-state nr))
        (open [current-state]
          (if (< (count current-state) 4) current-state
            (if (= current-state [1 2 3 4]) :open [])))
        (tr-fn [current-state [ev-name nr :as event]]
          (case ev-name
            :enter-nr (enter-nr current-state nr)
            :push-button (open current-state)
            (throw (IllegalArgumentException. "Event name must be :enter-nr or :push-button"))))]
  (def code-lock (finite-state-machine [] tr-fn)))

#_(map :state (reductions transition code-lock [[:enter-nr 5]
                                                [:enter-nr 2]
                                                [:enter-nr 7]
                                                [:push-button]
                                                [:enter-nr 4]
                                                [:enter-nr 9]
                                                [:push-button]
                                                [:enter-nr 1]
                                                [:enter-nr 2]
                                                [:enter-nr 3]
                                                [:enter-nr 4]
                                                [:push-button]]))

;;;;;;;;;;;;;;;;;
;; Markov chain
;;;;;;;;;;;;;;;;;

(def transition-matrix [[0.4 0.4 0.2]
                        [0.2 0.3 0.5]
                        [0.4 0.3 0.3]])

(def population-vec [30 50 20])

(defn inner-product [v1 v2]
  (reduce + (map * v1 v2)))

(defn apply-matrix [m v]
  (mapv (partial inner-product v) m))

(letfn [(tr-fn [current-state _]
          (apply-matrix transition-matrix current-state))]
  (def markov-process (finite-state-machine population-vec tr-fn)))

;; You can see it approach the equilibrium state [1/3 1/3 1/3]
#_ (:state (reduce transition markov-process (range 100)))

;;;;;;;;;;;;;;;;;
;; Cellular automaton
;;;;;;;;;;;;;;;;;
;; Glider gun

(defn- make-grid [rows cols]
  (vec (repeat rows (vec (repeat cols false)))))

(def grid (make-grid 36 9))

#_(def configuration)


;;;;;;;;;;;;;;;;;
;; Transitions with side effects
;;;;;;;;;;;;;;;;;

(def vcr-state (atom :off))

(defn handle-power-btn! [current-state]
  (case current-state
    :off (do
           (println "Powering up")
           (reset! vcr-state :on))
    (do
      (println "Powering down")
      (reset! vcr-state :off))))

(defn handle-play-btn! [current-state]
  (case current-state
    :off (do
           (println "Can't play when power is down")
           current-state)
    :on (do
          (println "Starting video play")
          (reset! vcr-state :playing))
    :playing (do
               (println "Already playing")
               current-state)
    :paused (do
              (println "Resuming play")
              (reset! vcr-state :playing))
    (throw (IllegalArgumentException. "Invalid state"))))

(defn handle-pause-btn! [current-state]
  (case current-state
    :off (do
           (println "Can't pause when power is down")
           current-state)
    :on (do
          (println "Pause what?")
          current-state)
    :playing (do
               (println "Pausing play")
               (reset! vcr-state :paused))
    :paused (do
              (println "Already paused")
              current-state)
    (throw (IllegalArgumentException. "Invalid state"))))

(def vcr-transition-map
  {:power-btn handle-power-btn!
   :play-btn handle-play-btn!
   :pause-btn handle-pause-btn!})


(letfn [(tr-fn [current-state event]
            ((partial (event vcr-transition-map)) @current-state))]
  (def vcr (finite-state-machine vcr-state tr-fn)))

#_(doseq [action [:power-btn :power-btn :play-btn :power-btn :play-btn :play-btn :pause-btn :pause-btn :play-btn :power-btn]]
    (transition vcr action))
#_(deref (:state vcr))

