(defproject finite-state-machine "0.1.0-SNAPSHOT"
  :description "A library for creating general finite state machines"
  :license {:name "WTFPL"
            :url "http://www.wtfpl.net"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [seesaw "1.4.4"]]
  :main finite-state-machine.glider-gun
  :profiles {:dev {:dependencies [[midje "1.6-beta1"]]}})
