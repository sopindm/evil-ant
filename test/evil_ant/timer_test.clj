(ns evil-ant.timer-test
  (:require [khazad-dum :refer :all]
            [evil-ant :as e]
            [evil-ant-test :refer :all])
  (:import [evil_ant ClosedEmitterException]))

(deftest zero-timing
  (let [e (e/timer 0)]
    (?emit-now= e [e])))

(deftest waiting-for-timer
  (let [e (e/timer 5)]
    (?emit-now= e nil)
    (?emit-in= e 2 nil)
    (?emit-in= e 4 [e])))

(deftest timer-emit-doesnt-block-forever
  (let [e (e/timer 3)]
    (?emit= e [e]))
  (let [e (e/timer 0)]
    (?emit= e [e])))

(deftest timer-stop-stops-emit
  (let [e (e/timer 1000)
        f (future (?emit= e nil))]
    (Thread/sleep 3)
    (?false (realized? f))
    (e/stop! e)
    (Thread/sleep 1)
    (?true (realized? f))))

(deftest default-timer-dont-start-second-time
  (let [e (e/timer 0)]
    (?emit= e [e])
    (?emit= e nil)))

(deftest timer-timeout
  (?= (e/timeout (e/timer 100500)) 100500)
  (let [e (e/timer 100)]
    (Thread/sleep 5)
    (?= (e/timeout e) 100)
    (?true (<= (e/remaining e) 95))))

(deftest starting-timer
  (let [e (e/timer 100)]
    (Thread/sleep 5)
    (?true (and (<= (e/remaining e) 95) (>= (e/remaining e) 5)))
    (e/stop! e)
    (?= (e/remaining e) 0)
    (e/start! e)
    (?true (>= (e/remaining e) 98))))

(deftest restarting-timer
  (let [e (e/timer 100)]
    (Thread/sleep 5)
    (?true (<= (e/remaining e) 95))
    (e/restart! e)
    (?true (>= (e/remaining e) 97))))

(deftest circular-timers
  (let [e (e/timer 0 :circular true)]
    (?emit= e [e])
    (?emit= e [e])))

(deftest oneoff-timers
  (let [e (e/timer 0 :one-off true)]
    (?emit= e [e])
    (?false (e/open? e))))

(deftest timer-sets
  (let [a (atom [])
        timers [(e/timer 0) (e/timer 3) (e/timer 6)]
        s (apply e/timer-set timers)]
    (doseq [t timers] (action-handler a t))
    (e/emit-now! s 123)
    (?actions= a [(first timers) 123])
    (reset! a [])
    (e/emit-in! s 234 1000)
    (?actions= a [(second timers) 234])
    (reset! a [])
    (e/emit-in! s 345 1000)
    (?action= a [(nth timers 2) 345])))

(deftest starting-timer-after-set-emit
  (let [a (atom [])
        timer (e/timer 0)
        s (e/timer-set)
        h (action-handler a timer)
        f (future (e/emit! s 123))]
    (Thread/sleep 1)
    (?false (realized? f))
    (e/conj! s timer)
    (Thread/sleep 1)
    (?true (realized? f))
    (?actions= a [timer 123])))
