(ns evil-ant.timer-test
  (:require [khazad-dum :refer :all]
            [evil-ant :as e]
            [evil-ant-test :refer :all])
  (import [evil_ant ClosedEmitterException]))

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
    (?= (e/remaining e) 100)))

(deftest restarting-timer
  (let [e (e/timer 100)]
    (Thread/sleep 5)
    (?true (<= (e/remaining e) 95))
    (e/restart! e)
    (?= (e/remaining e) 100)))

(deftest circular-timers
  (let [e (e/timer 0 :circular true)]
    (?emit= e [e])
    (?emit= e [e])))

(deftest timers-with-attachment
  (let [e (e/timer 0 :attachment 100500)]
    (?= (e/attachment e) 100500)))

(deftest oneoff-timers
  (let [e (e/timer 0 :one-off true)]
    (?emit= e [e])
    (?false (e/open? e))))

;timer sets

(comment
(deftest switch-sets
  (let [es (repeatedly 10 #(e/switch))
        a (atom [])
        hs (doall (map #(action-handler a %) es))
        s (apply e/switch-set es)]
    (doall (map #(e/turn-on! %) (take 3 (drop 2 es))))
    (e/emit! s 123)
    (?= (set @a) (set (map (fn [x] {:emitter x :src 123}) (take 3 (drop 2 es)))))))
    
(deftest conjing-active-switch
  (let [e (e/switch)
        s (e/switch-set)
        a (atom [])
        h (action-handler a e)]
    (e/turn-on! e)
    (e/conj! s e)
    (e/emit! s 123)
    (?actions= a [e 123])))

(deftest disjing-active-switch
  (let [e (e/switch) s (e/switch-set e)
        a (atom []) h (action-handler a e)]
    (e/turn-on! e)
    (e/disj! s e)
    (e/emit! s 123)
    (?actions= a)))

(deftest emitting-on-set-without-turned-on-switches-block
  (let [es (repeatedly 5 #(e/switch))
        s (apply e/switch-set es)
        f (future (e/emit! s 123))]
    (Thread/sleep 2)
    (?false (realized? f))
    (e/turn-on! (nth es 2))
    (Thread/sleep 2)
    (?true (realized? f))))

(deftest disabled-switches-affect-selection
  (let [a (atom [])
        e (e/switch)
        h (action-handler a e)
        s (e/switch-set e)]
    (e/disable! e)
    (let [f (future (e/emit! s 123))]
      (Thread/sleep 2)
      (?false (realized? f))
      (future-cancel f))))

(deftest closing-switch-set
  (let [e (e/switch)
        s (e/switch-set e)]
    (.close s)
    (?= (seq (e/emitters e)) nil)
    (?throws (e/emit! s 123) ClosedEmitterException)
    (?throws (e/emit-in! s 123 111) ClosedEmitterException)
    (?throws (e/emit-now! s 123) ClosedEmitterException)))
