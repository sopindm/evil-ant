(ns evil-ant.switch-test
  (:require [khazad-dum :refer :all]
            [evil-ant :as e]
            [evil-ant-test :refer :all])
  (import [evil_ant ClosedEmitterException]))

(deftest simple-switching
  (let [e (e/switch)
        actions (atom [])
        h (action-handler actions e)]
    (e/turn-on! e)
    (e/emit! e 123)
    (?actions= actions [e 123])))

(deftest switching-for-turned-off-switch-blocks
  (let [e (e/switch)
        a (atom [])
        h (action-handler a e)
        f (future (e/emit! e 123))]
    (Thread/sleep 2)
    (?false (realized? f))
    (e/turn-on! e)
    (Thread/sleep 2)
    (?true (realized? f))
    (?actions= a [e 123])))

(deftest emit-now-for-switches
  (let [e (e/switch)
        a (atom [])
        h (action-handler a e)]
    (e/emit-now! e 123)
    (?= (seq @a) nil)
    (e/turn-on! e)
    (e/emit-now! e 123)
    (?actions= a [e 123])))

(deftest emit-in-for-switches
  (let [e (e/switch)
        a (atom [])
        h (action-handler a e)
        f (future (e/emit-in! e 123 4))]
    (Thread/sleep 2)
    (?false (realized? f))
    (Thread/sleep 4)
    (?true (realized? f))
    (?= (seq @a) nil)
    (let [f2 (future (e/emit-in! e 234 4))]
      (Thread/sleep 2)
      (e/turn-on! e)
      (Thread/sleep 1)
      (?true (realized? f2))
      (?actions= a [e 234]))))

(deftest switches-are-attachable
  (let [e (e/switch)]
    (e/attach! e 123)
    (?= (e/attachment e) 123)))

(deftest turning-switch-off
  (let [e (e/switch)
        a (atom [])
        h (action-handler a e)]
    (e/turn-on! e)
    (e/turn-off! e)
    (e/emit-now! e 123)
    (?actions= a)))

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


