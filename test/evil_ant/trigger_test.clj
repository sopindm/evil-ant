(ns evil-ant.trigger-test
  (:require [khazad-dum :refer :all]
            [evil-ant :as e]
            [evil-ant-test :refer :all])
  (import [evil_ant ClosedEmitterException]))

(deftest simple-triggering
  (let [e (e/trigger)]
    (?emit= e [e])))

(deftest one-off-triggers
  (let [e (e/trigger :one-off true)]
    (?emit= e [e])
    (?false (e/open? e))))

(deftest trigger-sets
  (let [es (repeatedly 10 #(e/trigger))
        a (atom [])
        hs (doall (map #(action-handler a %) es))
        s (apply e/trigger-set es)]
    (doall (map #(e/touch! %) (take 3 (drop 2 es))))
    (e/emit! s 123)
    (?= (set @a) (set (map (fn [x] {:emitter x :src 123}) (take 3 (drop 2 es)))))))

(deftest trigger-sets-never-block
  (let [es (repeatedly 5 #(e/trigger))
        s (apply e/trigger-set es)]
    (let [f (future (e/emit! s 123))]
      (Thread/sleep 1)
      (?true (realized? f)))
    (let [f (future (e/emit-in! s 123 1000))]
      (Thread/sleep 1)
      (?true (realized? f)))
    (?true (e/emit-now! s 123))))

(deftest closing-trigger-set
  (let [e (e/trigger)
        s (e/trigger-set e)]
    (.close s)
    (?= (seq (e/emitters e)) nil)
    (?throws (e/emit! s 123) ClosedEmitterException)
    (?throws (e/emit-in! s 123 111) ClosedEmitterException)
    (?throws (e/emit-now! s 123) ClosedEmitterException)))

(deftest emitting-trigger-in-triggers-handler
  (let [e1 (e/trigger)
        e2 (e/trigger)
        s (e/trigger-set e1 e2)
        actions (atom [])]
    (e/handler ([e s] (e/touch! e2)) e1)
    (action-handler actions e1)
    (action-handler actions e2)
    (e/touch! e1)
    (e/emit! s 123)
    (?actions= actions [e1 123] [e2 123])))




