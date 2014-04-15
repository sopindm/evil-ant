(ns evil-ant.switch-test
  (:require [khazad-dum :refer :all]
            [evil-ant :as e]
            [evil-ant-test :refer :all]))

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

;switch without handlers disabled

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

;switching with sets
;;emit on set without turned on switches blocks
;;switches with attachment emit attachment
;;disabled switches affect selection
;;emit-in and emit-now

(comment
(deftest simple-triggering
  (let [s (e/trigger-set)
        e (e/trigger)]
    (e/conj! s e)
    (?= (.provider e) s)
    (?= (seq (e/signals s)) [e])
    (e/start! e)
    (e/turn-on! e)
    (?= (seq (e/select s)) [e])))

(deftest touching-trigger-twice-has-no-effect
  (let [t (e/trigger)
        s (e/trigger-set t)]
    (e/start! t)
    (e/start! t)
    (?= (seq (e/select s)) [t])))

(deftest touching-several-triggers
  (let [[t1 t2] (repeatedly 2 e/trigger)
        s (e/trigger-set t1 t2)]
    (dorun (map e/start! [t1 t2]))
    (?= (set (e/select s)) #{t1 t2})))

(deftest touching-set-with-several-triggers
  (let [[t1 t2] (repeatedly 2 e/trigger)
        s (e/trigger-set t1 t2)]
    (e/start! t1)
    (?= (seq (e/select s)) [t1])))

(deftest select!-is-blocking-for-triggers
  (let [t (e/trigger)
        s (e/trigger-set t)
        f (future (e/select s))]
    (Thread/sleep 10)
    (?false (realized? f))
    (e/start! t)
    (let [a (agent f)]
      (send-off a #(deref %))
      (when (not (await-for 1000 a)) (throw (RuntimeException. "Agent timeout")))
      (?= (seq @a) [t]))))

(deftest do-selections-test
  (let [[t1 t2] (repeatedly 2 e/trigger)
        s (e/trigger-set t1 t2)]
    (e/start! t1)
    (e/start! t2)
    (let [a (atom [])]
      (e/do-selections [e s]
        (swap! a conj e))
      (?= (set @a) #{t1 t2})
      (?= (seq (e/selections s)) nil))))

(deftest for-selections-test
  (let [triggers (repeatedly 100 e/trigger)
        s (apply e/trigger-set triggers)]
    (doall (map e/start! triggers))
    (comment 
    (?= (set (e/for-selections [e s] e)) (set triggers))
    (?= (e/for-selections [e s :timeout 0] e) [])
    (doall (map e/start! (take 3 triggers)))
    (?= (e/for-selections [e s] 1) (repeat 3 1)))))

(deftest registering-trigger-in-multiple-sets-error
  (let [t (e/trigger)]
    (e/conj! (e/trigger-set) t)
    (?throws (e/conj! (e/trigger-set) t) IllegalArgumentException)))

(deftest selecting-tirgger-now
  (let [s (e/trigger-set)]
    (?= (seq (e/select s :timeout 0)) nil)
    (let [t (e/trigger)]
      (e/conj! s t)
      (e/start! t)
      (?= (seq (e/select s :timeout 0)) [t]))))

(deftest selecting-trigger-with-timeout
  (let [s (e/trigger-set (e/trigger))
        f (future (e/select s :timeout 3))]
    (Thread/sleep 1)
    (?false (realized? f))
    (Thread/sleep 6)
    (?true (realized? f))))

(deftest canceling-trigger
  (let [[t1 t2] (repeatedly 2 e/trigger)
        s (e/trigger-set t1 t2)]
    (e/cancel! t1)
    (e/select s :timeout 0)
    (?= (set (e/signals s)) #{t2})
    (let [f (future (e/select s :timeout 4))]
      (Thread/sleep 2)
      (e/cancel! t2)
      (?= (set @f) #{})
      (?= (set (e/signals s)) #{}))))

(deftest closing-trigger-provider
  (let [t (e/trigger)
        s (e/trigger-set t)]
    (e/start! t)
    (.close s)
    (?= (.provider t) nil)
    (?= (seq (e/signals s)) nil)
    (?= (seq (e/selections s)) nil)
    (?throws (e/select s) ClosedSelectorException)
    (?throws (e/select s :timeout 0) ClosedSelectorException)
    (?throws (e/select s :timeout 10) ClosedSelectorException)
    (?throws (e/conj! s t) ClosedSelectorException)))

(deftest closing-tirgger-event
  (let [t (e/trigger 123)
        s (e/trigger-set t)]
    (?= (.provider t) s)
    (e/start! t)
    (.close t)
    (?= (.provider t) nil)
    (e/select s :timeout 0)
    (?= (e/attachment t) nil)
    (?= (seq (e/signals s)) nil)))

(deftest trigger-set-without-events-doesnt-block
  (let [s (e/trigger-set)]
    (?= (seq (e/select s)) nil)
    (?= (seq (e/select s :timeout 1000000)) nil)))

(deftest handling-trigger
  (let [t (e/trigger)
        actions (atom [])
        h (e/handler ([e s] (swap! actions conj {:emit e :src s})) t)]
    (e/attach! t 123)
    (e/emit! t 123)
    (?= (seq @actions) [{:emit t :src 123}])
    (.handle t 456)
    (?= (seq @actions) [{:emit t :src 123} {:emit t :src 456}])))

(deftest closing-trigger-closes-handles
  (let [t (e/trigger)
        h (e/handler () t)]
    (.close t)
    (?= (seq (e/emitters h)) nil)))

(deftest disj-for-trigger-sets
  (let [e (e/trigger)
        s (e/trigger-set e)]
    (e/disj! s e)
    (e/select s :timeout 0)
    (?= (.provider e) nil)
    (?= (seq (e/signals s)) nil)))

(deftest making-persistent-trigger
  (let [e (e/trigger)
        s (e/trigger-set e)]
    (e/set-persistent! e true)
    (?true (e/persistent? e))
    (e/start! e)
    (?= (e/for-selections [e s] (e/emit! e (e/attachment e)) e) [e])
    (?= (seq (e/select s :timeout 0)) [e]))))
