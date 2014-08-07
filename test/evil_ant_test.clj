(ns evil-ant-test
  (:require [evil-ant :as e]
            [khazad-dum :refer :all])
  (:import [evil_ant ClosedEmitterException ClosedAbsorberException]))

(defmacro ?handlers= [event coll] `(?= (seq (e/handlers ~event)) ~coll))
(defmacro ?events= [handler coll] `(?= (seq (e/events ~handler)) ~coll))

(defmacro -?emit= [emitter value form]
  `(let [actions# (atom [])]
     (with-open [handler# (e/handler ([e# s#] (swap! actions# #(conj % e#))) ~emitter)]
       ~form
       (?= (seq @actions#) ~value))))

(defmacro ?emit-now= [emitter value] `(-?emit= ~emitter ~value (e/emit-now! ~emitter (gensym))))
(defmacro ?emit-in= [emitter time value]
  `(-?emit= ~emitter ~value (e/emit-in! ~emitter (gensym) ~time)))
(defmacro ?emit= [emitter value] `(-?emit= ~emitter ~value (e/emit! ~emitter (gensym))))

(defn conj-action [actions e s] (swap! actions conj {:src s :emitter e}))

(defmacro action-handler [atom event]
  `(e/handler ([e# s#] (conj-action ~atom e# s#)) ~event))
(defmacro ?action= [actions [emitter source]]
  `(?= (deref ~actions) [{:src ~source :emitter ~emitter}]))
(defmacro ?actions= [actions & expected]
  `(?= (deref ~actions) [~@(map (fn [[e s]] {:emitter e :src s}) expected)]))

(defmacro with-pipe [[source sink] & body]
  `(let [[~source ~sink] (pipe-)] (with-open [~source ~source ~sink ~sink] ~@body)))

(deftest simple-events-and-handlers
  (let [actions (atom [])
        e (e/event)
        h (action-handler actions e)]
    (?handlers= e [h])
    (?events= h [e])
    (e/emit! e 123)
    (?action= actions [e 123])))

(deftest explicit-conj!
  (let [e (e/event)
        h (e/handler ())]
    (e/conj! e h)
    (?handlers= e [h])
    (?events= h [e]))
  (let [e (e/event)
        [h1 h2 h3] (repeatedly 3 #(e/handler ()))]
    (e/conj! e h1 h2 h3)
    (?handlers= e [h1 h2 h3])))

(deftest explicit-disj!
  (let [e (e/event)
        [h1 h2] (repeatedly 2 #(e/handler () e))]
    (e/disj! e h1)
    (?handlers= e [h2])
    (?events= h1 nil)
    (e/disj! e h2)
    (?handlers= e nil)
    (?events= h2 nil))
  (let [e (e/event)
        [h1 h2 h3] (repeatedly 3 #(e/handler () e))]
    (e/disj! e h1 h2 h3)
    (?handlers= e nil)))

(deftest conj!-thread-safety
  (let [e (e/event)
        agents (repeatedly 1000 #(agent e))]
    (doseq [a agents] (send a #(e/conj! % (e/handler ()))))
    (apply await agents)
    (?= (count (e/handlers e)) 1000))
  (let [h (e/handler ())
        agents (repeatedly 1000 #(agent h))]
    (doseq [a agents] (send a #(e/conj! (e/event) %)))
    (apply await agents)
    (?= (count (e/events h)) 1000)))

(deftest disj!-thread-safety
  (let [e (e/event)
        handlers (repeatedly 1000 #(e/handler () e))
        agents (map agent handlers)]
    (doseq [a agents] (send a #(e/disj! e %)))
    (apply await agents)
    (?= (count (e/handlers e)) 0))
  (let [h (e/handler ())
        events (repeatedly 1000 #(doto (e/event) (e/conj! h)))
        agents (map agent events)]
    (doseq [a agents] (send a #(e/disj! % h)))
    (apply await agents)
    (?= (count (e/events h)) 0)))

(deftest closing-handler
  (let [e (e/event)
        h (e/handler () e)]
    (e/close! h)
    (?handlers= e nil)
    (?events= h nil)))

(deftest closing-events
  (let [e (e/event)
        [h1 h2 h3] (repeatedly 3 #(e/handler () e))]
    (e/close! e)
    (?handlers= e nil)
    (?events= h1 nil)
    (?events= h2 nil)
    (?events= h3 nil)))

(deftest emitter-and-absorber-open?
  (let [e (e/event) h (e/handler ())]
    (?true (e/open? e))
    (?true (e/open? h))
    (e/close! e)
    (?false (e/open? e))
    (e/close! h)
    (?false (e/open? h))))

(deftest cannot-emit-closed-emitter
  (let [e (e/event)]
    (e/close! e)
    (?throws (e/emit! e 123) ClosedEmitterException)))

(deftest cannot-conj-for-closed-emitter-and-absorber
  (let [[e1 e2] (repeatedly 2 #(e/event))
        [h1 h2] (repeatedly 2 #(e/handler ()))]
    (e/close! e1)
    (?throws (e/conj! e1 h1) ClosedEmitterException)
    (e/close! h1)
    (?throws (e/conj! e2 h1) ClosedAbsorberException)))

(comment 
  (deftest cannot-conj-to-closed-emitter
    (let [e (e/event)
          agents (repeatedly 100000 #(agent (e/handler ())))]
      (doseq [a agents] (send-off a #(do (Thread/sleep 100) (e/conj! e %))))
      (Thread/sleep 100)
      (.close e)
      (?= (count (e/handlers e)) 0))
    (let [h (e/handler ())
          agents (repeatedly 100000 #(agent (e/event)))]
      (doseq [a agents] (send-off a #(do (Thread/sleep 100) (e/conj! % h))))
      (Thread/sleep 100)
      (.close h)
      (?= (count (e/events h)) 0)))
  "Takes too much time")

(deftest disabling-handler
  (let [actions (atom [])
        e (e/event)
        h (action-handler actions e)]
    (?true (e/enabled? h))
    (e/disable! h)
    (e/emit! e 123)
    (?= @actions [])
    (?events= h [e])))

(deftest reenabling-handler
  (let [actions (atom [])
        e (e/event)
        h (action-handler actions e)]
    (e/disable! h)
    (e/enable! h)
    (?true (e/enabled? h))
    (e/emit! e 123)
    (?action= actions [e 123])))

(deftest disabling-event
  (let [actions (atom [])
        e (e/event)
        h (action-handler actions e)]
    (e/disable! e)
    (e/emit! e 123)
    (?actions= actions)))

(deftest events-as-handlers
  (let [actions (atom [])
        e (e/event)
        h (e/event ([e s] (conj-action actions e s)) e)]
    (e/emit! e 123)
    (?action= actions [e 123])))

(deftest one-shot-event
  (let [e (e/event () :one-shot)]
    (?emit= e [e])
    (?false (e/open? e))
    (?handlers= e nil)))

(deftest when-any-event
  (let [actions (atom [])
        [e1 e2] (repeatedly 2 #(e/event))
        e (e/when-any e1 e2)
        h (action-handler actions e)]
    (e/emit! e1 1)
    (?action= actions [e 1])
    (e/emit! e2 2)
    (?actions= actions [e 1] [e 2])))

(deftest when-every-test
  (let [actions (atom [])
        [e1 e2 e3] (repeatedly 3 #(e/event))
        e (e/when-every e1 e2 e3)
        h (action-handler actions e)]
    (e/emit! e1 1)
    (e/emit! e2 2)
    (e/emit! e3 3)
    (?actions= actions [e 3])
    (?false (e/open? e))))

(deftest events-with-attachment
  (let [actions (atom [])
        e (e/event)
        h (action-handler actions e)]
    (e/attach! e :something)
    (?= (e/attachment e) :something)
    (e/emit! e 123)
    (?actions= actions [e :something])
    (.close e)
    (?= (e/attachment e) nil)))

;;
;; Event multisets
;;

(defn pipe- []
  (let [pipe (java.nio.channels.Pipe/open)]
    (.configureBlocking (.sink pipe) false)
    (.configureBlocking (.source pipe) false)
    [(.source pipe) (.sink pipe)]))

(defmacro with-events [[switch [timer timeout] [reader writer] set] & body]
  `(let [pipe# (pipe-)
         ~'actions (atom [])]
     (with-open [~switch (e/switch)
                 ~timer (e/timer ~timeout)
                 ~'a-pipe-reader (first pipe#)
                 ~'a-pipe-writer (second pipe#)
                 ~reader (e/selector ~'a-pipe-reader :read)
                 ~writer (e/selector ~'a-pipe-writer :write)
                 ~set (e/signal-set ~switch ~timer ~reader ~writer)
                 switch-handler# (action-handler ~'actions ~switch)
                 timer-handler# (action-handler ~'actions ~timer)
                 reader-handler# (action-handler ~'actions ~reader)
                 writer-handler# (action-handler ~'actions ~writer)]
       ~@body)))

(deftest making-multiset
  (with-events [switch [timer 123] [reader writer] s]
    (?= (set (e/absorbers s)) #{switch timer reader writer})
    (?= (-> s .switches e/absorbers seq) [switch])
    (?= (-> s .timers e/absorbers seq) [timer])
    (?= (-> s .selectors e/absorbers set) #{reader writer})
    (?throws (e/conj! s (proxy [evil_ant.Signal] [false])) IllegalArgumentException)))

(deftest emitting-multiset-with-switch
  (with-events [switch [timer 1000] [reader writer] s]
    (e/turn-on! switch)
    (e/disable! writer)
    (e/emit! s 123)
    (?actions= actions [switch 123])))

(deftest emitting-multiset-with-zero-timer
  (with-events [trigger [timer 0] [reader writer] s]
    (e/disable! writer)
    (e/emit-now! s 123)
    (?actions= actions [timer 123])))

(deftest emitting-multiset-with-nozero-timer
  (with-events [trigger [timer 5] [reader writer] s]
    (e/disable! writer)
    (e/emit! s 123)
    (?actions= actions [timer 123])))

(deftest selecting-on-multiset-no-active-timer
  (with-events [switch [timer 1000] [reader writer] s]
    (e/disable! writer)
    (let [f (future (e/emit! s 123) (:emitter (first @actions)))]
      (Thread/sleep 2)
      (e/turn-on! switch)
      (Thread/sleep 2)
      (?actions= actions [switch 123]))))

(deftest multisets-with-simple-selector
  (with-events [switch [timer 10] [reader writer] s]
    (e/emit! s 123)
    (?actions= actions [writer 123])))

(deftest blocking-emit-with-selectors
  (with-events [trigger [timer 10] [reader writer] s]
    (e/disable! writer)
    (let [f (future (e/emit! s 123) (map :emitter @actions))]
      (Thread/sleep 3)
      (.write a-pipe-writer (java.nio.ByteBuffer/wrap (byte-array (map byte (range 10)))))
      (?= (seq @f) [reader]))))

(deftest selecting-multiset-with-only-selector
  (with-pipe [source sink]
    (let [actions (atom [])]
      (with-open [selector (e/selector sink :write)
                  handler (action-handler actions selector)
                  s (e/signal-set selector)]
        (e/emit! s 123)
        (?actions= actions [selector 123])))))

(deftest selecting-multiset-with-selectors-and-timers
  (with-pipe [source sink]
    (let [actions (atom [])]
      (with-open [timer (e/timer 1000)
                  selector (e/selector sink :write)
                  s (e/signal-set timer selector)
                  handler (action-handler actions selector)]
        (e/emit! s 123)
        (?actions= actions [selector 123])))))

(deftest selecting-multiset-with-only-switch
  (let [actions (atom [])]
    (with-open [e (e/switch)
                s (e/signal-set e)
                h (action-handler actions e)]
      (e/turn-on! e)
      (e/emit! s 123)
      (?actions= actions [e 123]))))

(deftest selecting-without-selectors
  (let [actions (atom [])]
    (with-open [timer (e/timer 10)
                switch (e/switch)
                s (e/signal-set timer switch)
                h (action-handler actions switch)]
      (e/turn-on! switch)
      (e/emit! s 123)
      (?actions= actions [switch 123]))))

(deftest selecting-multiset-now
  (with-events [switch [timer 0] [reader writer] s]
    (e/turn-on! switch)
    (e/emit-now! s 123)
    (?= (set (map :emitter @actions)) #{switch timer writer})))

(deftest selecting-multiset-with-timeout
  (with-events [switch [timer 8] [reader writer] s]
    (e/disable! writer)
    (e/emit-in! s 123 3)
    (?actions= actions)
    (e/turn-on! switch)
    (e/enable! writer)
    (e/emit-in! s 100 3)
    (?= (set (map :emitter @actions)) #{switch writer})
    (e/disable! writer)
    (e/turn-off! switch)
    (reset! actions [])
    (e/emit-in! s 100 10)
    (?actions= actions [timer 100])))

(deftest closing-multiset
  (with-events [switch [timer 0] [reader writer] s]
    (.close s)
    (?false (.isOpen s))
    (?false (.isOpen (.switches s)))
    (?false (.isOpen (.timers s)))
    (?false (.isOpen (.selectors s)))))

(deftest disj-on-multiset
  (with-events [switch [timer 0] [reader writer] s]
    (e/disj! s switch timer reader writer)
    (e/emit-now! s 123)
    (?= (seq (e/absorbers s)) nil)
    (?= (seq (e/emitters switch)) nil)
    (?= (seq (e/emitters timer)) nil)
    (?= (seq (e/emitters reader)) nil)
    (?= (seq (e/emitters writer)) nil)
    (?throws (e/disj! s (proxy [evil_ant.ISignal] [false])) IllegalArgumentException)))
