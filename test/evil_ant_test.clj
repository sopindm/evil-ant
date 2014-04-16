(ns evil-ant-test
  (:require [evil-ant :as e]
            [khazad-dum :refer :all])
  (:import [evil_ant ClosedEmitterException ClosedAbsorberException]))

(defmacro ?handlers= [event coll] `(?= (seq (e/handlers ~event)) ~coll))
(defmacro ?events= [handler coll] `(?= (seq (e/events ~handler)) ~coll))

(defn conj-action [actions e s] (swap! actions conj {:src s :emitter e}))

(defmacro action-handler [atom event]
  `(e/handler ([e# s#] (conj-action ~atom e# s#)) ~event))
(defmacro ?action= [actions [emitter source]]
  `(?= (deref ~actions) [{:src ~source :emitter ~emitter}]))
(defmacro ?actions= [actions & expected]
  `(?= (deref ~actions) [~@(map (fn [[e s]] {:emitter e :src s}) expected)]))

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

(deftest events-as-handlers
  (let [actions (atom [])
        e (e/event)
        h (e/event ([e s] (conj-action actions e s)) e)]
    (e/emit! e 123)
    (?action= actions [e 123])))

(deftest one-shot-event
  (let [actions (atom [])
        e (e/event () :one-shot)
        h (action-handler actions e)]
    (e/emit! e 123)
    (?action= actions [e 123])
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
;; Events multiset
;;

(comment
(defmacro with-events [[trigger [timer timeout] [reader writer] set] & body]
  `(let [~trigger (e/trigger)
         ~timer (e/timer ~timeout)
         pipe# (pipe-)
         [~'a-pipe-reader ~'a-pipe-writer] pipe#
         ~reader (e/selector (first pipe#) :read)
         ~writer (e/selector (second pipe#) :write)
         ~set (e/event-set ~trigger ~timer ~reader ~writer)]
     ~@body
     (.close ~'a-pipe-reader)
     (.close ~'a-pipe-writer)))

(defn pipe- []
  (let [pipe (java.nio.channels.Pipe/open)]
    (.configureBlocking (.sink pipe) false)
    (.configureBlocking (.source pipe) false)
    [(.source pipe) (.sink pipe)]))

(deftest making-multiset
  (with-events [trigger [timer 123] [reader writer] s]
    (?= (set (e/signals s)) #{trigger timer reader writer})
    (?= (-> s .triggers e/signals seq) [trigger])
    (?= (-> s .timers e/signals seq) [timer])
    (?= (-> s .selectors e/signals set) #{reader writer})
    (?throws (e/conj! s (proxy [madnet.event.Signal] [])) IllegalArgumentException)))

(deftest selecting-on-multiset-with-trigger
  (with-events [trigger [timer 1000] [selector _] s]
    (e/start! trigger)
    (e/start! timer)
    (e/start! selector)
    (?= (e/for-selections [e s] e) [trigger])))

(deftest selecting-on-multiset-with-timer
  (with-events [trigger [timer 0] [selector _] s]
    (e/start! timer)
    (e/start! selector)
    (Thread/sleep 2)
    (?= (e/for-selections [e s] e) [timer]))
  (with-events [trigger [timer 3] [selector _] s]
    (e/start! timer)
    (e/start! selector)
    (?= (e/for-selections [e s] e) [timer]))
  (with-events [trigger [timer 5] [selector _] s]
    (e/start! timer)
    (e/start! selector)
    (let [f (future (e/select s))]
      (Thread/sleep 2)
      (e/start! trigger)
      (?= (seq @f) [trigger]))))

(deftest selecting-on-multiset-with-selector
  (with-events [trigger [timer 10] [reader writer] s]
    (e/start! timer)
    (e/start! reader)
    (e/start! writer)
    (?= (e/for-selections [e s] e) [writer]))
  (with-events [trigger [timer 10] [reader writer] s]
    (e/start! timer)
    (e/start! reader)
    (let [f (future (e/select s))]
      (Thread/sleep 3)
      (.write a-pipe-writer (java.nio.ByteBuffer/wrap (byte-array (map byte (range 10)))))
      (?= (seq @f) [reader])))
  (with-events [trigger [timer 10] [reader writer] s]
    (e/start! writer)
    (?= (seq (e/select s)) [writer])))

(deftest selecting-without-any-trigger
  (let [timer (e/timer 10)
        selector (e/selector (second (pipe-)) :write)
        s (e/event-set timer selector)]
    (e/start! selector)
    (e/start! timer)
    (?= (seq (e/select s)) [selector]))
  (let [selector (e/selector (second (pipe-)) :write)
        s (e/event-set selector)]
    (e/start! selector)
    (?= (seq (e/select s)) [selector])))

(deftest selecting-without-any-selector
  (let [timer (e/timer 10)
        trigger (e/trigger)
        s (e/event-set timer trigger)]
    (e/start! trigger)
    (e/start! timer)
    (?= (seq (e/select s)) [trigger]))
  (let [trigger (e/trigger)
        s (e/event-set trigger)]
    (e/start! trigger)
    (?= (seq (e/select s)) [trigger])))

(deftest selecting-event-set-now
  (with-events [trigger [timer 0] [reader writer] s]
    (e/start! trigger)
    (e/start! timer)
    (e/start! reader)
    (e/start! writer)
    (?= (set (e/select s :timeout 0)) #{trigger timer writer})))

(deftest selecting-multievent-with-timeout
  (with-events [trigger [timer 8] [reader writer] s]
    (e/start! timer)
    (e/start! reader)
    (?= (seq (e/select s :timeout 3)) nil)
    (e/start! trigger)
    (e/start! writer)
    (?= (set (e/for-selections [e s :timeout 3] e)) #{trigger writer})
    (e/stop! writer)
    (?= (set (e/for-selections [e s :timeout 10] e)) #{timer})))

(defmacro with-timeout [timeout & form]
  `(let [agent# (agent nil)]
     (send-off agent# (fn [s#] ~@form))
     (when-not (await-for ~timeout agent#)
       (throw (Exception. (str "Agent timeout: " (agent-errors agent#)))))))

(deftest selecting-multiset-with-only-timeouts
  (let [e (e/timer 4)
        s (e/event-set e)]
    (e/start! e)
    (?= (seq (e/for-selections [e s] e)) [e])
    (e/start! e)
    (?= (seq (e/for-selections [e s :timeout 0] e)) nil)
    (?= (seq (e/for-selections [e s :timeout 1] e)) nil)
    (?= (seq (e/for-selections [e s :timeout 10] e)) [e])))

(deftest closing-multiset
  (with-events [trigger [timer 0] [reader writer] s]
    (.close s)
    (?false (.isOpen s))
    (?false (.isOpen (.triggers s)))
    (?false (.isOpen (.timers s)))
    (?false (.isOpen (.selectors s)))))

(deftest for-selections-into
  (let [[t1 t2] (repeatedly 2 e/trigger)
        s (e/event-set t1 t2)]
    (e/start! t1) (e/start! t2)
    (?= (e/for-selections [e s :into #{}] e) #{t1 t2})))

(deftest disj-on-multiset
  (with-events [trigger [timer 0] [reader writer] s]
    (e/disj! s trigger timer reader writer)
    (e/select s :timeout 0)
    (?= (seq (e/signals s)) nil)
    (?= (.provider trigger) nil)
    (?= (.provider timer) nil)
    (?= (.provider reader) nil)
    (?= (.provider writer) nil)
    (?throws (e/disj! s (proxy [madnet.event.Signal] [])) IllegalArgumentException)))

(deftest default-events-persistence
  (with-events [trigger [timer 0] [reader writer] s]
    (?false (e/persistent? trigger))
    (?false (e/persistent? timer))
    (?true (e/persistent? reader))
    (?true (e/persistent? writer))))

;making triggers, selectors with explicit persistence

;;
;; Event loops
;;

(deftest event-loops
  (let [a (atom [])
        t1 (e/trigger 1)
        t2 (e/trigger 2)
        s (e/event-set t1 t2)
        e1 (e/event ([e s] (swap! a conj s)) t1)
        e2 (e/event ([e s] (swap! a conj s)) t2)
        f (future (e/loop s))]
    (e/start! t1)
    (Thread/sleep 1)
    (?= @a [1])
    (reset! a [])
    (e/start! t1) (e/start! t2)
    (Thread/sleep 1)
    (?= (set @a) #{1 2})
    (future-cancel f))))



