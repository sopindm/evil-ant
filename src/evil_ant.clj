(ns evil-ant
  (:refer-clojure :exclude [conj! disj!])
  (:import [evil_ant Event Handler MultiSignalSet
                     SwitchSignal SwitchSet
                     TimerSignal TimerSet
                     SelectorSignal SelectorSet]
           [java.nio.channels SelectionKey]))

;;
;; Emitter/absorber functions
;;

(defn emit! [emitter arg] (.emit emitter arg))
(defn emit-now! [emitter arg] (.emitNow emitter arg))
(defn emit-in! [emitter arg timeout] (.emitIn emitter arg timeout))

(defn conj! ([emitter] emitter)
  ([emitter absorber] (.conj emitter absorber))
  ([emitter absorber & more-absorbers] (reduce conj! (conj! emitter absorber) more-absorbers)))

(defn disj! ([emitter] emitter)
  ([emitter absorber] (.disj emitter absorber))
  ([emitter absorber & more-absorbers] (reduce disj! (disj! emitter absorber) more-absorbers)))

(defn- absorber-conj
  ([a] a) 
  ([a e] (.conj e a) a)
  ([a e & es] (reduce absorber-conj (absorber-conj a e) es)))

(defn close! [obj] (.close obj))
(defn open? [obj] (.isOpen obj))

(defn enabled? [obj] (.isEnabled obj))
(defn enable! [obj] (.enable obj))
(defn disable! [obj] (.disable obj))

(defn attach! [emitter obj] (.attach emitter obj))
(defn attachment [emitter] (.attachment emitter))

;;
;; Events and handlers 
;;

(defmacro handler- [class args [[emitter source] & handler] setup & events-and-options]
  `(~setup (proxy [~class] [~@args] (absorb [~(or emitter '_) ~(or source '_)] ~@handler))
           ~@events-and-options))

(defmacro handler [[[emitter source] & handler] & events]
  `(handler- Handler () ([~emitter ~source] ~@handler) #'absorber-conj ~@events))

(defn- setup-event- 
  ([event] event)
  ([event & args]
     (let [events (remove keyword? args)
           options (filter keyword? args)]
       (apply absorber-conj event events))))

(defmacro event
  ([] `(Event.))
  ([[[emitter source] & handler] & events]
     `(handler- Event [~(boolean (some #{:one-shot} events))] 
                ([~emitter ~source] ~@handler) #'setup-event- ~@events)))

(defn- scala-collection [coll] (scala.collection.JavaConversions/asJavaCollection coll))

(defn emitters [absorber] (scala-collection (.emitters absorber)))
(defn absorbers [emitter] (scala-collection (.absorbers emitter)))

(defn handlers [event] (scala-collection (.handlers event)))
(defn events [handler] (scala-collection (.events handler)))

(defn when-any ([] (proxy [Event] [] (absorb [e s] (.emit this s))))
  ([& events] (apply absorber-conj (when-any) events)))

(defn when-every ([] (evil_ant.WhenEveryEvent.))
  ([& events] (apply absorber-conj (when-every) events)))

;;
;; Signals
;;

(defmacro defsignal [[name [& args] & body] [set-name & set-body]]
  `(do (defn ~name ([~@args] ~@body)
         ([~@args attachment#] (doto (~name ~@args) (attach! attachment#))))
       (defn ~set-name ([] ~@set-body)
         ([& triggers#] (reduce conj! (~set-name) triggers#)))))

(defn switch ([] (SwitchSignal.))
  ([& {:keys [attachment one-off]}]
     (doto (SwitchSignal. (boolean one-off)) (attach! attachment))))

(defn switch-set ([] (SwitchSet.))
  ([& signals] (reduce conj! (switch-set) signals)))
         
(defn turn-on! [switch] (.turnOn switch))
(defn turn-off! [switch] (.turnOff switch))

(declare start!)
(defn timer ([milliseconds] (doto (TimerSignal. milliseconds) start!))
  ([milliseconds & {:keys [circular attachment one-off] :as options}]
     (doto (TimerSignal. milliseconds (boolean circular) (boolean one-off))
       start!
       (attach! attachment))))
(defn timer-set ([] (TimerSet.))
  ([& timers] (reduce conj! (timer-set) timers)))

(defn timeout [timer] (.timeout timer))
(defn remaining [timer] (.remaining timer))

(defn start! [timer] (.start timer))
(defn stop! [timer] (.stop timer))
(defn restart! [timer] (stop! timer) (start! timer))

(defn- scala-operation [operation]
  (case operation
    :read SelectionKey/OP_READ
    :write SelectionKey/OP_WRITE
    :accept SelectionKey/OP_ACCEPT
    :connect SelectionKey/OP_CONNECT))

(defn selector [channel operation]
  (letfn [(check-option [name type]
            (when (and (= operation name) (not (instance? type channel)))
              (throw (IllegalArgumentException.))))]
    (check-option :write java.nio.channels.WritableByteChannel)
    (check-option :read java.nio.channels.ReadableByteChannel)
    (check-option :accept java.nio.channels.ServerSocketChannel)
    (check-option :connect java.nio.channels.SocketChannel)
    (SelectorSignal. channel (scala-operation operation))))

(defn selector-set ([] (SelectorSet.))
  ([& selectors] (reduce conj! (selector-set) selectors)))

(defn signal-set ([] (MultiSignalSet.))
  ([& signals] (reduce conj! (signal-set) signals)))
