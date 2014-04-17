(ns evil-ant
  (:refer-clojure :exclude [conj! disj!])
  (:import [evil_ant Event Handler SwitchSignal SwitchSet TimerSignal TimerSet]))

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

(defn- scala-set [set] (scala.collection.JavaConversions/asJavaSet set))

(defn emitters [absorber] (scala-set (.emitters absorber)))
(defn absorbers [emitter] (scala-set (.absorbers emitter)))

(defn handlers [event] (scala-set (.handlers event)))
(defn events [handler] (scala-set (.events handler)))

(defn when-any ([] (proxy [Event] [] (absorb [e s] (.emit this s))))
  ([& events] (apply absorber-conj (when-any) events)))

(defn when-every ([] (proxy [Event] [true]
                       (absorb [e s]
                         (disj! e this)
                         (when (-> this events .isEmpty)
                           (emit! this s)))))
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
  ([attachment] (doto (switch) (attach! attachment))))

(defn switch-set ([] (SwitchSet.))
  ([& signals] (reduce conj! (switch-set) signals)))
         
(defn turn-on! [switch] (.turnOn switch))
(defn turn-off! [switch] (.turnOff switch))

(defn timer [milliseconds] (TimerSignal.))

