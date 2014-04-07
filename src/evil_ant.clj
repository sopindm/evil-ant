(ns evil-ant
  (:refer-clojure :exclude [conj! disj!])
  (:import [evil_ant Event Handler]))

;;
;; Emitter/absorber functions
;;

(defn emit! [emitter arg] (.emit emitter arg))

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

;;
;; Events and handlers 
;;

(defmacro handler- [class [[emitter source] & handler] setup & events-and-options]
  `(~setup (proxy [~class] [] (absorb [~(or emitter '_) ~(or source '_)] ~@handler))
           ~@events-and-options))

(defmacro handler [[[emitter source] & handler] & events]
  `(handler- Handler ([~emitter ~source] ~@handler) #'absorber-conj ~@events))

(defn- setup-event- 
  ([event] event)
  ([event & args]
     (let [events (remove keyword? args)
           options (filter keyword? args)]
       (when (some #{:one-shot} options) (.oneShot_$eq event true))
       (apply absorber-conj event events))))

(defmacro event
  ([] `(Event.))
  ([[[emitter source] & handler] & events]
     `(handler- Event #'setup-event- ([~emitter ~source] ~@handler) ~@events)))

(defn- scala-set [set] (scala.collection.JavaConversions/asJavaSet set))

(defn handlers [event] (scala-set (.handlers event)))
(defn events [handler] (scala-set (.events handler)))




