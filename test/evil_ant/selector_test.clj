(ns evil-ant.selector-test
  (:require [khazad-dum :refer :all]
            [evil-ant :as e]
            [evil-ant-test :refer :all]))

(deftest making-selectors
  (with-pipe [source sink]
    (with-open [s (e/selector source :read)]
      (?throws (e/selector source :write) IllegalArgumentException)
      (?throws (e/selector source :accept) IllegalArgumentException)
      (?throws (e/selector source :connect) IllegalArgumentException))
    (with-open [s (e/selector sink :write)]
      (?throws (e/selector sink :read) IllegalArgumentException)
      (?throws (e/selector sink :accept) IllegalArgumentException)
      (?throws (e/selector sink :connect) IllegalArgumentException)))
  (with-open [acceptor (java.nio.channels.ServerSocketChannel/open)
              s (e/selector acceptor :accept)]
    (?throws (e/selector acceptor :read) IllegalArgumentException)
    (?throws (e/selector acceptor :write) IllegalArgumentException)
    (?throws (e/selector acceptor :connect) IllegalArgumentException))
  (with-open [connector (java.nio.channels.SocketChannel/open)
              s (e/selector connector :connect)]
    (?throws (e/selector connector :accept) IllegalArgumentException)))

(deftest simple-selector-sets
  (with-pipe [source sink]
    (let [actions (atom [])]
      (with-open [rdr (e/selector source :read)
                  wrt (e/selector sink :write)
                  s (e/selector-set rdr wrt)
                  e1 (action-handler actions rdr)
                  e2 (action-handler actions wrt)]
        (?= (set (e/absorbers s)) #{rdr wrt})
        (?= (seq (e/emitters rdr)) [s])
        (?= (seq (e/emitters wrt)) [s])
        (e/emit! s)
        (?actions= actions wrt)
        (.write sink (java.nio.ByteBuffer/wrap
                      (byte-array (map byte (range 5)))))
        (reset! actions [])
        (e/emit! s)
        (?= (set @actions) #{rdr wrt})))))

(deftest selectors-emit-now
  (with-pipe [source sink]
    (let [actions (atom [])]
      (with-open [writer (e/selector sink :write)
                  s (e/selector-set writer)
                  handler (action-handler actions writer)]
        (e/emit-now! s)
        (?actions= actions writer)))))

(deftest disabling-selectors
  (with-pipe [source sink]
    (let [actions (atom [])]
      (with-open [rdr (e/selector source :read)
                  wrt (e/selector sink :write)
                  s (e/selector-set rdr wrt)
                  e1 (action-handler actions rdr)
                  e2 (action-handler actions wrt)]
        (.write sink (java.nio.ByteBuffer/wrap (byte-array (map byte (range 5)))))
        (e/disable! wrt)
        (e/emit! s)
        (?actions= actions rdr)
        (e/disable! rdr)
        (e/emit-now! s)
        (?actions= actions rdr)
        (e/enable! wrt)
        (e/emit! s)
        (?actions= actions rdr wrt)))))

(deftest select-for-set-with-disabled-event-blocks
  (with-pipe [source sink]
    (let [actions (atom [])]
      (with-open [reader (e/selector source :read)
                  writer (e/selector sink :write)
                  s (e/selector-set reader writer)]
        (e/disable! writer)
        (let [f (future (e/emit! s))]
          (Thread/sleep 2)
          (?false (realized? f))
          (.signal s)
          (Thread/sleep 1)
          (?true (realized? f)))))))

(deftest selecting-with-timeout
  (with-pipe [source sink]
    (with-open [rdr (e/selector source :read)
                s (e/selector-set rdr)]
      (e/emit-in! s 3))))

(deftest closing-selection-set
  (with-pipe [source sink]
    (with-open [rdr (e/selector source :read)
                s (e/selector-set rdr)]
      (.close s)
      (?= (seq (e/emitters rdr)) nil))))

(deftest emitting-selector
  (with-pipe [source sink]
    (with-open [reader (e/selector source :read)
                writer (e/selector sink :write)
                s (e/selector-set reader writer)]
      (?emit= writer [writer])
      (?emit-in= writer 5 [writer])
      (?emit-now= writer [writer])
      (?emit-in= reader 5 nil)
      (?emit-now= reader nil))))
