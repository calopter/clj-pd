(ns clj_pd.core
  (:require [clojure.string :as s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def port 8000)
(def path "server.pd")

(def test-obj
  (array-map :canvas "pd-new" :type "obj" :x 10 :y 20 :name "osc~"))
(def test-msg
  (array-map :canvas "pd-new" :type "msg" :x 100 :y 100 :text "hello world"))
(def test-float
  (array-map :canvas "pd-new" :type "floatatom" :x 100 :y 100))
(def test-symbol
  (array-map :canvas "pd-new" :type "symbolatom" :x 350 :y 70 :symbol "symbol"))
(def test-text
  (array-map :canvas "pd-new" :type "text" :x 350 :y 130 :text "Hello World!"))
(def test-connection
  (array-map :canvas "pd-new" :type "connect" :orig-obj 0 :outlet 0 :dest-obj 1 :inlet 0))
(def test-graph
  (array-map :canvas "pd-new" :type "graph" :name "mygraph"))
;; add array to this

;;add args vector

(defn clear-canvas [canvas]
  (send-pd (str canvas " clear;")))

(defn parse-out [arrmap]
  (let [els (->> arrmap
                 vals
                 (interpose " ")
                 (map str)
                 (apply str))
        msg (str els \; \newline)]
    (send-pd msg)))

 (defn send-pd [msg]
     (let [command (format "echo '%s' | ./pdsend %d" msg port)]
       (clojure.java.shell/sh "bash" "-c" command)))

(defn run-pd [path]
  (future (clojure.java.shell/sh "pd" path)))

(defn armpify [canvas type x y name message]
  (array-map :canvas canvas :type type :x x :y y :name name :message message))

(defn osc [x y freq]
  (send-pd (parse-out (armpify "pd-new" "obj" x  y "osc~" freq))))

 (defn dac [x y]
     (send-pd (parse-out (armpify "pd-new" "obj" x  y "dac~" ""))))

(defn slider [x y]
     (send-pd (parse-out (armpify "pd-new" "obj" x  y "slider" ""))))

