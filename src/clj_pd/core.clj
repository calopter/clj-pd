(ns clj_pd.core
  (:require [clojure.string :as s]))

(def path "ftest.pd")

(def canvas (slurp path))

(defn objs [canvas]
  (s/split canvas #";\n"))

(defn str-map->num [k m]
  (let [s (k m)
        num (read-string s)]
    (assoc m k num)))

(defn to-map [obj]
  (let [els (s/split obj #" ")
        keys [:hash :type :x :y :name]
        obj-map (zipmap keys els)]
    (->> obj-map (str-map->num :x) (str-map->num :y))))

(def obj-test (-> canvas objs second to-map))

(defn move [obj x y]
  (assoc obj :x x :y y))

(defn stringify [{:keys [hash type x y name]}]
  (let [order [hash type x y name]
        spaces (interpose " " order)
        strs (map str spaces)
        s (apply str strs)]
    (str s \; \newline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def port 8000)
(def server-path "server.pd")
(def pdsend-path "pdsend")
(def pd-path "pd")

(def base-keys [:canvas :type :x :y])

(defn zip-array-map [ks vs]
  "like zip-map but makes an array-map"
  (let [kv (interleave ks vs)]
    (apply array-map kv)))

(comment
  (def test-obj
    (array-map :canvas "pd-new" :type "obj" :x 10 :y 20 :name "osc~" :args [440])))

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

;; you can't override :type with make-obj,
;; use a protocol and reify make-obj as make-box?
;; each type then conjs the appropriate extra keywords to base-keys and calls make-box with it

;;TODO

;; finish box creation system
;; parse pd files
;; track all boxes in order to simplify making connections
;; deal with synchronizing between pd file and internal data structures

(defn obj
  "creates and sends an object from optional values map"
  ([] (obj {}))
  ([vals-map]
   (-> vals-map make-obj parse-out send-pd)))

(defn make-obj [vals-map]
  "merges optional values map with a defaults map,
   returns an obj as array-map"
  (let [ks (conj base-keys :name :args)
        vs ["pd-new" "obj" 100 100 "osc~" []]
        defaults (zip-array-map ks vs)]
    (merge defaults vals-map)))

(defn clear-canvas [canvas]
  "wipes canvas"
  (send-pd (str canvas " clear;")))

(defn parse-out [arrmap]
  "array map to msg string for pdsend"
  (let [els (->> arrmap
                 vals
                 flatten
                 (interpose " ")
                 (map str)
                 (apply str))]
    (str els \; \newline)))

(defn send-pd [msg]
  "calls pdsend from a shell, passing it msg"
     (let [command (format "echo '%s' | %s %d" msg pdsend-path port)]
       (clojure.java.shell/sh "bash" "-c" command)))

(defn run-pd [path]
  "runs pd as a subprocess"
  (future (clojure.java.shell/sh pd-path path)))
