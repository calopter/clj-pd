(ns clj_pd.talk)

(def port 8000)
(def server-path "server.pd")
(def pdsend-path "pdsend")
(def pd-path "pd")

(defn parse-out [arrmap]
  "array map to msg string for pdsend"
  (let [els (->> arrmap
                 vals
                 flatten ;; for the :args vector
                 (interpose " ")
                 (map str)
                 (apply str))]
    (str els \; \newline)))

(defn pd-send [msg]
  "calls pdsend from a shell, passing it msg"
     (let [command (format "echo '%s' | %s %d" msg pdsend-path port)]
       (clojure.java.shell/sh "bash" "-c" command)))

(defn pd-run [path]
  "runs pd as a subprocess"
  (future (clojure.java.shell/sh pd-path path)))

(def box-base (array-map :canvas "pd-new" :type nil :x 100 :y 100))

(def obj-base ["obj" (array-map :name "osc~" :args [440])])

(def msg-base ["msg" (array-map :text "bang")])

(def num-base ["floatatom" (array-map)])

(def send-box (comp pd-send parse-out box))

(def comment-base ["text" (array-map :text "Hello")])

(defn box
  "creates an appropriately merged array-map from box-base, base and vals-map"
  ([base] (box base {}))
  ([base vals-map]
   (let [m (merge (last base) (reverse box-base) vals-map)]
     (assoc m :type (first base)))))

(def send-box (comp pd-send parse-out box))
