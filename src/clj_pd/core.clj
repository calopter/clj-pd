(ns clj_pd.core)

(def box-base (array-map :canvas "pd-new" :type nil :x 100 :y 100))

(def obj-base ["obj" (array-map :name "osc~" :args [440])])

(def msg-base ["msg" (array-map :text "bang")])

(def num-base ["floatatom" (array-map)])

(def comment-base ["text" (array-map :text "Hello")])

(defn box
  "creates an appropriately merged array-map from box-base, base and vals-map"
  ([base] (box base {}))
  ([base vals-map]
   (let [m (merge (last base) (reverse box-base) vals-map)]
     (assoc m :type (first base)))))

(defn clear-canvas [canvas]
  "wipes canvas"
  (pd-send (str canvas " clear;")))

(defn s [tag val]
  "like pd s, sends to val the r object with tag"
  (pd-send (str tag " " val\;)))

(defn new-canvas [name]
  "see top 3.pdscript/test.txt")

(defn dsp [val]
  "turns dsp on or off - 1, 0"
  (pd-send (str "pd dsp " val \;)))
