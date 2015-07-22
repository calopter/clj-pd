(ns clj_pd.parse
  "from pd file to nested vector of object maps"
  (:require [clojure.string :as s]
            [clojure.zip :as zip]))

(def path "ftest.pd")

(defn objs [path]
  "grabs the string pertaining to each object from file"
  (-> path
      slurp
      (s/split #";\n")))

(defn parse-num [s]
  "if s contains a number, read-strings it"
  (if-let [num (re-find #"[\d.]+" s)]
    (read-string num)
    s))

(defn els [box-string]
  "splits into elements and parses nums" ;; drops the #X or #N
  (rest (map parse-num (s/split box-string #" "))))

(defn box-map [box-string]
  (let [[type x y] (els box-string)]
    (array-map :type type
               :x x :y y
               :rest (drop 3 (els box-string)))))

(defn zip-array-map [ks vs]
  (apply array-map (interleave ks vs)))

(defn assoc-last [m k v]
  "adds a value to the tail of an array-map"
  (merge (array-map k v) (reverse m)))

(defmulti to-map
  "converts string to box, canvas or connect array-map, dispatches on type"
  (fn [string] (first (els string))))

(defmethod to-map "canvas" [string]
  (zip-array-map [:type :arg1 :arg2 :width :height :name :visible]
                 (els string)))

(defmethod to-map "restore" [string]
  (let [bmap (box-map string)
        pd (first (:rest bmap))
        name (last (:rest bmap))
        rest (zip-array-map [:pd :name] [pd name])]
    (dissoc (merge rest (reverse bmap)) :rest)))

(defmethod to-map "obj" [box-string]
  (let [bmap (box-map box-string)
        name (first (:rest bmap))
        args (rest (:rest bmap))
        rest (zip-array-map [:name :args] [name args])]
    (dissoc (merge rest (reverse bmap)) :rest)))

(defmethod to-map "msg" [box-string]
  (let [bmap (box-map box-string)
        text (:rest bmap)]
    (-> bmap (assoc-last :text text) (dissoc :rest))))

(defmethod to-map "floatatom" [box-string]
  (dissoc (box-map box-string) :rest))

(defmethod to-map "text" [box-string]
  (let [bmap (box-map box-string)
        text (->> bmap :rest (interpose " ") (apply str))]
    (-> bmap (assoc-last :text text) (dissoc :rest))))

(defmethod to-map "connect" [string]
  (zip-array-map [:type :obox :olet :ibox :ilet]
                 (els string)))

(defn map-when [pred f coll]
  "applies f to each item in coll that satisfies pred"
  (map #(if (pred %) (f %) %) coll))

(defn fix-main-canvas [canvas-map]
  "the primary canvas has no name arg"
  (-> canvas-map
      (assoc :canvas :self)
      (assoc :name :main)
      (assoc-last :visible 16)))

(defn deepen [steps]
  "zips a vector of objs by canvas"
  (->> steps
       (reduce (fn [loc step]
                 (case (:type step)
                   "canvas" (-> loc
                                (zip/append-child [step])
                                (zip/down)
                                (zip/rightmost))
                   "restore" (-> loc
                                 zip/up
                                 (zip/append-child step))
                   (zip/append-child loc step)))
               (zip/vector-zip []))))

(defn zip-map [f z]
  "applies f to loc, not node in loc"
  (loop [loc z]
    (cond (zip/end? loc) (-> loc zip/root first)
          (zip/branch? loc) (recur (zip/next loc))
          :else (recur (-> loc f zip/next)))))

(defn parent [loc]
  "assocs the parent canvas to the node in loc"
  (let [canvas (if (= (-> loc zip/node :type) "canvas")
                 (-> loc zip/up zip/up zip/next zip/node :name)
                 (-> loc zip/leftmost zip/node :name))]
    (-> loc (zip/edit #(assoc % :canvas canvas)))))

(defn connectable? [node]
  (contains? #{"obj" "msg" "floatatom"} (:type node)))
(defn box-num [loc]
  "assocs the index of the box in its canvas to the node in loc"
  (if (-> loc zip/node connectable?)
    (let [num (->> loc zip/lefts (filter connectable?) count)]
      (-> loc (zip/edit #(assoc-last % :box-num num))))
    loc))

(defn grab-objs [path]
  (->> path
       objs
       (map to-map)
       (map-when #(= (:name %) 16) fix-main-canvas)
       vec
       deepen
       (zip-map (comp box-num parent))))
