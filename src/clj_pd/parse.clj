(ns clj_pd.parse
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
