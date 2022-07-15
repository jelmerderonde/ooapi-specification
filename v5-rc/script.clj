(ns script
  (:require
   [clojure.java.io :as io]
   [clj-yaml.core :as yaml]
   [clojure.walk :as walk]
   [clojure.string :as str]
   [babashka.fs :as fs]))

(def spec-file (io/file "spec.yaml"))

(defn slurp-yaml-impl
  [path]
  (yaml/parse-string (slurp path)
                     :keywords false))

(def slurp-yaml (memoize slurp-yaml-impl))

(def top-level-spec (slurp-yaml spec-file))

(defn ref?
  [form]
  (and (map? form)
       (contains? form "$ref")))

(defn attach-path-if-ref
  [path form]
  (if (ref? form)
    (with-meta form {:path path})
    form))

(defn relative-to-cwd
  [current-path path]
  (if current-path
    (let [current-folder (fs/parent current-path)]
      (loop [result (vec (fs/components current-folder))
             segments (vec (fs/components path))]
        (let [[segment & segments] segments]
          (if segment
            (recur
             (case (str segment)
               "." result
               ".." (vec (butlast result))
               (conj result segment))
             segments)
            (str (apply fs/path result))))))
    path))

(defn resolve-ref
  [ref]
  (let [current-path (:path (meta ref))
        path (get ref "$ref")
        cwd (relative-to-cwd current-path path)]
    (tap> {:current-path current-path
           :path path
           :cwd cwd
           :test (= current-path cwd)})
    (if (= current-path cwd)
      {"$recursive" current-path}
      (walk/postwalk (partial attach-path-if-ref cwd)
                     (slurp-yaml cwd)))))

(defn path-key?
  [x]
  (and (string? x)
       (str/starts-with? x "/")))

(defn keywordize
  [m]
  (->> m
       (map (fn [[k v]] [(if (path-key? k) k (keyword k)) v]))
       (into {})))

(defn walk-fn
  [form]
  (tap> form)
  (cond
    (ref? form)
    (resolve-ref form)

    (map? form)
    (keywordize form)

    :else
    form))

(def spec (walk/prewalk walk-fn top-level-spec))

(keys (:paths spec))