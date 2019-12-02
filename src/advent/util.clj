(ns advent.util
  (:refer-clojure :exclude [slurp])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Misc utils

(defn map-vals [f m]
  (reduce-kv (fn [m k v] (assoc m k (f v))) m m))

;; File parsing functions

(defn lines
  "Returns a seq of lines from a file."
  [f]
  (with-open [r (io/reader f)]
    (->> r
         (line-seq)
         (doall))))

(defn slurp
  "Slurp and remove any trailing newlines."
  [f]
  (str/trim-newline (clojure.core/slurp f)))

;; File finding functions

(defn file-from-symbol [sym]
  (let [sym-ns (or (find-ns sym)
                   (some->> (resolve sym)
                            meta
                            :ns))
        [_ year day] (some->> sym-ns
                              ns-name
                              name
                              (re-find #"(\d+)[.]day(\d+)"))
        input (format "%s/day%s.txt" year day)]
    (if input
      input
      (if-not sym-ns
        (throw (ex-info (str "Unable to resolve symbol or namespace: " sym) {:sym sym}))
        (throw (ex-info (str "Expected var in a `day` namespace; got " (or (resolve sym) sym-ns))
                        {:sym sym}))))))

(defmacro data
  "Returns the data filename for a namespace or var."
  ([]
   `(data ~(ns-name *ns*)))
  ([ns]
   `(io/resource ~(file-from-symbol ns))))

(defmacro run
  "Runs a function with the appropriate data."
  [f]
  `(~f (data ~f)))
