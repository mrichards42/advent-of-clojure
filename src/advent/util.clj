(ns advent.util
  (:refer-clojure :exclude [slurp])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Misc utils

(defn map-vals [f m]
  (reduce-kv (fn [m k v] (assoc m k (f v))) m m))

(defn pfilter
  "Like pmap, but for filter"
  [pred xs]
  (->> xs
       (pmap (juxt pred identity))
       (filter first)
       (map second)))

(defn example-input
  "Removes indents from a string of text to make it easier to use as example
  input. The first line is ignored, and any shared indentation from all
  remaining lines is stripped.

  (example-input \"testing
                  1 2 3
                       indented!\")
  \"testing
  1 2 3
       indented!\"
  "
  [txt]
  (let [[fst & more] (str/split-lines txt)
        shared-indent (->> more
                           (filter (comp pos? count))
                           (map #(count (re-find #"^ *" %)))
                           (reduce min))]
    (if (pos-int? shared-indent)
      (->> (map (fn [line]
                  (if (seq line)
                    (subs line shared-indent)
                    line))
                more)
           (cons fst)
           (str/join "\n"))
      txt)))

;; File parsing functions

(defn ^:deprecated lines
  "Returns a seq of lines from a file."
  [input]
  (str/split-lines input))

(defn ^:deprecated slurp
  "Slurp and remove any trailing newlines."
  [input]
  (str/trim-newline input))

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
   `(-> (io/resource ~(file-from-symbol ns))
        (clojure.core/slurp)
        (str/trim-newline))))

(defmacro run
  "Runs a function with the appropriate data."
  [f]
  `(~f (data ~f)))
