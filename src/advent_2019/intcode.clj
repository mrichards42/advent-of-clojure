(ns advent-2019.intcode
  "Intcode computer"
  (:require [advent.util :as util]
            [clojure.edn :as edn]))

(defn parse-code
  "Reads code from a file"
  [f]
  (edn/read-string (format "[%s]" (util/slurp f))))


;;; Protocols

;; The "system" concept makes it possible to model everything as a program, so
;; ultimately I don't need these protocols, but it's a decent interface, so I'm
;; going to leave it here in case I need to do something more complicated.

(defprotocol Executable
  (get-state
   [this]
   "Returns the state of this machine.")
  (set-state
   [this state]
   "Sets the state of this machine.")
  (run-step
   [this]
   "Runs a single step of this machine."))

(defprotocol Input
  (send-input
   [this inputs]
   "Adds `inputs` to the program input queue."))

(defprotocol Output
  (read-output
   [this]
   "Reads and returns all output.")
  (consume-output
   [this]
   "Removes all output values."))


;;; Debug printer

(def ^:dynamic *debug* false)

(defn should-debug? [tag]
  (and *debug*
       (cond
         (keyword? *debug*) (= tag *debug*)
         (set? *debug*) (contains? *debug* tag)
         :else (true? *debug*))))

(defmacro debug [tag & args]
  (let [tags (cons tag (butlast args))
        code (last args)]
    `(if (or ~@(for [v [tag (namespace tag)]
                     :when v]
                 `(should-debug? ~(keyword v))))
       (do
         (let [ret# ~code]
           (println ";;" ~@tags (str "(" ret# ")"))
           ret#))
       ~code)))


;;; Program interpreter

;; Helpers

(defn digit-at [n place]
  (mod (quot n (apply * (repeat place 10))) 10))

(defn arg-mode
  "Returns the `mode` for argument at `idx` (1-based)."
  [instr idx]
  (let [place (+ 1 idx)]
    (digit-at instr place)))

(defn instr
  "Returns the instruction from a number (the least significant 2 digits)"
  [n]
  (mod n 100))

(defn load-raw
  "Returns the raw value of memory at `idx` offset from `ip`."
  [{:keys [memory ip]} idx]
  (debug :load/raw ip "+" idx
         (get memory (+ ip idx))))

(defn load-val
  "Returns the value for argument `idx` (1-based), based on the argument's mode

  Modes:
    0    argument is a pointer (returns memory value at ptr)
    1    argument is a value"
  [program idx]
  (debug :load/val (:ip program) "+" idx
         (let [v (load-raw program idx)
               instr (load-raw program 0)]
           (case (arg-mode instr idx)
             0 (get (:memory program) v)
             1 v))))

;; Instruction dispatch

(defn dispatch [{:keys [memory ip]}]
  (debug :instr/instr ip (instr (get memory ip))))

(defmulti interpret dispatch)

(defmethod interpret :default
  [{:keys [memory ip] :as program}]
  (throw (ex-info (format "Unknown op: %s" (instr (get memory ip)))
                  program)))

;; Basic instruction set

(defmethod interpret 1 ;; add
  [program]
  (let [a (load-val program 1)
        b (load-val program 2)
        p (load-raw program 3)]
    (-> program
        (update :memory assoc p (debug :op/add a b "=>" p (+ a b)))
        (update :ip + 4))))

(defmethod interpret 2 ;; multiply
  [program]
  (let [a (load-val program 1)
        b (load-val program 2)
        p (load-raw program 3)]
    (-> program
        (update :memory assoc p (debug :op/mul a b "=>" p (* a b)))
        (update :ip + 4))))

(defmethod interpret 3 ;; read input
  [{:keys [input] :as program}]
  (let [p (load-raw program 1)
        v (peek input)]
    (cond
      v (-> program
            (update :input pop)
            (update :memory assoc p (debug :op/input p v))
            (update :ip + 2))
      ;; `nil` means EOF
      (seq input) (assoc program :state (debug :op/input p :end))
      ;; no input means yield and wait for input
      :else (assoc program :state (debug :op/input p :waiting-for-input)))))

(defmethod interpret 4 ;; write output
  [program]
  (let [v (load-val program 1)]
    (-> program
        (update :output conj (debug :op/output v))
        (update :ip + 2))))

(defmethod interpret 5 ;; jnz
  [program]
  (let [check (load-val program 1)
        ip (load-val program 2)]
    (if-not (zero? check)
      (assoc program :ip (debug :op/jnz check ip))
      (do
        (debug :op/jnz check :ignore)
        (update program :ip + 3)))))

(defmethod interpret 6 ;; jz
  [program]
  (let [check (load-val program 1)
        ip (load-val program 2)]
    (if (zero? check)
      (assoc program :ip (debug :op/jz check ip))
      (do
        (debug :op/jz check :ignore)
        (update program :ip + 3)))))

(defmethod interpret 7 ;; lt
  [program]
  (let [a (load-val program 1)
        b (load-val program 2)
        p (load-raw program 3)]
    (-> program
        (assoc-in [:memory p] (debug :op/lt a b "=>" p (if (< a b) 1 0)))
        (update :ip + 4))))

(defmethod interpret 8 ;; eq
  [program]
  (let [a (load-val program 1)
        b (load-val program 2)
        p (load-raw program 3)]
    (-> program
        (assoc-in [:memory p] (debug :op/eq a b "=>" p (if (= a b) 1 0)))
        (update :ip + 4))))

(defmethod interpret 99 ;; halt
  [program]
  (assoc program :state :end))


;;; Program

(defrecord Program
  [id memory ip state input output]
  Executable
  (get-state [this] state)
  (set-state [this state] (assoc this :state state))
  (run-step [this] (interpret this))
  Input
  (send-input [this input] (update this :input into input))
  Output
  (read-output [this] output)
  (consume-output [this] (update this :output empty)))


(def pid (atom 1))
(defn new-pid []
  (swap! pid inc))

(defn program [memory]
  (map->Program
   {:id (new-pid)
    :memory (vec memory)
    :ip 0
    :state :init
    :input (clojure.lang.PersistentQueue/EMPTY)
    :output []}))


;;; Execution

(def paused-states #{:init :waiting-for-input})
(def running-states #{:running})
(def end-states #{:end})

(def paused? (comp paused-states get-state))
(def running? (comp running-states get-state))
(def end? (comp end-states get-state))

(defn run-while
  "Runs the program as long as `f` is true."
  [program f]
  (if (f program)
    (recur (run-step program) f)
    program))

(defn run-until
  "Runs the program until `f` is false."
  [program f]
  (run-while program (complement f)))

(defn resume
  "Resumes a paused program and runs until the program halts or yields."
  [program]
  (if (paused? program)
    (-> program
        (set-state :running)
        (run-while running?))
    program))

(defn run-program
  "Runs a program with the given input until it halts or yields."
  [memory & input]
  (-> (program memory)
      (as-> state (send-input state input))
      (resume)))


;;; Standard Programs and System Calls

(defn file
  "Returns a program that copies input to output."
  []
  (program [3 8 4 8 1105 1 0 99 0]))

(def EOF nil)

(def stdin (assoc (file) :id "stdin"))
(def stdout (assoc (file) :id "stdout"))

(defn fork
  "Returns a copy of this program with a new id."
  [program]
  (assoc program :id (new-pid)))


;;; System

(declare pipe)

(defn system
  "Returns a new system with an optional pipeline."
  [& pipeline]
  (let [s {:programs {}
           :root nil
           :children {}
           :parents {}}]
    (if (seq pipeline)
      (apply pipe s (flatten pipeline))
      s)))

(defn get-program
  "Returns the program named by `id`."
  [system id]
  (get-in system [:programs id]))

(defn update-program
  "Updates the program named by `id`."
  [system id f & args]
  (apply update-in system [:programs id] f args))

(defn pipe
  "Adds `a` and `b` to the `system`, connecting the output from `a` to
  the input for `b`.

  If there are no programs in the system, the system will start each execution
  pass with `a`."
  [system a b & more]
  (let [programs (cons a (cons b more))
        add-program (fn [s program]
                      (cond-> s
                        true (assoc-in [:programs (:id program)] program)
                        (not (:root s)) (assoc :root (:id program))))
        connect (fn [s [a b]]
                  (-> s
                      (update-in [:children (:id a)] (fnil conj []) (:id b))
                      (update-in [:parents (:id b)] (fnil conj []) (:id a))))]
    (as-> system $
      (reduce add-program $ programs)
      (reduce connect $ (partition 2 1 programs)))))

;; System execution

(defn run-system-step
  "Runs a single program in the system, copying its output to the input of any
  connected programs."
  [system id children]
  (if (end? (get-program system id))
    system
    (let [;; check for EOF from parents
          parent-programs (map #(get-program system %) (get-in system [:parents id]))
          eof? (and (seq parent-programs) (every? end? parent-programs))
          ;; run program
          s (cond-> system
              eof? (update-program id send-input [EOF])
              true (update-program id resume))
          ;; copy output -> input
          output (read-output (get-program s id))
          s (reduce #(update-program %1 %2 send-input output) s children)
          ;; consume output
          s (if (seq children)
              (update-program s id consume-output)
              s)]
      (debug :system/step id (seq output))
      s)))

(defn run-system-pass
  "Runs all programs in the system until they halt or yield."
  [system]
  (loop [queue (conj (clojure.lang.PersistentQueue/EMPTY) (:root system))
         seen #{}
         s system]
    (if-let [id (peek queue)]
      (let [children (get-in s [:children id])]
        (recur (into (pop queue) (remove seen children))
               (conj seen id)
               (run-system-step s id children)))
      s)))

(defn run-system
  "Runs the system until all programs halt or yield."
  [s]
  (let [s' (run-system-pass s)]
    (if (= s s')
      s'
      (recur s'))))



(comment
  ;; testing it out
  (binding [*debug* :system]
  (let [doubler (program [3 12 102 2 12 12 4 12 1105 1 0 99 0])]
    (-> (system [stdin doubler (fork doubler) stdout])
        (update-program "stdin" send-input [10 nil])
        (run-system)
        (get-program "stdout")
        (read-output))))
  )
