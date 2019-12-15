(ns advent-2019.day14
  "Space Stoichiometry"
  (:require [advent.util :as util]
            [clojure.math.numeric-tower :as math]))

;;; Part 1

; As you approach the rings of Saturn, your ship's low fuel indicator turns on.
; There isn't any fuel here, but the rings have plenty of raw material. Perhaps
; your ship's Inter-Stellar Refinery Union brand nanofactory can turn these raw
; materials into fuel.

; You ask the nanofactory to produce a list of the reactions it can perform
; that are relevant to this process (your puzzle input). Every reaction turns
; some quantities of specific input chemicals into some quantity of an output
; chemical. Almost every chemical is produced by exactly one reaction; the only
; exception, ORE, is the raw material input to the entire process and is not
; produced by a reaction.

; You just need to know how much ORE you'll need to collect before you can
; produce one unit of FUEL.

; Each reaction gives specific quantities for its inputs and output; reactions
; cannot be partially run, so only whole integer multiples of these quantities
; can be used. (It's okay to have leftover chemicals when you're done, though.)
; For example, the reaction 1 A, 2 B, 3 C => 2 D means that exactly 2 units of
; chemical D can be produced by consuming exactly 1 A, 2 B and 3 C. You can run
; the full reaction as many times as necessary; for example, you could produce
; 10 D by consuming 5 A, 10 B, and 15 C.

; Suppose your nanofactory produces the following list of reactions:

; 10 ORE => 10 A
; 1 ORE => 1 B
; 7 A, 1 B => 1 C
; 7 A, 1 C => 1 D
; 7 A, 1 D => 1 E
; 7 A, 1 E => 1 FUEL

; The first two reactions use only ORE as inputs; they indicate that you can
; produce as much of chemical A as you want (in increments of 10 units, each 10
; costing 10 ORE) and as much of chemical B as you want (each costing 1 ORE).
; To produce 1 FUEL, a total of 31 ORE is required: 1 ORE to produce 1 B, then
; 30 more ORE to produce the 7 + 7 + 7 + 7 = 28 A (with 2 extra A wasted)
; required in the reactions to convert the B into C, C into D, D into E, and
; finally E into FUEL. (30 A is produced because its reaction requires that it
; is created in increments of 10.)

; Or, suppose you have the following list of reactions:

; 9 ORE => 2 A
; 8 ORE => 3 B
; 7 ORE => 5 C
; 3 A, 4 B => 1 AB
; 5 B, 7 C => 1 BC
; 4 C, 1 A => 1 CA
; 2 AB, 3 BC, 4 CA => 1 FUEL

; The above list of reactions requires 165 ORE to produce 1 FUEL:

;   - Consume 45 ORE to produce 10 A.
;   - Consume 64 ORE to produce 24 B.
;   - Consume 56 ORE to produce 40 C.
;   - Consume 6 A, 8 B to produce 2 AB.
;   - Consume 15 B, 21 C to produce 3 BC.
;   - Consume 16 C, 4 A to produce 4 CA.
;   - Consume 2 AB, 3 BC, 4 CA to produce 1 FUEL.

; Here are some larger examples:

;   - 13312 ORE for 1 FUEL:

;     157 ORE => 5 NZVS
;     165 ORE => 6 DCFZ
;     44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
;     12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
;     179 ORE => 7 PSHF
;     177 ORE => 5 HKGWZ
;     7 DCFZ, 7 PSHF => 2 XJWVT
;     165 ORE => 2 GPVTF
;     3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT

;   - 180697 ORE for 1 FUEL:

;     2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
;     17 NVRVD, 3 JNWZP => 8 VPVL
;     53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
;     22 VJHF, 37 MNCFX => 5 FWMGM
;     139 ORE => 4 NVRVD
;     144 ORE => 7 JNWZP
;     5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
;     5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
;     145 ORE => 6 MNCFX
;     1 NVRVD => 8 CXFTF
;     1 VJHF, 6 MNCFX => 4 RFSQX
;     176 ORE => 6 VJHF

;   - 2210736 ORE for 1 FUEL:

;     171 ORE => 8 CNZTR
;     7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
;     114 ORE => 4 BHXH
;     14 VRPVC => 6 BMBT
;     6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
;     6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
;     15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
;     13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
;     5 BMBT => 4 WPTQ
;     189 ORE => 9 KTJDG
;     1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
;     12 VRPVC, 27 CNZTR => 2 XDBXC
;     15 KTJDG, 12 BHXH => 5 XCVML
;     3 BHXH, 2 VRPVC => 7 MZWV
;     121 ORE => 7 VRPVC
;     7 XCVML => 6 RJRHP
;     5 BHXH, 4 VRPVC => 5 LTCX

; Given the list of reactions in your puzzle input, what is the minimum amount
; of ORE required to produce exactly 1 FUEL?

(defn parse-reaction-line [line]
  (let [parts (for [[_ amount id] (re-seq #"(\d+) ([A-Z]+)" line)]
                {:amount (Integer/parseInt amount)
                 :chemical id})]
    {:inputs (set (butlast parts))
     :output (last parts)}))

(defn parse-reactions [f]
  (->> (util/lines f)
       (map parse-reaction-line)
       (filter :output)
       (reduce (fn [m {:keys [inputs output]}]
                 (assoc m (:chemical output)
                        {:inputs inputs
                         :output output}))
               {})))

(defn chemicals-needed [store]
  (->> store
       (remove (comp #{"FUEL" "ORE"} key))
       (map (fn [[chemical {:keys [produced consumed]}]]
              [chemical (- (or consumed 0) (or produced 0))]))
       (filter (comp pos? second))))

(defn produce [store {:keys [chemical amount]} n]
  (update-in store [chemical :produced] (fnil + 0) (* n amount)))

(defn consume [store {:keys [chemical amount]} n]
  (update-in store [chemical :consumed] (fnil + 0) (* n amount)))

(defn react [store {:keys [inputs output]} n]
  (as-> store $
    (produce $ output n)
    (reduce #(consume %1 %2 n) $ inputs)))

(defn produce-chemical [state chemical amount]
  (let [reaction (get-in state [:reactions chemical])
        times (math/ceil (/ amount (get-in reaction [:output :amount])))]
    (update state :store react reaction times)))

(defn run-machine [{:keys [store] :as state}]
  (if-let [[chemical amount] (first (chemicals-needed store))]
    (recur (produce-chemical state chemical amount))
    state))

(defn machine [reactions]
  {:reactions reactions, :store {}})

(defn part1 [f]
  (-> (machine (parse-reactions f))
      (produce-chemical "FUEL" 1)
      (run-machine)
      (get-in [:store "ORE" :consumed])))

#_ (time (util/run part1))

;;; Part 2

; After collecting ORE for a while, you check your cargo hold: 1 trillion
; (1000000000000) units of ORE.

; With that much ore, given the examples above:

;   - The 13312 ORE-per-FUEL example could produce 82892753 FUEL.
;   - The 180697 ORE-per-FUEL example could produce 5586022 FUEL.
;   - The 2210736 ORE-per-FUEL example could produce 460664 FUEL.

; Given 1 trillion ORE, what is the maximum amount of FUEL you can produce?

(defn run-until-target-ore
  [machine target-ore]
  (loop [;; calibrate the machine first by producing a single fuel
         machine (-> machine
                     (produce-chemical "FUEL" 1)
                     (run-machine))]
    (let [;; determine the current ore : fuel rate
          ore (get-in machine [:store "ORE" :consumed] 0)
          fuel (get-in machine [:store "FUEL" :produced] 0)
          rate (/ ore fuel)
          ;; produce an amount of fuel that should get us close to the target
          amount (int (/ (- target-ore ore) rate))
          machine' (-> machine
                       (produce-chemical "FUEL" (max 1 amount))
                       (run-machine))]
      (if (< target-ore (get-in machine' [:store "ORE" :consumed]))
        machine ;; we're one past the end, so return the previous machine
        (recur machine')))))

(defn part2 [f]
  (let [target-ore 1000000000000
        machine (machine (parse-reactions f))
        machine (run-until-target-ore machine target-ore)]
    (get-in machine [:store "FUEL" :produced])))

#_ (time (util/run part2))
