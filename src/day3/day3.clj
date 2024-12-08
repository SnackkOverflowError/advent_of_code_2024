(ns day3.day3 
  (:require
    [clojure.string :as str]))

(defn isolate_commands [input] 
  (re-seq #"mul\(\d{1,3},\d{1,3}\)|do(?:n't)?\(\)" input)
)

(defn process_mul [input] 
(let [open_para (str/index-of input "(")
      closed_para (str/index-of input ")")
      cleaned_input (subs input (inc open_para) closed_para)
      values (str/split cleaned_input #",")
     ]
  (* (Integer/parseInt (get values 0)) (Integer/parseInt (get values 1)))
))

(defn process_com [commands] 
  (println "commands" commands)
  (def enabled true)
  (map (fn [value] 
     (println "command" value)
     (case value
       "do()" (do (def enabled true) 0)
       "don't()" (do (def enabled false) 0)
       (if enabled  
         (process_mul value) 
         0) ; default case should be mul()          
     )) 
     commands
  )
)


(defn part_1 [input] 
  ; isolate the muls with regex
  ; process each mul with a map reduce
  (reduce + 
          (process_com (isolate_commands input)))
)

(defn -main [] 
      (let [input (slurp "src/day3/input.txt")]
      (println "part 1" (part_1 input)) 
))
