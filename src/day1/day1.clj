(ns day1.day1)
(require '[clojure.string :as str])

(defn split_list [i value]
;  (println "tf data")
  (let [split_values (str/split value #" ")]
    (if (= i 0)
      (get split_values 0)
      (get split_values (- (count split_values) 1))
    )
    )
  
)

(defn tfdata [input_name] 
  (let [input (slurp input_name)]

  (let [split_input (str/split input #"\n")]
    ; I know this should really just be a single loop, but im having fun 
    ; learning fp and mapping and getting used to everything
    [
     (map (fn [value] (Integer/parseInt (split_list 0 value))) split_input)
     (map (fn [value] (Integer/parseInt (split_list 1 value))) split_input)
    ]
  )
))

(defn part_1 [] 
  (println "part 1")
  (let [result (tfdata "src/day1/input.txt")]
;    (println "First list" (get result 0))
;    (println "Second list" (get result 1))
    (println "Final Distance"
             (reduce + (map (fn [a, b] (Math/abs (- a b))) (sort (get result 0)) (sort (get result 1)) ))
    )
))

(defn part_2 [] 
  (println "part 2")
  (let [result (tfdata "src/day1/input.txt")]
  ; I know theres a built in for this, but I wanted to try it myself
  (let [freq 
    (reduce 
      (fn [hm item] (update hm item (fnil inc 0))) 
      {} 
      (get result 1)
    )]
    ;(println "frequencies" freq)
    
    (println "similarity score"
      (reduce + (map (fn [value] (* value (get freq value 0)) ) (get result 0) ))
    )
  )

))
(defn -main [] 
  (part_1)
  (part_2)
)

