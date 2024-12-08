(ns day2.day2)
(require '[clojure.string :as str])

; one big condition to see if the levels change is unsafe
(defn are_levels_unsafe [a b is_inc] 
  ; is this good style for larger conditional statements :sob:
  (or 
        (= 
          ; check if b is greater than a
          (> (- b a) 0) 
          is_inc
        ) 
        (or 
          (< (Math/abs (- a b)) 1) 
          (> (Math/abs (- a b)) 3) 
        )
  )
)

; return 1 for safe, 0 for unsafe
(defn is_report_safe [report]
  ;(println "is increment" (< (nth report 0) (nth report 1)))
  (let [is_inc (< (nth report 0) (nth report 1))  
        safe (reduce 
         (fn [acc [index value]] 
           ; have the current value, get the one previous
           (if (not= index 0) 
              (if (are_levels_unsafe value (nth report (dec index)) is_inc) 
                (do (println "unsafe" value (nth report (dec index))) (reduced false)) (println "safe" value (nth report (dec index)) ))
               ()
         )) nil (map-indexed vector report) )]
    (if (nil? safe) 
      1 
      0
    )
))


; return 1 for safe, 0 for unsafe
(defn is_report_safe_part_2 [report]
  ;(println "report" report)
  ;(println "is increment" (< (nth report 0) (nth report 1)))
  (let [is_inc (< (nth report 0) (nth report 1))  
        safe (reduce 
         (fn [acc [index value]] 
           ; have the current value, get the one previous
           (if (not= index 0) 
              (if (are_levels_unsafe value (nth report (dec index)) is_inc) 
                (do 
               ;   (println "unsafe" value (nth report (dec index))) 
                  (reduced [0 index (dec index)])
                ) 
                (
    ;             println "safe" value (nth report (dec index)) 
                )
              )
           ()
         )) nil (map-indexed vector report) )]
    (if (or (nil? safe ) (empty? safe)) 
      (do 
        ;(println "safe is nil" safe)
        [1]
       ) 
      (do
      ;(println "safe" safe)
      safe
      )
    )
))

(defn remove-index [arr index] 
  (concat (take index arr) (drop (inc index) arr))
)

(defn part_2_wrapper [report]
  (let [res (is_report_safe_part_2 report)]
  (if (= (get res 0) 0) 
    (reduce (fn [_ index] 
            (let [res (is_report_safe_part_2 (remove-index report index))]
            (if (= 1 (get res 0))
              (reduced 1)
              0
             ))
          ) 0 (range (count report))) 
  1
  )

))

(defn part_1 [input] 
  ;(println "input" input)
  ; for each report check if its safe. The value returned 
  ; will be the number of safe reports
  (reduce + (map part_2_wrapper input))
  ;(is_report_safe [50 49 47 46 48])
)

(defn -main [] 
(let [input (map 
      (fn [a] 
          (map (fn [b] (Integer/parseInt b)) (str/split a #" ")) 
      ) 
      (str/split (slurp "src/day2/input.txt") #"\n") 
      )]
(println "number of safe reports" (part_1 input))
)
)
