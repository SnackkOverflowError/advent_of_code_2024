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
              (if  (are_levels_unsafe value (nth report (dec index)) is_inc) 
                (do (println "unsafe" value (nth report (dec index))) (reduced false)) (do (println "safe" value (nth report (dec index))) ))
               ()
         )) nil (map-indexed vector report) )]
    (if (nil? safe) 
      1 
      0
    )
))

(defn part_1 [input] 
  ;(println "input" input)
  ; for each report check if its safe. The value returned 
  ; will be the number of safe reports
  (reduce + (map is_report_safe input))
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
