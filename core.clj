(ns fun-pro.core
  (require [fun-pro.data :refer :all])
  (require [fun-pro.objects :refer :all]))
(require ['clojure.string :as 'string])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn check-if-direction [value]
  (some #(= value %) '(:north :south :east :west)))
(defn get-directions [id]
  (let [record (get final-decision-table id)
        result (filter #(check-if-direction (first %)) record)
        n (count result)]
    (case n
      0 (println "You appear to have entered a room with no exits.>")
      1 (println (format "You can see an exit to the %s>" (name (first (first result)))))
      (let [lst-of-keywords (map first result)
            lst-of-strings (map name lst-of-keywords)]
        (println (str "You can see exits to the " (string/join " and " lst-of-strings) ">"))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; returns list of keywords from decisiontable "attached" to id 
(defn get-keywords [id]
  (let [keys (get final-decision-table id)]
    (map (fn [key] (first key)) keys)))

;; lookup if decision-table contains any tokens and returns associated value
(defn lookup-clojure [id tokens]
  (let [set-value (into #{} (get-keywords id))
           tokens (into #{} (map keyword tokens))
           set (clojure.set/intersection tokens set-value)]
    (cond 
      (or (empty? set) (> (count set) 1)) :false  
      :else (get (get final-decision-table id) (first set)))))

(defn String->Number [str]
  (let [n (read-string str)]
       (if (number? n) n nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn startgame-1 [initial-id]
  (let [object-dbs (create-object-db descriptions)] ;; collection of db for objects in rooms and inventory
    (loop [x (rest object-dbs)] 
      (if (seq x) 
        (do
          (add-items (first x))
          (recur (rest x)))))
    (loop [id initial-id description true]
      (if description
        (do
          (println (str (clojure.string/join " "(get descriptions id)) "> ") )
          (println (display-items (nth object-dbs (Integer/parseInt (name id))))))
        )
      (let [input (read-line)
            tokens (string/split input #" ")
            response (lookup-clojure id tokens)
            reply (cond
                   (= response :false)
                     "I dont understand>"            
                   (= response :look)
                     (get-directions id) 
                   (= response :quit)
                     "So Long, and Thanks for All the Fish...>"
                   (= response :pick)
                     (move-item (nth object-dbs (Integer/parseInt (name id))) (first object-dbs) input))]
        (if (not= reply nil)
          (println reply))
        (cond 
            (number? (String->Number (name response)))
              (recur response true)
            (not= response :quit)
              (recur id false))
        )))  
  )
