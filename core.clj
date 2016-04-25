(ns fun-pro.core)
(require ['clojure.string :as 'string])


(def descriptions {:1 '(You are in the living room)
                   :2 '(You are in the hallway)
                   :3 '(You are in a swamp)
                   })


(def look {:directions :look
           :look :look
           :examine :look
           })

(def quit {:exit :quit
           :quit :quit
           })

(def decisiontable {:1 {:north :2, :west :3}
                    :2 {:south :1}
                    :3 {}})

(def actions (merge look quit))

;; merge actions into every record of decisiontable - result is list of maps with single key-value pair
(def merged-tables
  (map #(merge (assoc {} (first %) (merge (second %) actions))) decisiontable))

;; converting list of maps with single key-value pair into single map
(defn list-to-map [input-lst]
  (zipmap (map #(first (first %)) input-lst) (map #(second (first %)) input-lst)))

(def final-decision-table
  (list-to-map merged-tables))

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

(defn startgame-1 [initial-id]
  (loop [id initial-id description true]
    (if description
      (println (str (clojure.string/join " "(get descriptions id)) "> ") ))
      (let [input (read-line)
            tokens (string/split input #" ")
            response (lookup-clojure id tokens)
            reply (cond
                   (= response :false)
                     "I dont understand>"            
                   (= response :look)
                     (get-directions id) 
                   (= response :quit)
                     "So Long, and Thanks for All the Fish...>")]
        (if (not= reply nil)
          (println reply))
        (cond 
            (number? (String->Number (name response)))
              (recur response true)
            (not= response :quit)
              (recur id false))
        )))
