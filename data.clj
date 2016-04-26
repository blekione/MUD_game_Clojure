(ns fun-pro.data)

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
