(ns fun-pro.data)

(def descriptions {:1 '(the old church)
                   :2 '(the dungeon)
                   :3 '(the cemetery)
                   :4 '(the dark garden)
                   :5 '(the dungeon)
                   :6 '(a swamp)
                   :7 '(the haunting house)
                   :8 '(the old gold mine)
                   })

(def look {:directions :look
           :look :look
           :examine :look
           })

(def quit {:exit :quit
           :quit :quit
           })
 
(def pick {:get :pick
           :pickup :pick
           :pick :pick})

(def put {:put :drop
          :drop :drop
          :place :drop
          :remove :drop})

(def inventory {:inventory :inventory
                :bag :inventory})

(def room-info {:description :room-info
                :info :room-info
                :lost :room-info})

(def decisiontable {:1 {:north :2, :west :3}
                    :2 {:south :1, :north :8}
                    :3 {:east :1, :north :4}
                    :4 {:south :3, :west :5}
                    :5 {:east :4, :north :6}
                    :6 {:east :7}
                    :7 {:west :6, :east :8}
                    :8 {:west :7, :south :2}})

(def actions (merge look quit pick put inventory room-info))

;; merge actions into every record of decisiontable - result is list of maps with single key-value pair
(def merged-tables
  (map #(merge (assoc {} (first %) (merge (second %) actions))) decisiontable))

;; converting list of maps with single key-value pair into single map
(defn list-to-map [input-lst]
  (zipmap (map #(first (first %)) input-lst) (map #(second (first %)) input-lst)))

(def final-decision-table
  (list-to-map merged-tables))


