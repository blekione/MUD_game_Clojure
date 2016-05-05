(ns fun-pro.objects
  (require [fun-pro.data :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; databases for rooms and inventory  and associated functions to work with it

(def game-objects ["a silver dagger"
              "a gold coin"
              "a rusty sword"
              "a viking helmet"
              "a steel long sword"
              "a healing potion"
              "a plate with meat meal (+2 strength)"])

;;;;; function to create mutable references (atom) to vector for each room

(defn create-object-db [input-coll]
  (loop [x 0 my-db '()]
    (if (<= x (count input-coll))
      (recur (+ x 1) (conj my-db (atom [])))
      my-db)))

(def object-dbs (create-object-db descriptions))


;;;;; functions to adds/remove objects to db

(defn find-item [input-str coll]
  (first (filter #(.contains input-str %) game-objects))) ;; use java.lang.String.contains method

(defn add-item [output-db item]
  (if (nil? item)
      false
      (swap! output-db conj item)))

(defn remove-item [input-db item]
  (if (nil? item)
      false
      (swap! input-db #(vec (remove #{item} %)))))

;;;;; adds up to 4 random items from the list to the room-db

(defn add-items [room-db]
  (loop [x (rand-int 4) my-objects game-objects ]
    (when (> x 0)
      (let [random-item (rand-nth my-objects)]
        (add-item room-db random-item)
        (recur (- x 1)
               (remove #{random-item} my-objects)))
     ))
  @room-db) 

;;;;; display objects from database

(defn display-items [db]
  (let [data @db
        output-no-first (clojure.string/join ", " (rest data))
        output (if (> (count data) 1)
                 (str output-no-first " and " (first data))
                 (first data))]
    (when (not-empty output)
      (str output))))

;;;;; moving object between db

(defn move-item [input-db output-db input-str]
  (let [item (find-item input-str input-db)]
    (if (complement (nil? item))
       (do
         (add-item output-db item)
         (remove-item input-db item))
       )))



