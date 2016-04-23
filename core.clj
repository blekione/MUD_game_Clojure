(ns fun-pro.core)
(require ['clojure.string :as 'string])

(def descriptions { :1 '(you are in the living room)
                    :2 '(you are in the hallway)
                   })

(def directions { :1 {:north :2, :south :0, :east :0, :west :0}
                  :2 {:north :0, :south :1, :east :0, :west :0}})


;; check which direction are available
(defn lookup [room-id direction]
 (get (get directions room-id ) direction))


;; function to start game
(defn startgame [room-id]
  (loop [rid room-id]
    (println (clojure.string/join " "(get descriptions rid)))
    (let [input (keyword (read-line))]
      (cond
       (= input :quit) "bye bye"
       (some #(= input %) '(:north :south :east :west) )
       (let [direction (lookup rid input)]
         (if (= :0 direction)
           (recur rid)
           (recur direction)))
       :else (do
               (print "uh! I don't understand. Stil, ")
               (recur rid)))      
      )))

;; simple command-line processor
(defn command []
  (loop []
     (let [input (read-line)
           string-tokens (string/split input #" ")
           tokens (map keyword string-tokens)
           cmd (first tokens)
           reply (case cmd
                   :help "Usage:\nsearch <term>\nquit"
                   :quit "bye bye"
                   :search (format "Searching for %s..." (string/join " " (rest string-tokens)))
                   "Huh?")]
       (println reply)
       (when-not (= :quit cmd)
         (recur)))))

;; advanced command-line processor

(def responses
  {:1 "What type of films do you like?"
   :2 "So you like gore?"
   :7 "Shall I recommend a gory film for you?"
   :8 "Shall I recommend a non-gory scary film for you?"})

(def decisiontable
  {:1 {"comedy" :20
       "very scary" :2
       "thrillers" :6
       "not animated" :5
       "horror" :2
       "scifi" :4}
   :2 {"some" :8
       "a lot" :7
       "yes" :7
       "no" :8
       "not really" :8}
   :7 {"yes" "gory"
       "ok" "gory"
       "no" :0}
   :8 {"yes" "non-gory"
       "ok" "gory"
       "no" :0}})

(defn get-response [id]
  (get responses id))

;; returns list of keywords from decisiontable "attached" to id 
(defn get-keywords [id]
  (let [keys (get decisiontable id)]
    (map (fn [key] (first key)) keys)))

;; outputs a list in the form: (0 0 0 2 0 0)
(defn list-of-lengths [keylist tokens]
  (map
   (fn [x]
     (let [set-value (conj #{} x)
           tokens (into #{} tokens)
           set (clojure.set/intersection tokens set-value)]
       (* (/ (count set) (count set-value)) (count set)))
     )
  keylist))

;; returns the index of element which match the pred
(defn indices [pred coll]
   (keep-indexed #(when (pred %2) %1) coll))

;; returns index of the largest number from list
(defn index-of-largest-number [list-of-numbers]
  (let [n (last (sort list-of-numbers))]
    (if (= 0 n)
      false
      (first (indices (fn [x] (= x n)) list-of-numbers)))))


(defn lookup-tok [id tokens]
  (let [record (get decisiontable id)
        keylist (get-keywords id)
        index (index-of-largest-number (list-of-lengths keylist tokens))]
    (if (not= index false)
      (second (nth record index))
      false)))


;; simplified in Clojure compared to Lisp solution translated into CLojure from above
(defn lookup-clojure [id tokens]
  (let [set-value (into #{} (get-keywords id))
           tokens (into #{} tokens)
           set (clojure.set/intersection tokens set-value)]
    (if (> (count set) 1)
      false 
      (get (get decisiontable id) (first set)))))

(defn recommend [initial-id]
  (loop [id initial-id]
    (println (format "%s\n> " (get-response id)))    
    (let [input (read-line)
          tokens (string/split input #" ")
          response (lookup-clojure id tokens)]
      (case response
        false "You confused me. Can you be more specific"
        :0 "bye bye"
        (recur response))))) ;; default
