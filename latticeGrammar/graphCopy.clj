;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FINDING NETWORK COMMUNITIES  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use '[clojure.java.io :only (reader)])
(use '[clojure.string :only (split)])
(use '[clojure.set])
;(use '[clojure.lang.PersistentQueue/EMPTY :as (PersistentQueue)])

;;;;;;;;;;;;;;;;;;;;;;;;
;; READING INPUT FILE ;;
;----------------------;

(defn re-match? [re s] (not (nil? (re-find re s))))
(defn attr? [s] (re-match? #"^attributes:" s)) 
(defn object? [s] (re-match? #"^objects:" s))
(defn objectV? [s] (re-match? #"^\w+\-\-" s))
(defn symD [s1 s2]
  (union (difference s1 s2) (difference s2 s1)))

(defn calcV[l]
  (defn extrE [p]
    (let [sl (split p #"--")
          keyi (first sl)
          vali (split (first (rest sl)) #";")
          mapi (hash-map keyi vali)] mapi))
  (try 
    (let [remap (map #(extrE %) l)] remap)
    (catch Exception e
      (prn "val format issue")
      (java.lang.System/exit 0))))

(defn getAO [l pred]
  (try 
    (let [v (split (first (filter pred l))#":")
          g (split (nth v 1) #";")] g)
    (catch Exception e
      (prn "object/attr err")
      (java.lang.System/exit 0))))

(defn doubleCheck [atr objects objV]
  "Sanity check; probably not needed in the end"
  (let [v (map #(first (keys %)) objV)
        zw (map #(first (vals %)) objV)
        zi (reduce concat zw)       
        cor (and (= (set v) (set objects))
                 (= (set zi) (set atr)))]
    (case cor
      false (do (prn "match issue")
                (prn (symD (set v) (set objects)))
                (prn (symD (set zi) (set atr)))
                (java.lang.System/exit 0))
      true ())))

(defn buildRev [l]
  (defn kv [bag [k v]]
    (update-in bag [k] conj v))
  (defn mergeMatches [mList]
    (reduce #(reduce kv %1 %2) {} mList))
  (let [hh (map #(for [i (last %)]
                   (hash-map i (first %))) l)
        gh (mergeMatches (reduce concat hh))] gh))

(defn readF [f]
  (def x (line-seq (reader f)))
  (let [atr (getAO x attr?)
        objects (getAO x object?)
        objF (calcV (filter objectV? x))
        objectM (into {} objF)
        attributeM (buildRev objectM)]
    (doubleCheck atr objects objF)
    [atr objects objectM attributeM]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FINDING MAXIMAL BICLIQUES ;;
;;---------------------------;;

(flush)
(print "reading input file: ")

(def graphD (time (readF "example.txt")))

(defn computeMaxBI [graphs]
  (def S (ref (for [i (last graphs)] (set (last i)))))
  (def Q
    (ref (into clojure.lang.PersistentQueue/EMPTY @S)))
  (println S)

  ;;something about set here !!
  ;(println #(first (list %)) @S)
  
  ;; (println (list @S))
  ;; (dosync (alter S conj "this is a test"))
  (println (peek @Q))
  (println (peek (pop @Q))))
  ;; (println @S))
 


(computeMaxBI graphD)

  ;; (ref (-> clojure.lang.PersistentQueue/EMPTY
    ;;          (conj (doseq [i (list S)] i)))))
  
             ;(conj (for [i (list S)] i)))))
;;this S also needs to be immutable 
;(println S)
;(println (last graphD))
;(println (nth graphD 2))
;(def S #{(for [i (last graphD)] (last i))})





;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPUTING COMMUNITIES ;;
;------------------------;;

;; (flush)
;; (print "reading input file: ")
;; (time (readF "example.txt"))
;; (time (readF "synConcepts.txt"))

;(time (readF "synConcepts.txt"))

;; (reduce concat (map #(for [i (last %)] (hash-map i (first %))) x))

;; (defn kv [bag [k v]] 
;;   (update-in bag [k] conj v))
;; (defn mergeMatches [propertyMapList]
;;   (reduce #(reduce kv %1 %2) {} propertyMapList))