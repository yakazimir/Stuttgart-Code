;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FINDING NETWORK COMMUNITIES  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use '[clojure.java.io :only (reader)])
(use '[clojure.string :only (split)])
(use '[clojure.set])
(import '[clojure.lang PersistentQueue])

;;;;;;;;;;;;;;;;;;;;;;;;
;; READING INPUT FILE ;;
;----------------------;

(defn re-match? [re s] (not (nil? (re-find re s))))
(defn attr? [s] (re-match? #"^attributes:" s)) 
(defn object? [s] (re-match? #"^objects:" s))
;(defn objectV? [s] (re-match? #"^\w+\-\-" s))
(defn objectV? [s] (re-match? #"^[\w+\s*]+\-\-" s))
(defn symD [s1 s2] (union (difference s1 s2)
                          (difference s2 s1)))

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
    ;(println atr objects objectM attributeM)
    [atr objects objectM attributeM]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FINDING MAXIMAL BICLIQUES ;;
;;---------------------------;;

(flush)
(print "reading input file: ")
;(def graphD (time (readF "synConcepts.txt")))
;(def graphD (time (readF "example.txt")))
(def graphD (time (readF "newcljtest.txt")))

(defn outside [l t]
  (defn gg [j]
  (let [ma (nth graphD j)
        i (map #(set (get ma %)) l)] i))
  (case t "ext" (gg 3) "int" (gg 2)))

(defn forS [t s]
  (try 
    (let [xV (outside s "ext") 
          SN (set (map #(intersection % t) xV))] SN)
    (catch Exception e
      (println "caught"))))

(defn computeMaxBI [graphs]
  (def S (ref (set (for [i (last graphs)]
                    (set (last i))))))
  (def Q (ref (into PersistentQueue/EMPTY @S)))
  (defn CFun [] 
    (let [top (peek @Q)
          diff (difference
                (set (first graphs))
                (try (reduce intersection
                             (outside top "int"))
                     (catch Exception e ())))]
      (do (dosync (alter Q pop))
          (def D (difference (forS top diff) @S))
          (dosync (alter S union D))
          (doall (for [i D] (dosync (alter Q conj i)))))))
  
  (while (not (empty? @Q)) (CFun))
  
  (doall (for [i @S] (println i)))
  (print "total bicliques found: ")
  (println (count @S)))

(time (computeMaxBI graphD))



;)
    ;; (for [i (difference (forS top diff) @S)] 
    ;;   )
    ;(println (difference (forS top diff) @S)) 
    ;; (println @S)
    ;; (println top)
    ;; (println diff)
    ;; (println (forS top diff ))))


;; (defn gg [j]
;;     (let [ma (nth graphD j)
;;           i (map #(set (get ma %)) l)]
;;       (reduce (intersection i))))

;; (defn findInt [l]
;;   (let [ma (nth graphD 2)
;;         i (map #(set (get ma %)) l)]
;;     (reduce intersection i)))



;(findInt top))
 
    ;(println diff)
    ;; (set (map #(sssssssss)
    ;;           (difference
    ;;            (findInt top)
    ;;            (set (first graphs)))))
     
    ;(println (first graphs))  
    ;(println top)
    ;(println (findInt top))



    ;(for [i (findInt peek)]

  ;(println (peek @Q))
  ;(dosync (alter Q pop))
  ;(println (peek @Q))
   ; (println top)
   ; (println (peek @Q))
  ;)

 ;(def S (ref (set (map #(set %) (vals(last graphs))))))

 ;; S (ref (set (for [i (last graphs)]
 ;;   ;                  (set (last i))))))

 ;(while (not (empty? @Q)) 
  ;(println "A"))

  ;; (while (not (nil? (peek @Q)))
  ;;   (println "a")
  ;;   )
  
  ;; (println (empty? @Q))
  ;; (println (nil? (peek @Q)))
  
  ;; (println S)
  ;; (println (peek @Q))
  ;; (println (peek (pop @Q)))

  ;; (dosync (ref-set S #{"first","testing"}))
  ;; (println S)
  ;; (println (peek @Q))
  ;; (println (peek (pop @Q)))

;(println (not (nil? (peek @Q)))) 
  ;; (println @S))
;(time (computeMaxBI graphD))
  ;; (ref (-> clojure.lang.PersistentQueue/EMPTY
    ;;          (conj (doseq [i (list S)] i)))))
             ;(conj (for [i (list S)] i)))))
;;this S also needs to be immutable 
;(println S)
;(println (last graphD))
;(println (nth graphD 2))
;(def S #{(for [i (last graphD)] (last i))})


  ;;something about set here !!
  ;(println #(first (list %)) @S)
  
  ;; (println (list @S))
  ;; (dosync (alter S conj "this is a test"))
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