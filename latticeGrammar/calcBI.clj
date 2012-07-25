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
        gh (mergeMatches
            (reduce concat hh))]
    gh))

(defn readF [f]
  (def x (line-seq (reader f)))
  (let [atr (getAO x attr?)
        objects (getAO x object?)
        objF (calcV (filter objectV? x))
        objectM (into {} objF)
        attributeM (buildRev objectM)]
    ;(future (doubleCheck atr objects objF))
    [atr objects objectM attributeM]))
 

(print "reading input file: ")
(flush)
(def graphD (time (readF "newcljtest.txt")))
;(def graphD (time (readF "newestCLJTEST.txt")))
;(def graphD (time (readF "largerCLJ.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FINDING MAXIMAL BICLIQUES ;;
;;---------------------------;;

(def S (ref (set (for [i (last graphD)]
                 (set (last i))))))
(def Q (ref (into PersistentQueue/EMPTY @S)))
(def subGraphs (ref #{})) 

(defn outside [l t]
  (defn gg [j]
    (let [ma (nth graphD j)
          i (map #(set (get ma %)) l)]
      (if-not (= i '()) i '(#{}))))
  (def gg-memo (memoize gg))
  (case t "ext" (gg-memo 3) "int" (gg-memo 2)))

(defn forS [t s]
  (defn forSN [t s] 
    (let [xV (outside s "ext")
          SN (set (map #(intersection % t) xV))]
      SN)) 
  (def forSN-memo (memoize forSN))
  (forSN-memo t s))

(def switch (atom true))
(defn computeMaxBI []
    (let [top (peek @Q)
          diff (difference
                (set (first graphD))
                (reduce intersection
                        (outside top "int")))]
      (do (dosync (alter Q pop))
          (def D (difference (forS top diff) @S))
          (dosync (alter S union D))
          (doall (for [i D]
                   ;(dosync (alter Q conj i))))))
                   (and (dosync (alter Q conj i))
                        (println i))))))
    (if (empty? @Q)
      (reset! switch false)))

;; (print "finding maximal bicliques: ")
;; (flush)
;; (time (while @switch
;;         (computeMaxBI)))
;; (print "total bicliques: ")
;; (println (count @S))

;(shutdown-agents)
;this gets an extra biclique 
;; (time (while @switch
;;         (let [workers (repeatedly
;;                        2 #(future (computeMaxBI)
;;                                    (Thread/sleep 0.005)))]
;;           (doseq [worker workers] @worker))))
;; (shutdown-agents)
;; (println (count @S))
;(print "total found: ")
;(println (count @S))
;(doall (for [i @S] (println i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FINDING NETWORK COMMUNITIES ;;
;;-----------------------------;;


(def attributeVals {"a" '(1 2) "b" '(1 2) "c" '(1 2)
                    "d" '(3) "e" '(2 3) "f" '(3 4 5 6 7)
                    "g" '(5 6) "h" '(9 10) "i" '(10 11)})
(def objectVals {1 '("a" "b" "c") 2 '("a" "b" "c" "e")
                 3 '("d" "e" "f") 4 '("f") 5 '("f" "g")
                 6 '("g" "f") 7 '("f") 9 '("h")
                 10 '("h" "i") 11 '("i")})

(def total (merge attributeVals objectVals))
(def subG (ref #{}))

(defn breadth-f [start]
  (def gQ (ref PersistentQueue/EMPTY)) 
  (def marked (ref #{start}))
  (dosync (alter gQ conj start))
  (while (seq @gQ)
    (let [top (peek @gQ)]
      (dosync (alter gQ pop))
      (doseq [i (get total top)]
        (if-not (contains? @marked i)
          (dosync (alter marked conj i)
                  (alter gQ conj i))))))
  (dosync
   (alter subG conj @marked)))

(defn findA [l]
  (let [f (map #(future (breadth-f %)) l)]
    (doseq [i f] @i)))

(def vertex '(1 2 3 4 5 6 7 9 10 11))
(time (findA vertex))
(println @subG)
(shutdown-agents)


