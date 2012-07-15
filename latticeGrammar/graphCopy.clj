;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FINDING NETWORK COMMUNITIES  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use '[clojure.java.io :only (reader)])
(use '[clojure.string :only (split)])

;;;;;;;;;;;;;;;;;;;;;;;;
;; READING INPUT FILE ;;
;----------------------;

(defn re-match? [re s] (not (nil? (re-find re s))))
(defn attr? [s] (re-match? #"^attributes:" s)) 
(defn object? [s] (re-match? #"^objects:" s))
(defn objectV? [s] (re-match? #"^\w+\-\-" s))
(defn in? [seq elem] (some #(= elem %) seq))

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

;sanity check, probably not necessary
(defn doubleCheck [atr objects objV]
  (let [v (map #(first (keys %)) objV)
        zw (map #(first (vals %)) objV)
        zi (reduce concat zw)       
        cor (and (= (set v) (set objects))
                 (= (set zi) (set atr)))]
    (case cor
      false (do (prn "match issue")
                (println [(set v) (set objects)])
                (println [(set zi) (set atr)])
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
    [objectM attributeM]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPUTING COMMUNITIES ;;
;------------------------;;

(flush)
(println "reading input file")
(time (readF "example.txt"))
(time (readF "synConcepts.txt"))





;(time (readF "synConcepts.txt"))

;; (reduce concat (map #(for [i (last %)] (hash-map i (first %))) x))

;; (defn kv [bag [k v]] 
;;   (update-in bag [k] conj v))
;; (defn mergeMatches [propertyMapList]
;;   (reduce #(reduce kv %1 %2) {} propertyMapList))