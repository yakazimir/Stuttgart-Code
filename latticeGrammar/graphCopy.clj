;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FINDING NETWORK COMMUNITIES  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use '[clojure.java.io :only (reader)])
(use '[clojure.string :only (split)])
(defn re-match? [re s] (not (nil? (re-find re s))))
(defn attr? [s] (re-match? #"^attributes:" s)) 
(defn object? [s] (re-match? #"^objects:" s))
(defn objectV? [s] (re-match? #"^\w+\-\-" s))
(defn calcV[l]
  (defn extrE [p]
    (let [sl (split p #"--")
          keyi (first sl)
          vali (split (first (rest sl)) #" ")
          mapi (hash-map keyi vali)] mapi))
  (try 
    (let [remap (map #(extrE %) l)] remap)
    (catch Exception e
      (prn "val format issue")
      (java.lang.System/exit 0))))
(defn getAO [l pred]
  (try 
    (let [v (split (first (filter pred l))#":")
          g (split (nth v 1) #" ")] g)
    (catch Exception e
      (prn "object/attr err")
      (java.lang.System/exit 0))))
(defn compileGraph [atr objects objV]
  (println [atr objects objV])
  (let [v (map #(first (keys %)) objV)
        zw (map #(first (vals %)) objV)
        zi (reduce concat zw) 
        cor (and (= (set v) (set objects))
                 (= (set zi) (set atr)))]
    (case cor
      false (do (prn "match issue")
                (java.lang.System/exit 0))
      true (do (println v)))
    (println zi)))
(defn readF [f]
  (def x (line-seq (reader f)))
  (let [atr (getAO x attr?)
        objects (getAO x object?)
        objV (calcV (filter objectV? x))]
    (do
      (compileGraph atr objects objV))))

(time (readF "example.txt"))
