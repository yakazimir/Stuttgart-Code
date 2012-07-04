;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FINDING NETWORK COMMUNITIES  ;;
;; KYLE RICHARDSON              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use '[clojure.java.io :only (reader)])
(use '[clojure.string :only (split)])
;(use '[java.lang.System :only (exit)])
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
        cor (= (set v) (set objects))]
    (case cor
      false (do (prn "match issue")
                (java.lang.System/exit 0))
      true (do (println v)))
  (println v)))
(defn readF [f]
  (def x (line-seq (reader f)))
  (let [atr (getAO x attr?)
        objects (getAO x object?)
        objV (calcV (filter objectV? x))]
    (do
      (compileGraph atr objects objV))))
(readF "example.txt")



  
;; (defn extrP [l]
;;   (doseq [p l]
;;     (let [sl (split p #"--")
;;           keyi (first sl)
;;           vali (split (first (rest sl)) #" ")
;;           mapi (hash-map keyi vali)]
;;       mapi)))



      ;(doto mapi
       ; (println)))))
          



;(println
; (extrP
;  (filter objectV? '("this is a test" "a--b c d s" "g--h r e" "e--D e w q"))))



;; (defn createM [w]
;;   (let [keyi (first w)
;;         valuei (first (rest w))]
;;     ([valuei])))
  ;; (defn exp [w]
  ;;   (let [keyi (first w)]
  ;;     (keyi)))
          ;; value (first (rest h))
      ;;     mapt #(hash-map (keyi) (value))]
      ;; (mapt)))
        
    ;;     h (map #(hash-map
    ;;              (first %)
    ;;              (split (first (rest %)) #" ")) g)]
    ;; (doto h
    ;;   (println))))

;; (defn readF [f]
;;   (def x (line-seq (reader f))) 
;;   (let [atr (split (nth (split (first (filter attr? x)) #":") 1) #" ")
;;         objects (split (nth (split (first (filter object? x)) #":") 1) #" ")
;;         objV (map #(hash-map (first %)(split (first (rest %))#" "))(map #(split % #"--") (filter objectV? x)))]
;;     (println [atr, objects, objV])))

;(readF "example.txt")

    ;;     h (map
    ;;        #(hash-map
    ;;          (first %)
    ;;          (split(first (rest %))#" ")) g)]
    ;; (doto h (println))))





















;; (defn atr? [d] #(re-find #"^\#+" %))

;; (defn readF [f]
;;   (def x (line-seq (reader f)))
;;   (let [attr (filter #(re-find #"attributes:" %) x)
;;         objects (filter #(re-find #"objects:" %) x)
;;         objV (filter #(re-find #"^\w+\-\-" %) x)]
;;     (doto [objects,attr, objV]
;;      (println))
;;     (doto objV
;;       (println))
;;     [attr,objects,objV]))

;; (def to (readF "example.txt"))
;(println to)

    ;; (doto objects (println))
    ;; (doto objV (println))))
;(readF "example.txt")
;(println attr)))
    ;(doto attr (println attr))))
    ;(doto objects()))) 
    ;; (filter even? [1 2 3 4 5 6])
;; 2
;; (2 4 6)
;; (defn readF [f]
;;   (def x (line-seq (reader f)))
;;   (let [attr (= x)
;;         objects ()
;;         graph ()]
;;   ;;   (doto total
;;   ;;     (println total))
;;   ;;   (doto y
;;   ;;     ())
;;   ;; (println x))
;; (defn game []
;;   (let [snake (ref (create-snake))
;;       apple (ref (create-apple))
;;       frame (JFrame. "Snake")
;;       panel (game-panel frame snake apple)
;;         timer (Timer. turn-millis panel)]
;;     (doto panel
;;       (.setFocusable true)
;;       (.addKeyListener panel))
;;     (doto frame
;;       (.add panel)
;;       (.pack)
;;       (.setVisible true))
;;     (.start timer)
;;     [snake, apple, timer]))

;; ;(def comm? [line] (if (re.find #"^\#+" line)))
;; (defn readF [fileN]
;;   (doseq [line (line-seq (reader "inFile.txt"))]
;;     (if (not (re-find #"^\#+" line))
;;       (println line)))
;;   (doseq [i (range 1 11)] (println i)))
;; (readF "inFile.txt")
;; (with-open [rdr (reader "inFile.txt")]
;;   (doseq [line (line-seq rdr)]
;;     (println line)))
;; (defn date [person-1 person-2 & others]
;;   (println person-1 "and" person-2 "went out with" (count others) "people"))
;; (date "romeo" "juliet" "john")
;; (date "romeo" "juliet")
;; (defn indexed [coll] (map vector (iterate inc 0) coll))
;; (defn index-filter [pred coll]
;;   (when pred
;;     (for [[idx elt] (indexed coll) :when (pred elt)] idx)))
;; (println (index-filter #{\a \b} "abcabcddae"))
;; (defn add-to-cache [dict key1 value]
;;   (assoc-in dict [key1] value))
;; (def y (-> (add-to-cache {} :chicago '(:lakeview))
;;            (add-to-cache :gh :dan)
;;            (add-to-cache :chicago '(:lake))))
;; (println y)


