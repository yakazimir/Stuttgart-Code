(ns latticeGRAM.core)
(use '[clojure.string :only (split)])
(use '[clojure.java.io :only (reader)])
(use '[clojure.set])
(import '[clojure.lang PersistentQueue])


(def banner  (str "\t*************************\n"
                  "\t***COMMUNITY DETECTION***\n"
                  "\t*************************\n"))
(println)                    
(println banner)

;(def inputFile (line-seq (reader "../../data/practiceFile.txt")))
;(def inputFile (line-seq (reader "../../data/larger.txt")))
(def inputFile (line-seq (reader "../../data/futbolData/output/window3.txt")))

(defn re-match? [re s] (not (nil? (re-find re s))))
(defn objectV? [s] (re-match? #"^[\w+\s*]+\-\-" s))
(def words (ref {}))
(def attr (ref {}))

;---------------------
(defn prog-bar [percent]
  (let [bar (StringBuilder. "[")] 
    (doseq [i (range 50)]
      (cond (< i (int (/ percent 2))) (.append bar "=")
            (= i (int (/ percent 2))) (.append bar ">")
            :else (.append bar " ")))
    (.append bar (str "] " percent "%     "))
    (print "\r" (.toString bar))
    (flush)))
;--------------------
;---READING FILES----
;--------------------
(defn findR [i]
  (doseq [h (split (last i) #";")]
    (let [v (first i)
          new (union (get @attr h) [v])]
      (cond (contains? @attr h)
            (dosync (alter attr merge {h new}))
            :else
            (dosync (alter attr assoc h [v]))))))

(defn readF [f]
  (def iCount1 (ref 1))
  (let [cut (filter objectV? f)
        v1 (map #(split % #"--") cut)
        si (count v1)]
    (doseq [i v1]
      (let [poS (float (/ @iCount1 si))
            uS (future (prog-bar
                        (read-string
                         (format "%.0f" (* 100 poS)))))]
        (def rev (future (findR i)))
        (dosync (alter words assoc
                       (first i)
                       (split (last i) #";")))
        (deref uS)
        (deref rev)
        (dosync (alter iCount1 inc))))
    (println)))

(print "reading input file ...")
(println)
(flush)
(time (readF inputFile))
(flush)
(println)
;------------------
;--BREADTH FIRST---
;------------------
(def total2 (merge @words @attr))
(def subG (ref #{}))
(def seenWords (ref #{}))
(def wordList (keys @words))

(defn breadth-f [start]
  "BREADTH FIRST SEARCH"
  
  (def graphQueue
    (ref PersistentQueue/EMPTY))
  (def marked
    (ref #{start}))
  (dosync
   (alter graphQueue conj start))
  
  (while (seq @graphQueue)
    (let [top (peek @graphQueue)]
      (dosync
       (alter graphQueue pop))
      (doseq [i (get total2 top)]
        (if-not (contains? @marked i)
          (dosync
           (alter marked conj i)
           (alter graphQueue conj i))))))

  (let [wonly (intersection @marked (set wordList))]
    (dosync
     (alter subG conj wonly)
     (alter seenWords union wonly))))

(defn findSubGraphs [wordsL]
  (def iCount (ref 1))
  (let [size (count wordsL)]
    (doseq [i wordsL]
      (let [poS (float (/ @iCount size))
            uS (future (prog-bar
                        (read-string
                         (format "%.0f" (* 100 poS)))))]
        (cond (not (contains? @seenWords i))
              (breadth-f i))
        (dosync (alter iCount inc))
        (deref uS)))
    (println)))   

;----FOR PRINTING-----
;---------------------

(println "##########################")
(println "GRAPH DETAILS:")
(println "number of nodes: " (+ (count wordList) (count @attr)))
(println  "number of words: " (count wordList))
(println "##########################")

(println)
(println "finding subgraphs ...")



(time (findSubGraphs wordList))
(println)
(println "##########################")
(println "num subgraphs found: " (count @subG))
;(println "aver graph size : " (float (/ (reduce +))

(def sum (reduce + (doall (map #(count %) @subG))))
(println sum)
(println "avg graph size :" (float (/ sum (count @subG))))
(println "##########################")
(println)
(println)

(shutdown-agents)
     




;parallel
;(def pData (partition-all 15 wordList))
;; (defn fPara [word]
;;   (cond (not (contains? @seenWords word))
;;         (breadth-f word)))
;; (defn pFindSub [word]
;;   (let [re (map #(future (fPara %)) word)]
;;     (doseq [i re] (deref i))))

;;for parallel
;(time (pFindSub wordList))
;(time (pFindSub pData))    

;;agents don't workd
;;;non parallel
;(time (doseq [i wordList]
;       (breadth-f i)))

;; (defn searchG [a]
;;   (doseq [wS a] 
;;     (let [agents (doall (map #(agent %) wS))]
;;       (doseq [agent agents]
;;         (println @agent)
;;         (send-off agent breadth-f))
;;       (apply await agents)
;;       (doall (map #(deref %) agents)))))
      
;; (time (searchG pData))


