(ns latticeGRAM.core)
(use '[clojure.string :only (split)])
(use '[clojure.java.io :only (reader)])
(use '[clojure.set])
(import '[clojure.lang PersistentQueue])
;(use 'clojure.contrib.math)

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
  (let [cut (filter objectV? f)
        v1 (map #(split % #"--") cut)]
    (doseq [i v1]
      (def rev (future (findR i)))
      (dosync (alter words assoc
                     (first i)
                     (split (last i) #";")))
      (deref rev))))

(print "reading input file: ")
(time (readF inputFile))

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

(def iCount (ref 1))

(defn findSubGraphs [wordsL]
  (let [size (count wordsL)]
    (doseq [i wordsL]
      (let [poS (float (/ @iCount size))
            uS (future (prog-bar (* 100 poS)))]
        (cond (not (contains? @seenWords i))
              (breadth-f i))
        (dosync (alter iCount inc))
        (deref uS)))
    (println)))   

;----FOR PRINTING-----
;---------------------

(def banner  (str "*************************\n"
                  "****COMMUNITY DETECTION\n"
                  "*************************\n"))
(println)                    
(println banner)
(println "GRAPH DETAILS:")
(println "\t number of nodes: " (+ (count wordList) (count @attr)))
(println  "\t number of words: " (count wordList))
(println)
(println)
(println "finding subgraphs ...")
(time (findSubGraphs wordList))

(println "# disconnected subgraphs: " (count @subG))
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


