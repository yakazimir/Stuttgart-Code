(ns ^{:doc "main file for doing community analysis"
      :author "Kyle Richardson"}
  latticeGRAM.core
  (:use [clojure.set]
        [clojure.java.io :only [reader]]
        [latticeGram.inputReader :only [ioMain progBar]])
  (:import [clojure.lang PersistentQueue]))


(def banner  (str "\t*************************\n"
                  "\t***COMMUNITY DETECTION***\n"
                  "\t*************************\n"))
(println)                    
(println banner)

;files
(def fileLoc (line-seq (reader "../../data/futbolData/output/window3.txt")))
;(def fileLoc (line-seq (reader "../../data/larger.txt")))
;(def fileLoc (line-seq (reader "../../data/practiceFile.txt")))





;------------------
;--BREADTH FIRST---
;------------------
;; (def total2 (merge @words @attr))
;; (def subG (ref #{}))
;; (def seenWords (ref #{}))
;; (def wordList (keys @words))

;; (defn breadth-f [start]
;;   "BREADTH FIRST SEARCH"
  
;;   (def graphQueue
;;     (ref PersistentQueue/EMPTY))
;;   (def marked
;;     (ref #{start}))
;;   (dosync
;;    (alter graphQueue conj start))
  
;;   (while (seq @graphQueue)
;;     (let [top (peek @graphQueue)]
;;       (dosync
;;        (alter graphQueue pop))
;;       (doseq [i (get total2 top)]
;;         (if-not (contains? @marked i)
;;           (dosync
;;            (alter marked conj i)
;;            (alter graphQueue conj i))))))

;;   (let [wonly (intersection @marked (set wordList))]
;;     (dosync
;;      (alter subG conj wonly)
;;      (alter seenWords union wonly))))

;; (defn findSubGraphs [wordsL]
;;   (def iCount (ref 1))
;;   (let [size (count wordsL)]
;;     (doseq [i wordsL]
;;       (let [poS (float (/ @iCount size))
;;             uS (future (prog-bar
;;                         (read-string
;;                          (format "%.0f" (* 100 poS)))))]
;;         (cond (not (contains? @seenWords i))
;;               (breadth-f i))
;;         (dosync (alter iCount inc))
;;         (deref uS)))
;;     (println)))   

;; ;----FOR PRINTING-----
;; ;---------------------

;; (println "##########################")
;; (println "GRAPH DETAILS:")
;; (println "number of nodes: " (+ (count wordList) (count @attr)))
;; (println  "number of words: " (count wordList))
;; (println "##########################")

;; (println)
;; (println "finding subgraphs ...")



;; (time (findSubGraphs wordList))
;; (println)
;; (println "##########################")
;; (println "num subgraphs found: " (count @subG))
;; ;(println "aver graph size : " (float (/ (reduce +))

;; (def sum (reduce + (doall (map #(count %) @subG))))
;; (println sum)
;; (println "avg graph size :" (float (/ sum (count @subG))))
;; (println "##########################")
;; (println)
;; (println)

;; (shutdown-agents)
     
;$$$$$$$$$$$$$$$$














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


