(ns ^{:doc "read in files for graph analysis"
      :author "Kyle Richardson"}
      latticeGRAM.inputReader
      (:use [clojure.string :only [split]]
            [clojure.java.io :only [reader]]
            [clojure.set]))

(defn re-match? [re s]
  (not (nil? (re-find re s))))

(defn objectV? [s]
  (re-match? #"^[\w+\s*]+\-\-" s))

(def words (ref {}))
(def attr (ref {}))

(defn printBar [percent]
  "PRINTS PROGRESS BAR"
  (let [bar (StringBuilder. "[")] 
    (doseq [i (range 50)]
      (cond (< i (int (/ percent 2)))
            (.append bar "=")
            (= i (int (/ percent 2)))
            (.append bar ">")
            :else
            (.append bar " ")))
    (.append bar (str "] " percent "%     "))
    (print "\r"(.toString bar))
    (flush)))

(defn progBar [item size]
  (let [poS (float (/ item size))
        uS (read-string
            (format "%.0f" (* 100 poS)))]
    (printBar uS)))

(defn findR [i]
  (doseq [h (split (last i) #";")
          v (first i) 
          new (union (get @attr h) [v])]
    (cond (contains? @attr h)
          (dosync (alter attr merge
                         {h new}))
          :else
          (dosync (alter attr assoc
                         h [v])))))

(defn readF [f]
  (def iCount1 (ref 1))
  (let [cut (filter objectV? f)
        v1 (map #(split % #"--") cut)
        si (count v1)]
    (doseq [i v1]
      ;(println i)
      (let [prog (future (progBar @iCount1 si))]
        (def rev (future findR i))
        (dosync (alter words assoc
                       (first i)
                       (split (last i) #";"))
                (alter iCount1 inc))
        (deref prog)
        (deref rev)))
    (println)))
               
(defn ioMain [fileLoc]
  (readF fileLoc)
  (do [@words @attr]))



;for running locally
;; (def fileLoc
;;   (line-seq (reader "../../data/futbolData/output/window3.txt")))
;; (time (ioMain fileLoc))
;; (shutdown-agents)
  
        
  








