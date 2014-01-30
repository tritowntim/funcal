(ns funcal.core
 (:require [clojure.string :as string]))

(enable-console-print!)

(println "Hello world!")

(defn main
  []
  (write (p (today)))
  (write (p (birthday)))
  (write (p (str (.toString (+ 1 (.diff (birthday) (today) "days"))) " days!")))
  (write (table (tr (render-week (countdown (birthday))))))
  (log "end of processing"))

(defn title
  []
  (str (p "<strong>fun</strong><strong><em>c</em></strong><em>al</em>")))

(defn node
  [element-name contents]
  (let [element element-name]
  (str "<" element ">" contents "</" element ">" )))

(defn p
  [contents]
  (node "p" contents))

(defn em
  [contents]
  (node "em" contents))

(defn table
  [contents]
  (node "table" contents))

(defn tr
  [contents]
  (node "tr" contents))

(defn td
  [contents]
  (node "td" contents))

(defn days
  [x]
  (map (fn [n] (write (p n)) n) (take x (iterate inc 1))))

(defn now
  []
  (.valueOf (js/moment)))

(defn today
  []
  (.startOf (js/moment) "day"))

(defn dates
  []
  (map (fn [n] (write (p (.add (today) "days" n))) n) (take 7 (iterate inc 0))))

(defn list-dates
  []
  (map (fn [n] (.add (today) "days" n)) (take 30 (iterate inc 0))))

(defn birthday
  []
  (js/moment (js/Date. 2014 5 20)))

(defn countdown
  [d-day]
  (let [now (today)
        days-until (.diff d-day (today) "days")
        within? (fn [d] (<= d days-until))]
  (map (fn [n] (.add (today) "days" n)) (take-while within? (iterate inc 0)))))

; (.diff d-day (today) "days")
; (> d days-until)

(defn weeks
  []
  (distinct (map (fn [day] (.week day)) (list-dates))))

(defn week-dates
  [week-number]
  (filter (fn [day] (= week-number (.week day))) (list-dates)))

(defn format
  [day]
  (.format day "ddd<br>D<br>MMM"))

(defn format-week
  [week]
  (map (fn [day] (format day)) week))

(defn render-week
  [dates]
  (string/join "" (map (fn [x] (td (format x))) dates)))

(defn write-seq-raw
  [seq]
  (map (fn [n] (write n))) seq)

(defn write-seq
  [seq]
  (map (fn [n] (write (p n))) seq))

(defn write
  [output]
  (println output)
  (.write js/document output))

(defn log
  [log-text]
  (.log js/console log-text))

(main)



