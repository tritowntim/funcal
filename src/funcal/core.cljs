(ns funcal.core
 (:require [clojure.string :as string]))

(enable-console-print!)

(defn main
  []
  (write (p (today)))
  (write (p (.week (js/moment))))
  (write (p (birthday)))
  (write (p (.week (birthday))))
  (write (p (str (.toString (+ 1 (.diff (birthday) (today) "days"))) " days!")))
  (write (table (map (fn [week] (render-week (countdown-week-backfill (birthday) week))) (range (.week (js/moment)) (+ 1 (.week (birthday)))) ))))

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


(defn countdown-week
  [d-day week-number]
  (filter (fn [x] (= (.week x) week-number)) (countdown d-day)))

(defn countdown-week-backfill
  [d-day week-number]
  (let [the-week (countdown-week d-day week-number)]
  (flatten (if (= (.day (first the-week)) 0)
    (conj (repeat (- 7 (count the-week)) nil) the-week)
    (cons (repeat (- 7 (count the-week)) nil) the-week)))))

(defn weeks
  []
  (distinct (map (fn [day] (.week day)) (list-dates))))

(defn week-dates
  [week-number]
  (filter (fn [day] (= week-number (.week day))) (list-dates)))

(defn format
  [day]
  (if day (.format day "ddd<br>D<br>MMM") "<br>"))

(defn format-week
  [week]
  (map (fn [day] (format day)) week))

(defn render-week
  [dates]
  (tr (string/join "" (map (fn [x] (td (format x))) dates))))

(defn write-seq-raw
  [seq]
  (map (fn [n] (write n))) seq)

(defn write-seq
  [seq]
  (map (fn [n] (write (p n))) seq))

(defn write
  [output]
  (.write js/document output))

(defn log
  [log-text]
  (.log js/console log-text))

(main)



