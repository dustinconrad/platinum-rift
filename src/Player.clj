(ns Player
  (:gen-class))

(defn read-number-input-line []
  (->> (read-line)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))))

(defn initialize-platinum-info [zone-count]
  (loop [i zone-count
         acc {}]
    (if (zero? i)
      acc
      (recur
        (dec i)
        (apply assoc acc (read-number-input-line))))))

(defn initialize-link-info [link-count]
  (loop [i link-count
         acc {}]
    (if (zero? i)
      acc
      (let [[zone1 zone2] (read-number-input-line)]
        (recur
          (dec i)
          (-> (update-in acc [zone1] conj zone2)
              (update-in [zone2] conj zone1)))))))

(defn read-round-platinum-info []
  (->> (read-line)
       Integer/parseInt))

(defrecord ZoneState [owner-id p0-count p1-count p2-count p3-count])

(defn read-round-game-state [zone-count]
  (loop [i zone-count
         acc {}]
    (if (zero? i)
      acc
      (let [[z-id & zone-info] (read-number-input-line)]
        (recur
          (dec i)
          (assoc acc z-id (apply ->ZoneState zone-info)))))))

(defn -main [& args]
  (let [[playerCount my-id zone-count link-count] (read-number-input-line)
        zone-info (initialize-platinum-info zone-count)
        link-info (initialize-link-info link-count)]
    #_(binding [*out* *err*]
       (println "zone info")
       (println zone-info)
       (println)
       (println "link info")
       (println link-info)
       (println))
    (while true
      (let [platinum (read-round-platinum-info)
            game-state (read-round-game-state zone-count)]

        (binding [*out* *err*]
          (println "game-state")
          (println game-state)
          (println))

        ; first line for movement commands, second line for POD purchase (see the protocol in the statement for details)
        (println "WAIT")
        (println "1 73")))))