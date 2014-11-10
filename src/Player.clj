(ns Player
  (:gen-class))

(defn read-number-input-line []
  (->> (read-line)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))))

(defn read-platinum-info [zone-count]
  (loop [i zone-count
         acc {}]
    (if (zero? i)
      acc
      (recur
        (dec i)
        (apply assoc acc (read-number-input-line))))))

(defn read-link-info [link-count]
  (loop [i link-count
         acc {}]
    (if (zero? i)
      acc
      (let [[zone1 zone2] (read-number-input-line)]
        (recur
          (dec i)
          (-> (update-in acc [zone1] conj zone2)
              (update-in [zone2] conj zone1)))))))

(defn -main [& args]
  (let [[playerCount my-id zone-count link-count] (read-number-input-line)
        zone-info (read-platinum-info zone-count)
        link-info (read-link-info link-count)]
    ; playerCount: the amount of players (2 to 4)
    ; myId: my player ID (0, 1, 2 or 3)
    ; zoneCount: the amount of zones on the map
    ; linkCount: the amount of links between all zones
    (binding [*out* *err*]
       (println "zone info")
       (println zone-info)
       (println)
       (println "link info")
       (println link-info)
       (println))
    (while true
      (let [platinum (read)]
        ; platinum: my available Platinum
        (loop [i zone-count]
          (when (> i 0)
            (let [zId (read) ownerId (read) podsP0 (read) podsP1 (read) podsP2 (read) podsP3 (read)]
              ; zId: this zone's ID
              ; ownerId: the player who owns this zone (-1 otherwise)
              ; podsP0: player 0's PODs on this zone
              ; podsP1: player 1's PODs on this zone
              ; podsP2: player 2's PODs on this zone (always 0 for a two player game)
              ; podsP3: player 3's PODs on this zone (always 0 for a two or three player game)
              (recur (dec i)))))

        ; (binding [*out* *err*]
        ;   (println "Debug messages..."))

        ; first line for movement commands, second line for POD purchase (see the protocol in the statement for details)
        (println "WAIT")
        (println "1 73")))))