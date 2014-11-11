(ns Player
  (:gen-class))

(def pod-price 20)

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

(defrecord ZoneState [zone-id owner-id p0-count p1-count p2-count p3-count])

(defn read-round-game-state [zone-count]
  (loop [i zone-count
         acc {}]
    (if (zero? i)
      acc
      (let [[z-id & zone-info] (read-number-input-line)]
        (recur
          (dec i)
          (assoc acc z-id (apply ->ZoneState z-id zone-info)))))))

(defn naive-compute-moves
  "Capture zones, prioritized by platinum production."
  [plat-info link-info my-id game-state]
  (reduce
    (fn [moves [id state]]
      (let [my-pods ((keyword (str "p" id "-count")) state)]
        (if (pos? my-pods)
          (->> (get link-info id)
               (map game-state)
               (filter #(not= my-id (:owner-id %)))
               (max-key (comp plat-info :owner-id))
               :zone-id
               (list my-pods id)
               (conj moves)))
        moves))
    '()
    game-state))

(defn naive-compute-purchases
  "Prioritize neutral zones"
  [plat-info my-id plat game-state]
  (let [pod-cnt (quot plat pod-price)
        neutral-zones (->> game-state
                           (filter (comp (partial = -1) :owner-id val))
                           (sort-by (comp plat-info :zone-id) (comp unchecked-negate compare))
                           (take pod-cnt))]
    (if (= pod-cnt neutral-zones)
      (map #(list 1 (:zone-id %)) neutral-zones)
      )))

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