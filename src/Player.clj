(ns Player
  (:require [clojure.string :as clj-str])
  (:gen-class))

(defmacro dbg [x]
  `(let [x# ~x]
     (binding [*out* *err*]
       (println "dbg:" '~x "=" x#))
     x#))

(defmacro dbg-v [x]
  `(let [x# ~x]
     (binding [*out* *err*]
       (println "dbg-v:" x#))
     x#))

(def pod-price 20)

(defn read-number-input-line []
  (->> (read-line)
       (re-seq #"-?\d+")
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

(defn zone-comparator [plat-info my-id z1 z2]
  (let [z1-owner (:owner-id z1)
        z2-owner (:owner-id z2)]
    (cond
      (= z1-owner z2-owner) (compare (plat-info (:zone-id z1)) (plat-info (:zone-id z2)))
      (and (not= z1-owner my-id) (not= z2-owner my-id)) (compare (plat-info (:zone-id z1)) (plat-info (:zone-id z2)))
      (= z1-owner my-id) -1
      (= z2-owner my-id) 1)))

(defn naive-compute-moves
  "Capture zones, prioritized by platinum production."
  [plat-info link-info my-id game-state]
  (reduce
    (fn [moves [id state]]
      (let [my-pods ((keyword (str "p" my-id "-count")) state)]
        (if (pos? my-pods)
          (->> (get link-info id)
               (map game-state)
               (sort (partial zone-comparator plat-info my-id))
               first
               :zone-id
               (list my-pods id)
               (conj moves))
          moves)))
    '()
    game-state))

(defn naive-compute-purchases
  "Prioritize neutral zones"
  [plat-info plat game-state]
  (let [pod-cnt (quot plat pod-price)
        neutral-zones (->> (vals game-state)
                           (filter (comp (partial = -1) :owner-id))
                           (sort-by (comp plat-info :zone-id) (comp unchecked-negate compare))
                           (take pod-cnt))]
    (->> (map #(list 1 (:zone-id %)) neutral-zones)
         (take pod-cnt))))

(defn ->moves-format [moves]
  (if (empty? moves)
    "WAIT"
    (->> (apply concat moves)
         (clj-str/join " "))))

(defn ->purchases-format [purchases]
  (if (empty? purchases)
    "WAIT"
    (->> (apply concat purchases)
         (clj-str/join " "))))

(defn -main [& args]
  (let [[playerCount my-id zone-count link-count] (read-number-input-line)
        plat-info (initialize-platinum-info zone-count)
        link-info (initialize-link-info link-count)]

    (while true
      (let [platinum (read-round-platinum-info)
            game-state (read-round-game-state zone-count)
            moves (naive-compute-moves plat-info link-info my-id game-state)
            purchases (naive-compute-purchases plat-info platinum game-state)]

        (dbg moves)
        (dbg purchases)

        ; first line for movement commands, second line for POD purchase (see the protocol in the statement for details)
        (println (->moves-format moves))
        (println (->purchases-format purchases))))))