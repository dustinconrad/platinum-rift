(ns Player
  (:require [clojure.string :as clj-str]
            [clojure.set :as clj-set])
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
               (sort (comp unchecked-negate (partial zone-comparator plat-info my-id)))
               first
               :zone-id
               (list my-pods id)
               (conj moves))
          moves)))
    '()
    game-state))

(defn naive-compute-purchases
  "Prioritize neutral zones"
  [plat-info my-id plat game-state]
  (let [pod-cnt (quot plat pod-price)
        valid-zones (->> (vals game-state)
                         (filter (comp #(or (= -1 %) (= my-id %)) :owner-id))
                         (sort (comp unchecked-negate (partial zone-comparator plat-info my-id)))
                         (take pod-cnt))]
    (map #(list 1 (:zone-id %)) valid-zones)))

(defn frontier-distances [link-info my-id game-state]
  (loop [n 1
         acc (->> (vals game-state)
                  (filter (comp (partial not= my-id) :owner-id))
                  (map :zone-id)
                  (into #{})
                  (assoc {} 0))]
    (if-let [n-1 (seq (acc (dec n)))]
      (let [visited (some->> (vals acc)
                             (apply concat)
                             (into #{}))]
        (->> n-1
             (mapcat
               (fn [zone-id]
                 (->> (link-info zone-id)
                      (map game-state)
                      (filter (comp (partial = my-id) :owner-id))
                      (map :zone-id))))
             (into #{})
             (remove visited)
             (assoc acc n)
             (recur (inc n))))
      (reduce
        (fn [m [d ns]]
          (->> (map #(vector % d) ns)
               (into m)))
        {}
        acc))))

(defn zone-value [plat-info link-info depth zone-id]
  (loop [v 0
         d 0
         visited #{}
         q #{zone-id}]
    (if (> d depth)
      v
      (let [modifier (/ (* (inc d) (inc d)))
            next (-> (mapcat link-info q)
                     set
                     (clj-set/difference visited))]
        (recur
          ((fnil + 0)
           (some->> (map plat-info q)
                    (reduce +)
                    (* modifier))
           v)
          (inc d)
          (clj-set/union visited q)
          next)))))

(defn plat-heat-map [plat-info link-info depth]
  (->> (keys plat-info)
       (map #(vector % (zone-value plat-info link-info depth %)))
       (into {})))

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
            purchases (naive-compute-purchases plat-info my-id platinum game-state)]
        (dbg (zone-value plat-info link-info 3 0))
        (dbg (zone-value plat-info link-info 3 1))

        ; first line for movement commands, second line for POD purchase (see the protocol in the statement for details)
        (println (->moves-format moves))
        (println (->purchases-format purchases))))))