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
(def neutral-zone-owner-id -1)

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

(defrecord ZoneState [zone-id owner-id pod-counts])

(defrecord PlayerState [id zone-count income total-pods])

(defn read-round-game-state [zone-count]
  (loop [i zone-count
         acc {}]
    (if (zero? i)
      acc
      (let [[z-id owner-id & pod-info] (read-number-input-line)
            pod-map (zipmap (range 4)
                            pod-info)]
        (recur
          (dec i)
          (assoc acc z-id (->ZoneState z-id owner-id pod-map)))))))

(defn compute-player-states [plat-info game-state]
  (let [update-player-state (fn [current-state {:keys [zone-id owner-id pod-counts]}]
                              (let [owner-pods (pod-counts owner-id)
                                    zone-income (plat-info zone-id)]
                                (-> current-state
                                    (update-in [:zone-count] (fnil inc 0))
                                    (update-in [:income] (fnil + 0) zone-income)
                                    (update-in [:total-pods] (fnil + 0) owner-pods))))]
    (->> (vals game-state)
         (reduce
           (fn [acc {:keys [owner-id] :as zone-state}]
             (if (not= neutral-zone-owner-id owner-id)
               (update-in
                 acc
                 [owner-id]
                 update-player-state
                 zone-state)
               acc))
           {}))))

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

(defn contiguous-zone [link-info game-state zone-id]
  (let [owner-id (get-in game-state [zone-id :owner-id])
        owner-pred (fn [z-id]
                     (= owner-id
                        (get-in game-state [z-id :owner-id])))]
    (loop [visited #{}
           q (conj (clojure.lang.PersistentQueue/EMPTY) zone-id)
           acc #{}]
      (if (empty? q)
        acc
        (let [z-id (peek q)
              new-zones (->> (link-info z-id)
                             (filter owner-pred)
                             (remove visited))]
          (recur
            (conj visited z-id)
            (into (pop q) new-zones)
            (conj acc z-id)))))))

(defn contiguous-zones [link-info game-state]
  (loop [remaining-zones (set (keys link-info))
         acc #{}]
    (if (empty? remaining-zones)
      acc
      (let [z (first remaining-zones)
            contiguous-area (contiguous-zone link-info game-state z)]
        (recur
          (disj (clj-set/difference remaining-zones contiguous-area) z)
          (conj acc contiguous-area))))))

(defn base-zone-value [plat-info link-info depth zone-id]
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

(defn zone-value-map [plat-info link-info depth]
  (->> (keys plat-info)
       (map #(vector % (base-zone-value plat-info link-info depth %)))
       (into {})))

(defn halving [n]
  (cond
    (>= 0 n) nil
    (= 1 n) '(1)
    :else (let [half (if (even? n)
                       (/ n 2)
                       (/ (inc n) 2))]
            (cons half (lazy-seq (halving (- n half)))))))

(defn adjacent-enemies [link-info my-id game-state zone-id]
  (if-let [result (some->> (link-info zone-id)
                           (map game-state)
                           (map #(dissoc (:pod-counts %) my-id))
                           (mapcat vals)
                           (apply max))]
    result
    0))

(defn create-score-fn [zone-vals link-info my-id game-state frontier-map]
  (fn [zone-id]
    (let [distance (get frontier-map zone-id (dec Integer/MAX_VALUE))
          enemies (adjacent-enemies link-info my-id game-state zone-id)
          my-pods (get-in game-state [zone-id :pod-counts my-id])
          multiplier (if (and (apply = 0 (vals (get-in game-state [zone-id :pod-counts])))
                              (pos? (get-in game-state [zone-id :owner-id])))
                       2
                       1)]
      (cond
        (< 1 distance) (/ (inc distance))
        (zero? distance) (* multiplier (+ 51/100 (zone-vals zone-id)))

        (and (pos? enemies) (<= my-pods enemies))
        (+ 51/100 (zone-vals zone-id))

        :else (/ (inc distance))))))

(defn allocate-moves [live-zone-values pod-cnt possible-moves]
  (let [max-moves-cnt (min pod-cnt (count possible-moves))
        trimmed-moves (->> (sort-by live-zone-values (comp unchecked-negate compare) possible-moves)
                           (take max-moves-cnt))
        trimmed-moves-sum (reduce #(+ %1 (live-zone-values %2)) 0 trimmed-moves)]
    (loop [rem-pods pod-cnt
           [m & ms :as all-moves] trimmed-moves
           committed-moves '()]
      (if (or (empty? all-moves) (<= rem-pods 0))
        committed-moves
        (let [allocated (min (int (inc (* rem-pods (/ (live-zone-values m) trimmed-moves-sum))))
                             rem-pods)]
          (recur
            (- rem-pods allocated)
            ms
            (conj committed-moves (list allocated m))))))))

(defn compute-moves [link-info my-id game-state zone-values]
  (let [move-fn (fn [zone-id]
                  (let [pod-cnt (get-in game-state [zone-id :pod-counts my-id])
                        current-val (zone-values zone-id)
                        pod-seq (halving pod-cnt)]
                    (->> (link-info zone-id)
                         (sort-by zone-values (comp unchecked-negate compare))
                         (take-while #(<= current-val (zone-values %)))
                         (map vector pod-seq (repeat zone-id)))))]
    (->> (vals game-state)
         (filter #(pos? (get-in % [:pod-counts my-id])))
         (mapcat (comp move-fn :zone-id)))))

(defn live-zone-values [base-zone-values link-info my-id game-state frontier-map]
  (let [score-fn (create-score-fn base-zone-values link-info my-id game-state frontier-map)]
    (->> (map (juxt identity score-fn) (keys game-state))
         (into {}))))

(defn compute-purchases [link-info my-id plat game-state live-zone-values]
  (let [pod-cnt (quot plat pod-price)
        purchases (quot pod-cnt 1)]
    (->> (vals game-state)
         (filter (comp #(or (= my-id %) (= neutral-zone-owner-id %)) :owner-id))
         (map :zone-id)
         (sort-by live-zone-values (comp unchecked-negate compare))
         (take purchases)
         (map #(vector (quot pod-cnt purchases) %)))))

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
        link-info (initialize-link-info link-count)
        plat-vals (zone-value-map plat-info link-info 3)]

    (while true
      (let [platinum (read-round-platinum-info)
            game-state (read-round-game-state zone-count)
            frontier-map (frontier-distances link-info my-id game-state)
            zone-values (live-zone-values plat-vals link-info my-id game-state frontier-map)
            moves (compute-moves link-info my-id game-state zone-values)
            purchases (compute-purchases link-info my-id platinum game-state zone-values)]

        #_(dbg (compute-player-states plat-info game-state))

        (dbg (contiguous-zones link-info game-state))

        ; first line for movement commands, second line for POD purchase (see the protocol in the statement for details)
        (println (->moves-format moves))
        (println (->purchases-format purchases))))))