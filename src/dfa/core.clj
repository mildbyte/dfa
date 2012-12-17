(ns dfa.core)

(def all-accepted-strings
  (fn [{states :states,
        alphabet :alphabet,
        start :start,
        accepts :accepts,
        transitions :transitions}]
    (let [filter-accepted-strings
          (fn [current-states]
            (map (fn [[current-state string]] (apply str string))
                 (filter (fn [[current-state string]]
                           (accepts current-state))
                         current-states)))
          generate-next-state
          (fn [[current-state string]]
            (let [possible-transitions (transitions current-state)]
              (for [possible-symbol (keys possible-transitions)]
                [(possible-transitions possible-symbol)
                   (conj string possible-symbol)])))
          generate-all-next-states
          (fn [current-states]
            (apply concat
                   (map generate-next-state current-states)))]
      (filter-accepted-strings
       (apply concat
              (take-while #(not (empty? %))
                          (iterate generate-all-next-states
                                   [[start []]])))))))