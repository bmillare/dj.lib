;; #section:1 {:tags [:programming/tools] :created "2016-04-28"}
(ns dj.tracked-futures
  "provide tools for creating futures that are easily tracked and
  managed like jobs on a cluster")

(defn tracked-future-call-fn
  []
  (let [db (atom {:running {}
                  :last-id -1})]
    (with-meta
      (fn [f]
        (let [free-id (:last-id
                       (swap! db
                              (fn [state]
                                (let [{:keys [running last-id]} state]
                                  (let [free-id (inc last-id)]
                                    (-> state
                                        (assoc :last-id free-id)
                                        (assoc-in [:running free-id]
                                                  {:future nil
                                                   :start (java.util.Date.)})))))))
              fut (future-call (fn []
                                 (let [ret (f)]
                                   (swap! db assoc-in [:running free-id :end] (java.util.Date.))
                                   ret)))]
          (swap! db assoc-in [:running free-id :future] fut)
          fut))
      {::db db})))

(defn running
  "list running futures"
  [f-caller]
  (reduce-kv (fn [ret k v]
               (let [{:keys [start end]} v]
                 (if end
                   ret
                   (conj ret
                         [k (let [diff (- (.getTime (java.util.Date.))
                                          (.getTime ^java.util.Date start))
                                  hours (.toHours java.util.concurrent.TimeUnit/MILLISECONDS diff)
                                  mins (.toMinutes java.util.concurrent.TimeUnit/MILLISECONDS diff)]
                              (format "%02d:%02d:%02d"
                                      hours
                                      (- mins
                                         (.toMinutes java.util.concurrent.TimeUnit/HOURS hours))
                                      (- (.toSeconds java.util.concurrent.TimeUnit/MILLISECONDS diff)
                                         (.toSeconds java.util.concurrent.TimeUnit/MINUTES mins))))]))))
             [[:id :start :run-time]]
             (-> (meta f-caller)
                 ::db
                 deref
                 :running)))

(defn finished
  "list finished futures"
  [f-caller]
  (reduce-kv (fn [ret k v]
               (let [{:keys [start end]} v]
                 (if end
                     (conj ret
                           [k (let [diff (- (.getTime ^java.util.Date end)
                                            (.getTime ^java.util.Date start))
                                    hours (.toHours java.util.concurrent.TimeUnit/MILLISECONDS diff)
                                    mins (.toMinutes java.util.concurrent.TimeUnit/MILLISECONDS diff)]
                                (format "%02d:%02d:%02d"
                                        hours
                                        (- mins
                                           (.toMinutes java.util.concurrent.TimeUnit/HOURS hours))
                                        (- (.toSeconds java.util.concurrent.TimeUnit/MILLISECONDS diff)
                                           (.toSeconds java.util.concurrent.TimeUnit/MINUTES mins))))])
                     ret)))
             [[:id :start :run-time]]
             (-> (meta f-caller)
                 ::db
                 deref
                 :running)))

(defn kill
  "cancel future and mark finished, pass future id"
  [f-caller id]
  (let [db (-> (meta f-caller)
               ::db)]
    (-> db
        deref
        :running
        (get id)
        :future
        future-cancel)
    (swap! db
           update-in [:running id :end] (fn [end]
                                          (or end (java.util.Date.))))
    (-> db
        deref
        :running
        (get id))))

(defn kill-all
  "cancels all running futures and marks finished"
  [f-caller]
  (let [db (-> (meta f-caller)
               ::db)]
    (doseq [[id v] (-> db
                      deref
                      :running)]
      (-> v
          :future
          future-cancel)
      (swap! db
             update-in [:running id :end] (fn [end]
                                            (or end (java.util.Date.)))))))

(defn clear-finished
  "removes all finished futures from db"
  [f-caller]
  (let [db (-> (meta f-caller)
               ::db)
        remove-ids (reduce-kv (fn [ids id row]
                                (if (:end row)
                                  (conj ids id)
                                  ids))
                              []
                              (-> db
                                  deref
                                  :running))]
    (apply swap! db update-in [:running] dissoc remove-ids)))

(defn get-fut
  "returns the tracked future corresponding to the provided id"
  [f-caller id]
  (-> (meta f-caller)
      ::db
      deref
      :running
      (get id)))
