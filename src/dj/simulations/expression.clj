(ns dj.simulations.expression
  (:require [dj]
            [clojure.set :as s]
            [dj.dispatch.plurality :as dp]))

(defn identifiers [txt]
  (dj/re-find-all #"(?<!\d)[a-zA-Z_][a-zA-Z_0-9]*" txt))

(defn filter-by [k map-fn table]
  (for [entry table
	:let [[ek ev] entry]
	:when (= k ek)]
    (map-fn ev)))

(defn is-modifier? [modifiers-data mod-type]
  (some (fn [[t arg]]
	  (and (= t :modifier)
	       (= (first arg) mod-type)))
	modifiers-data))

(defn unused-vars [parsed-data]
  ;; there's a lot of junk in the code lufang gave me, i want to
  ;; create some tools so I can analyze the code, this will let me
  ;; know if there are unused variables

  ;; the strategy is to aggregate all the assignments together, and
  ;; all the parameters together, then build a set of used
  ;; identifiers. We recursively traverse the definitions of each
  ;; identifier and add each identifier to the used set. After all the
  ;; parameters have been searched, any identifiers remaining that are
  ;; not in the used set will be considered unused.

  ;; This is the aggregation section
  (let [{:keys [assignment params]}
	(reduce (fn [m t]
		  (let [[k v] t]
		    (case k
			  ;; we want to know all defined variables,
			  ;; but also, we want to keep track of all
			  ;; diff variables
			  :assignment (update-in m [:assignment]
						 conj
						 v)
			  :differential (-> m
					    (update-in [:assignment]
						       conj
						       [(str "diff_" (first v)) (second v)])
					    (update-in [:params]
						       conj
						       (str "diff_" (first v))))
			  :initialize (update-in m [:assignment]
						 conj
						 v)
			  :declare (let [[id ms] v]
				     (cond
				      (is-modifier? ms "param") (update-in m [:params]
									   conj
									   id)
				      ;; We want to assume external
				      ;; variables are defined since
				      ;; they are initialized
				      ;; externally
				      (is-modifier? ms "external") (update-in m [:assignment]
									      conj
									      [id "0"])
				      :else m))
			  m)))
		{:assignment []
		 :params []}
		parsed-data)
	assignment-map (assoc (apply hash-map
				     (apply concat assignment))
			 "log" "0"
			 "exp" "0"
			 "pow" "0"
			 "sqrt" "0")
	params-set (set params)]
    ;; this is the comparison section
    (loop [ps params-set
	   s params-set]
      (if (empty? ps)
	(s/difference (set (keys assignment-map)) s)
	(let [fp (first ps)
	      new-ids (s/difference (set (identifiers
					  (if-let [r (or (assignment-map fp)
							 (#{"float"} fp))]
					    r
					    (throw (Exception. (str "identifier not defined: " (pr-str fp)))))))
				    s)]
	  (if (empty? new-ids)
	    (recur (disj ps fp)
		   s)
	    (recur (s/union (disj ps fp) new-ids)
		   (s/union s new-ids))))))))

;; Find duplicates of what? assignments probably
(defn find-duplicates [parsed-data]
  (dj/duplicates (for [x parsed-data
		       :let [y (:assignment x)]
		       :when y]
		   (first y))))

(defn parse-gate-parameter [txt]
  (let [match (re-matches #"(alpha|a|beta|b|t|tau)_(.+)" txt)]
    (if match
      match
      (let [match (re-matches #"(.+)_(inf|infinity|st)" txt)]
        (when match
          (let [[variable state-variable type] match]
            [variable type state-variable]))))))

(defn dotmodel-ods-data
  "Extract ordinary differential system data from parse data
  keys that this function outputs

  :track-vars variables to track
  :shared-vars variables that will need support for sharing
  :shared-algebra-assignment algebra that will run early for performance (warning this means don't depend on strange variables not defined)
  :algebra-assignment
  :differential-assignment
  "
  [parse-data]
  ;; parameters are algebra that needs to be tracked according to
  ;; dotmodel file. This can be overridden and is only used for
  ;; advice

  ;; makes two passes because needs to collect information to make decisions about second pass
  (let [{:keys [parameters shared-algebra shared-vars all-state-vars]}
        (dj/group-by-for [[k v] parse-data
                          :let [[ret gb] (if (= k :declare)
                                           (let [[p m] v]
                                             (cond
                                               (is-modifier? m "param")
                                               [p :parameters]

                                               (is-modifier? m "shared_algebra")
                                               [p :shared-algebra]

                                               (is-modifier? m "shared_sv")
                                               [p :shared-vars]

                                               
                                               :else
                                               [p :ignore]))
                                           (if (= k :initialize)
                                             [(first v) :all-state-vars]
                                             [nil :ignore]))]
                          :group-by gb]
                         ret)
        shared-algebra-set (set shared-algebra)
        all-state-vars-set (set all-state-vars)] ; for determining if variables are part of special solver methods that only need parameters but not differential equation
    (-> (reduce (fn [ret [ek ev]]
                  (if (= ek :assignment)
                    (let [[n v] ev]
                      (if (shared-algebra-set n)
                        (let [k :shared-algebra-assignment]
                          (assoc! ret
                                  k
                                  (conj (get ret
                                             k
                                             [])
                                        ev)))
                        (let [k :algebra-assignment
                              ret' (assoc! ret
                                           k
                                           (conj (get ret
                                                      k
                                                      [])
                                                 ev))]
                          ;; handle gate case
                          (let [match (parse-gate-parameter n)]
                            (if match
                              (let [[variable type state-variable] match
                                    k :special-differential-dependencies]
                                (if (all-state-vars-set state-variable)
                                  (assoc! ret'
                                          k
                                          (update (get ret' k {})
                                                  state-variable
                                                  (fn [vars]
                                                    (conj (or vars [])
                                                          variable))))
                                  ret'))
                              ret')))))
                    (let [k (cond
                              (= ek :differential)
                              :differential-assignment

                              (= ek :initialize)
                              :initial-conditions-assignment

                              (= ek :trace)
                              :trace

                              :else :ignore)]
                      (assoc! ret
                              k
                              (conj (get ret
                                         k
                                         [])
                                    ev)))))
                (transient {:track-vars parameters
                            :shared-vars shared-vars
                            :all-state-vars all-state-vars-set})
                parse-data)
        persistent!
        (dissoc :ignore))))

(defn total-variables [model-data]
  (let [{:keys [differential-assignment parameters]} model-data]
    (+ (count differential-assignment)
       (count parameters))))

(def get-dependents (dp/->recursive-simple-multi-fn
                     {:number (fn [cont]
                                (fn [exp accum]
                                  accum))
                      :op (fn [cont]
                            (fn [{:keys [name children]} accum]
                              (reduce (fn [ret child]
                                        (cont child ret))
                                      (if name
                                        (conj accum name)
                                        accum)
                                      children)))}
                     (fn [cont]
                       (fn [exp accum]
                         accum))
                     (fn [exp accum]
                       (if (number? exp)
                         :number
                         (if (map? exp)
                           (if (:op exp)
                             :op
                             :unknown)
                           :unknown)))))
