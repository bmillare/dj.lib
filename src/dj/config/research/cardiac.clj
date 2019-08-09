(ns dj.config.research.cardiac)

(def sv-clip-values {"vm" {:lower-clip-value -92.0
                           :upper-clip-value 50.0}
                     "G_Dpsi" {:lower-clip-value 0.0
                               :upper-clip-value 132}
                     "G_Ca_i" {:lower-clip-value 0.0
                               :upper-clip-value 0.00395}
                     "G_TRACE_IKATP" {:lower-clip-value 0.0
                                      :upper-clip-value 17.9}
                     "G_h" {:lower-clip-value 0.0
                            :upper-clip-value 1.0}})
