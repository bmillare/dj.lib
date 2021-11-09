(ns dj.thread)

(defn thread-pool
  "return new fixed thread pool of size nthreads"
  [nthreads]
  (java.util.concurrent.Executors/newFixedThreadPool (int nthreads)))

(defn scheduled-thread-pool
  "return new fixed scheduled thread pool of size nthreads"
  [nthreads]
  (java.util.concurrent.ScheduledThreadPoolExecutor. (int nthreads)))

(defn scheduled-future-call
  [f
   {:keys [scheduled-thread-pool
           delay]}]
  (assert (not (nil? scheduled-thread-pool)))
  (assert (number? delay))
  (let [fut (.schedule ^java.util.concurrent.ScheduledThreadPoolExecutor scheduled-thread-pool
                       ^Callable f
                       (long delay)
                       java.util.concurrent.TimeUnit/MILLISECONDS)]
    (reify
      clojure.lang.IDeref
      (deref [_]
        (.get fut))
      clojure.lang.IPending
      (isRealized [_]
        (.isDone fut))
      java.util.concurrent.Future
      (get [_] (.get fut))
      (get [_ timeout unit] (.get fut timeout unit))
      (isCancelled [_] (.isCancelled fut))
      (isDone [_] (.isDone fut))
      (cancel [_ interrupt?] (.cancel fut interrupt?)))))

(defn pool-future-call
  [f
   the-thread-pool]
  (let [fut (.submit ^java.util.concurrent.ExecutorService the-thread-pool
                     ^Callable f)]
    (reify
      clojure.lang.IDeref
      (deref [_]
        (.get fut))
      clojure.lang.IPending
      (isRealized [_]
        (.isDone fut))
      java.util.concurrent.Future
      (get [_] (.get fut))
      (get [_ timeout unit] (.get fut timeout unit))
      (isCancelled [_] (.isCancelled fut))
      (isDone [_] (.isDone fut))
      (cancel [_ interrupt?] (.cancel fut interrupt?)))))

(defmacro pool-future
  [the-thread-pool
   & body]
  `(pool-future-call (^{:once true} fn* []
                      ~@body)
                     ~the-thread-pool))

(defmacro scheduled-future
  [args
   & body]
  `(scheduled-future-call (^{:once true} fn* []
                           ~@body)
                          ~args))
