(ns dj.workspace
  (:require [dj.dispatch.treefn :as dt]
            [dj.repl]))

;; actions
;; new: checks if exist, fail if already exists
;; reuse: reuses existing
;; auto: automatically creates one based on rules

;; remote & local
;; remote: creates using handle plus local prefix
;; local: no prefix necessary

(def reuse-target-dir (dt/fm [:target/relative-directory
                              :workspace/local-prefix]
                             (let [local-handle (dj.io/file local-prefix
                                                            relative-directory)]
                               (when-not (dj.io/exists? local-handle)
                                 (throw (ex-info "directory doesn't exist yet"
                                                 (dj.repl/local-context))))
                               local-handle)))

(def new-target-dir (dt/fm [:target/relative-directory
                            :workspace/local-prefix]
                           (let [local-handle (dj.io/file local-prefix
                                                          relative-directory)]
                             (if (dj.io/exists? local-handle)
                               (throw (ex-info "directory already exists"
                                               (dj.repl/local-context)))
                               (dj.io/mkdir local-handle))
                             local-handle)))

;;;----------------------------------------------------------------------
;;; Workspace workflow

;; 1. Acquire dependency information
;; 2. Construct internal representation
;;   -may have multiple sub steps
;;   -final sub step is emit
;; 3. Provide target environment/workspace
;; 4. Build executable
;; 5. Execute
;; 6. Obtain Post Execution State
;; 7. Analyze data

;; Might be best to use treefns to take care of this pipeline
;;----------------------------------------------------------------------
