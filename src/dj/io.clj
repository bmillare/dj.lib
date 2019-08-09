(ns dj.io)

(defmacro with-file-channel
  "see java.io.RandomAccessFile for what the mode is, but options are \"r\",\"rw\",\"rws\",\"rwd\"

  channel will be bound to the filechannel for file

  closes channel on exit of body
  "
  [[channel file mode]
   & body]
  (let [file-sym (vary-meta (gensym "file") assoc :tag 'java.io.File)]
    `(let [~file-sym ~file
           raf# (java.io.RandomAccessFile. ~file-sym
                                           ~mode)]
       (with-open [~channel (.getChannel raf#)]
         ~@body))))
