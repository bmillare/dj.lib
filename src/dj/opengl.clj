(ns dj.opengl)

(defn build-shader
  "create any type of
  shader (javax.media.opengl.GL2ES2/[GL_VERTEX_SHADER,
  GL_FRAGMENT_SHADER]) for use within jogamp, ie glAttachShader and
  glCreateProgram
  
  currently assumes javax.media.opengl.GL2ES2
  "
  [^javax.media.opengl.GL2ES2 gl shader-type src]
  (let [shader (.glCreateShader gl shader-type)
        compile-status-buffer (int-array 1)]
    (.glShaderSource gl
                     shader
                     1
                     (into-array String [src])
                     (int-array [(count src)])
                     0)
    (.glCompileShader gl
                      shader)
    (.glGetShaderiv gl
                    shader
                    javax.media.opengl.GL2ES2/GL_COMPILE_STATUS
                    compile-status-buffer
                    0)
    (let [compiler-status (first compile-status-buffer)]
      (if (zero? compiler-status)
        (throw (ex-info "compiling shader failed"
                        {:src src
                         :shader-type shader-type
                         :compiler-status compiler-status
                         :log (let [log-length-buffer (int-array 1)
                                    _ (.glGetShaderiv gl
                                                      shader
                                                      javax.media.opengl.GL2ES2/GL_INFO_LOG_LENGTH
                                                      log-length-buffer
                                                      0)
                                    log (byte-array (first log-length-buffer))]
                                (.glGetShaderInfoLog gl
                                                     shader
                                                     (first log-length-buffer)
                                                     (int-array 1)
                                                     0
                                                     log
                                                     0)
                                (->> log
                                     (map char)
                                     (apply str)))}))
        shader))))

(defn uniform
  [^javax.media.opengl.GL2ES2 gl type id value]
  (case type
    :matrix4fv (.glUniformMatrix4fv gl
                                    (int id)
                                    (int (/ (count value)
                                            16))
                                    false
                                    value
                                    (int 0))
    :1i (.glUniform1i gl (int id) value)
    :sampler2D (.glUniform1i gl (int id) (int value))))

(defn set-vao [^javax.media.opengl.GL2ES2 gl vao-idx vbo-handle ^floats data type dimension]
  (let [fb-data (com.jogamp.common.nio.Buffers/newDirectFloatBuffer data)]
    (doto gl
      (.glBindBuffer javax.media.opengl.GL2ES2/GL_ARRAY_BUFFER
                     (int vbo-handle))
      (.glBufferData javax.media.opengl.GL2ES2/GL_ARRAY_BUFFER
                     (int (* (count data)
                         4))
                     fb-data
                     javax.media.opengl.GL2ES2/GL_STATIC_DRAW)
      (.glVertexAttribPointer (int vao-idx) ;; vertex attribute
                              (int dimension)
                              type
                              false    ;; normalized
                              (int 0)  ;; stride
                              (int 0)  ;; bound VBO offset
                              )
      (.glEnableVertexAttribArray (int vao-idx)))))

(defn set-ibo [^javax.media.opengl.GL2ES2 gl gl-index ^shorts data]
  (let [fb-data (com.jogamp.common.nio.Buffers/newDirectShortBuffer data)]
    (doto gl
      (.glBindBuffer javax.media.opengl.GL2ES2/GL_ELEMENT_ARRAY_BUFFER
                     (int gl-index))
      (.glBufferData javax.media.opengl.GL2ES2/GL_ELEMENT_ARRAY_BUFFER
                     (int (* (count data)
                             2))
                     fb-data
                     javax.media.opengl.GL2ES2/GL_STATIC_DRAW))))
