(ns dj.plot)

;; code to convert between formats, create colorbars and 2d image plots
;; - probably need to split this up later into categories

(defn intrgb->byte-array [data]
  (byte-array (apply concat (mapv (fn [v]
                                    (let [iv (int v)]
                                      [(bit-shift-right (bit-and 0x00ff0000 iv) 16)
                                       (bit-shift-right (bit-and 0x0000ff00 iv) 8)
                                       (bit-and 0x000000ff iv)]))
                                  data))))

;; Ints: From black to purple to red to white
(def multicolorbw {:data-ints [-16777216 -15204328 -13565903 -11993015 -11206559 -11206535 -11206510 -11206486 -12779326 -14417701 -15990541 -16773889 -16767745 -16761345 -16755201 -16749057 -16742657 -16736513 -16730369 -16724225 -16717825 -16711681 -16711705 -16711730 -16711754 -16711778 -16711802 -16711827 -16711851 -16711875 -16711900 -16711924 -15925504 -14352640 -12714240 -11141376 -9568512 -7930112 -6357248 -4784384 -3211520 -1573120 -256 -6400 -12800 -18944 -25088 -31232 -37632 -43776 -49920 -56320 -62464 -62452 -56284 -49859 -43691 -37523 -31098 -24930 -18762 -12594 -6169 -1]
                   :data-components [[0 0 0] [24 0 24] [49 0 49] [73 0 73] [85 0 97] [85 0 121] [85 0 146] [85 0 170] [61 0 194] [36 0 219] [12 0 243] [0 12 255] [0 36 255] [0 61 255] [0 85 255] [0 109 255] [0 134 255] [0 158 255] [0 182 255] [0 206 255] [0 231 255] [0 255 255] [0 255 231] [0 255 206] [0 255 182] [0 255 158] [0 255 134] [0 255 109] [0 255 85] [0 255 61] [0 255 36] [0 255 12] [12 255 0] [36 255 0] [61 255 0] [85 255 0] [109 255 0] [134 255 0] [158 255 0] [182 255 0] [206 255 0] [231 255 0] [255 255 0] [255 231 0] [255 206 0] [255 182 0] [255 158 0] [255 134 0] [255 109 0] [255 85 0] [255 61 0] [255 36 0] [255 12 0] [255 12 12] [255 36 36] [255 61 61] [255 85 85] [255 109 109] [255 134 134] [255 158 158] [255 182 182] [255 206 206] [255 231 231] [255 255 255]]
                   :channel-width 8
                   :format :RGB})

(def distinct-colors ["#000000", "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
                      "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
                      "#5A0007", "#809693", "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
                      "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100",
                      "#DDEFFF", "#000035", "#7B4F4B", "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F",
                      "#372101", "#FFB500", "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
                      "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66",
                      "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C",
                      "#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
                      "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00",
                      "#7900D7", "#A77500", "#6367A9", "#A05837", "#6B002C", "#772600", "#D790FF", "#9B9700",
                      "#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
                      "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C"])

(defn ->buffered-image
  "
image-type: java.awt.image.BufferedImage/TYPE_INT_RGB
  "
  ^java.awt.image.BufferedImage
  [{:keys [width height image-type]}]
  (let [image-type (or image-type
                       java.awt.image.BufferedImage/TYPE_INT_RGB)]
    (java.awt.image.BufferedImage. width
                                   height
                                   image-type)))

;; Needed so we can be particular about how the image is scaled and
;; not rely on the browser / viewer
(let [scale-method {:bicubic java.awt.RenderingHints/VALUE_INTERPOLATION_BICUBIC
                    :bilinear java.awt.RenderingHints/VALUE_INTERPOLATION_BILINEAR
                    :nearest-neighbor java.awt.RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR}]
  (defn scale-image [^java.awt.image.BufferedImage image {:keys [width height scale-type]}]
    (let [nbi (java.awt.image.BufferedImage. width
                                             height
                                             (.getType image))]
      (doto (.createGraphics nbi)
        (.setRenderingHint java.awt.RenderingHints/KEY_INTERPOLATION
                           (scale-method scale-type))
        (.drawImage image 0 0 width height nil)
        (.dispose))
      nbi)))

(defn image->png-binary
  "convert bufferedimage into a png binary stored in a bytearray"
  [^java.awt.image.BufferedImage bi]
  (let [bos (java.io.ByteArrayOutputStream.)]
    (javax.imageio.ImageIO/write bi "png" bos)
    (.toByteArray bos)))

;; add refactor from dj.cuda.image convert-1d-time->2d-image-interpolation
(defn interpolated-colorbar-mapper [{:keys [clip-min
                                            clip-max
                                            colorbar-data]}]
  (let [max-idx (double (dec (count colorbar-data)))
	diff (double (- clip-max clip-min))
        typed-colorbar-data (int-array colorbar-data)
        red-mask (int 0xff0000)
        green-mask (int 0xff00)
        blue-mask (int 0xff)
        int-256 (int 256)]
    (fn -set! [v]
      (let [value (double v)
            clip-val (double (if (> value clip-max)
                               clip-max
                               (if (< value clip-min)
                                 clip-min
                                 value)))
            cont-idx (double (unchecked-multiply (/ (unchecked-subtract clip-val clip-min)
                                                    diff)
                                                 max-idx))
            high-idx (Math/ceil cont-idx)
            low-idx (Math/floor cont-idx)
            p (int (unchecked-multiply (unchecked-subtract cont-idx low-idx)
                                       int-256))
            high-color (aget typed-colorbar-data high-idx)
            low-color (aget typed-colorbar-data low-idx)
            high-red (bit-and high-color red-mask)
            high-green (bit-and high-color green-mask)
            high-blue (bit-and high-color blue-mask)
            low-red (bit-and low-color red-mask)
            low-green (bit-and low-color green-mask)
            low-blue (bit-and low-color blue-mask)
            final-red (bit-and (quot (unchecked-add (unchecked-multiply p high-red)
                                                    (unchecked-multiply (unchecked-subtract int-256 p) low-red))
                                     int-256)
                               red-mask)
            final-green (bit-and (quot (unchecked-add (unchecked-multiply p high-green)
                                                      (unchecked-multiply (unchecked-subtract int-256 p) low-green))
                                       int-256)
                                 green-mask)
            final-blue (bit-and (quot (unchecked-add (unchecked-multiply p high-blue)
                                                     (unchecked-multiply (unchecked-subtract int-256 p) low-blue))
                                      int-256)
                                blue-mask)
            final-color (bit-or final-red final-green final-blue)]
        final-color))))

(defn convert-1d-time->2d-image-interpolated ^java.awt.image.BufferedImage [{:keys [clip-min
                                                                                    clip-max
                                                                                    output
                                                                                    colorbar-data
                                                                                    parameter]}]
  (let [magnitudes (if (and clip-min clip-max)
                     nil
                     (vec (mapcat (fn [node-data]
                                    (get node-data parameter))
                                  output)))
        clip-min (or clip-min (apply min magnitudes))
        clip-max (or clip-max (apply max magnitudes))
        v->c (interpolated-colorbar-mapper {:clip-min clip-min
                                            :clip-max clip-max
                                            :colorbar-data colorbar-data})
        x-dim (count output)
        y-dim (-> output
                  first
                  (get parameter)
                  count)
        bi (->buffered-image {:width x-dim
                              :height y-dim})
        int-zero (int 0)]
    (loop [x int-zero
           y int-zero]
      (if (< x x-dim)
        (if (< y y-dim)
          (do
            (.setRGB bi
                     x
                     y
                     (int (v->c (-> output
                                    (nth x)
                                    (get parameter)
                                    (nth y)))))
            (recur x
                   (unchecked-inc y)))
          (recur (unchecked-inc x)
                 int-zero))))
    bi))
