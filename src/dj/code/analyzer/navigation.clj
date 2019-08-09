(ns
    ^{:dj.codeloading/dependencies '[[org.apache.xmlgraphics/batik-dom "1.9"]
                                     [org.apache.xmlgraphics/batik-swing "1.9"]]}
    dj.code.analyzer.navigation
  (:require [dj.code.analyzer :as anal]))

;; see http://www.graphviz.org/pdf/libguide.pdf for alternative, using
;; graphviz as library instead of trying to manipulate SVG

(def document-factory
  (org.apache.batik.anim.dom.SAXSVGDocumentFactory. (org.apache.batik.util.XMLResourceDescriptor/getXMLParserClassName)))

(defn main1 []
  (let [f (javax.swing.JFrame. "A Frame")
        s (org.apache.batik.swing.JSVGCanvas.)
        parser (org.apache.batik.util.XMLResourceDescriptor/getXMLParserClassName)
        df (org.apache.batik.anim.dom.SAXSVGDocumentFactory. parser)
        svg-str "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" height=\"100 \" width=\"100 \">
  <circle cx=\"50 \" cy=\"50 \" r=\"40 \" stroke=\"black \" stroke-width=\"3 \" fill=\"red \" />
  Sorry, your browser does not support inline SVG.
</svg>"
        document (.createSVGDocument df "" (java.io.ByteArrayInputStream. (.getBytes svg-str
                                                                                     java.nio.charset.StandardCharsets/UTF_8)))]
    (.setSVGDocument s document)
    (doto (.getContentPane f)
      (.add s))
    (doto f
      (.pack)
      (.setVisible true))))

#_ (do
     (require 'dj.codeloading.namespace)
     (dj.codeloading.namespace/accommodate-ns "dj.code.analyzer.navigation"))

