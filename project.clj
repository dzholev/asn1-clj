(defproject third-assignment "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [javax.xml.bind/jaxb-api "2.3.0"]]
  :main ^:skip-aot asn1.parser
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
