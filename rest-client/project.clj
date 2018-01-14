(defproject rest-client "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[cheshire "5.8.0"]
                 [clj-http "3.7.0"]
                 [org.clojure/clojure "1.9.0"]]
  :main ^:skip-aot rest-client.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:plugins [[jonase/eastwood "0.2.5"]
                             [lein-eftest "0.4.1"]
                             [lein-cljfmt "0.5.7"]
                             [lein-kibit "0.1.6"]]
                   :aliases {"lint" ^{:doc "Execute cljfmt check, eastwood and kibit."}
                             ["do" ["cljfmt" "check"] ["eastwood"] ["kibit"]]}}})
