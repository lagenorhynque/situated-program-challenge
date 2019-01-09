(defproject rest-client "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[cheshire "5.8.1"]
                 [clj-http "3.9.1"]
                 [org.clojure/clojure "1.10.0"]]
  :main ^:skip-aot rest-client.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev {:plugins [[jonase/eastwood "0.3.4"]
                             [lein-eftest "0.5.4"]
                             [lein-cljfmt "0.6.3"]
                             [lein-kibit "0.1.6"]]
                   :aliases {"lint" ^{:doc "Execute cljfmt check, eastwood and kibit."}
                             ["do" ["cljfmt" "check"] ["eastwood"] ["kibit"]]}}})
