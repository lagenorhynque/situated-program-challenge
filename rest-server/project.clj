(defproject rest-server "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :min-lein-version "2.8.1"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [duct/core "0.7.0"]
                 [duct/module.logging "0.4.0"]
                 [duct/module.web "0.7.0"]
                 [duct/module.ataraxy "0.3.0"]
                 [duct/module.sql "0.5.0"]
                 [org.postgresql/postgresql "42.2.5"]
                 [camel-snake-kebab "0.4.0"]
                 [honeysql "0.9.4"]]
  :plugins [[duct/lein-duct "0.11.1"]]
  :main ^:skip-aot rest-server.main
  :resource-paths ["resources" "target/resources"]
  :prep-tasks     ["javac" "compile" ["run" ":duct/compiler"]]
  :profiles
  {:dev  [:project/dev :profiles/dev]
   :repl {:prep-tasks   ^:replace ["javac" "compile"]
          :repl-options {:init-ns user}}
   :uberjar {:aot :all}
   :profiles/dev {}
   :project/dev  {:source-paths   ["dev/src"]
                  :resource-paths ["dev/resources"]
                  :dependencies   [[integrant/repl "0.3.1"]
                                   [eftest "0.5.4"]
                                   [kerodon "0.9.0"]
                                   [com.gearswithingears/shrubbery "0.4.1"]]
                  :plugins [[jonase/eastwood "0.3.4"]
                            [lein-eftest "0.5.4"]
                            [lein-cljfmt "0.6.3"]
                            [lein-kibit "0.1.6"]]
                  :aliases {"lint" ^{:doc "Execute cljfmt check, eastwood and kibit."}
                            ["do" ["cljfmt" "check"] ["eastwood" "{:source-paths [\"src\"]}"] ["kibit"]]}}})
