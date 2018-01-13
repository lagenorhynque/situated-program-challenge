(defproject rest-server "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [duct/core "0.6.2"]
                 [duct/module.logging "0.3.1"]
                 [duct/module.web "0.6.4"]
                 [duct/module.ataraxy "0.2.0"]
                 [duct/module.sql "0.4.2"]
                 [org.postgresql/postgresql "42.1.4"]
                 [camel-snake-kebab "0.4.0"]
                 [honeysql "0.9.1"]]
  :plugins [[duct/lein-duct "0.10.6"]]
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
                  :dependencies   [[integrant/repl "0.3.0"]
                                   [eftest "0.4.1"]
                                   [kerodon "0.9.0"]
                                   [com.gearswithingears/shrubbery "0.4.1"]]
                  :plugins [[jonase/eastwood "0.2.5"]
                            [lein-eftest "0.4.1"]
                            [lein-cljfmt "0.5.7"]
                            [lein-kibit "0.1.6"]]
                  :aliases {"lint" ^{:doc "Execute cljfmt check, eastwood and kibit."}
                            ["do" ["cljfmt" "check"] ["eastwood" "{:source-paths [\"src\"]}"] ["kibit"]]}}})
