{:user {:plugins [[lein-ancient "0.6.15"]
                  [lein-kibit "0.1.6"]]}
 :repl {:plugins [[cider/cider-nrepl "0.17.0"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.13"]
                       [com.cemerick/piggieback "0.2.2"]
                       [figwheel-sidecar "0.5.16"]]
        :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}}}
