{:user {:plugins [[refactor-nrepl "2.4.0"]
                  [cider/cider-nrepl "0.20.0"]
                  [lein-ancient "0.6.15"]
                  [lein-kibit "0.1.6"]]
        :dependencies [[org.clojure/tools.nrepl "0.2.13"]
                       [com.cemerick/piggieback "0.2.2"]
                       [figwheel-sidecar "0.5.16"]]
        :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
        :figwheel {:nrepl-middleware ["cider.nrepl/cider-middleware" "cemerick.piggieback/wrap-cljs-repl"]}}}
