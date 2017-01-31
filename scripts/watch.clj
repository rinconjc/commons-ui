(require '[cljs.build.api :as b])

(b/watch "src"
  {:main 'commons-ui.core
   :output-to "out/commons_ui.js"
   :output-dir "out"})
