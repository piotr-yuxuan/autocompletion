(defproject autocomplete "0.1.0-SNAPSHOT"
  :description "A same problem viewed two ways: list-processing and graph-processing"
  :url "https://github.com/piotr-yuxuan/autocompletion"
  :license {:name "GNU General Public License Version 3"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main autocomplete.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
