(defproject
  provisdom/test
  "0.2.0"
  :plugins
  [[s3-wagon-private "1.2.0"]]
  :dependencies
  [[org.clojure/clojure "1.8.0"]
   [provisdom/boot-tasks "0.6.0" :scope "test"]
   [adzerk/boot-test "1.1.1" :scope "test"]
   [incanter "1.5.7"]]
  :repositories
  [["clojars" "http://clojars.org/repo/"]
   ["maven-central" "http://repo1.maven.org/maven2/"]
   ["provisdom"
    {:username :env/aws_access_key,
     :passphrase :env/aws_secret_key,
     :url "s3p://provisdom-artifacts/releases/"}]]
  :source-paths
  ["src" "resources" "test"])