name := "subgraphSampling"

version := "1.0"

scalaVersion := "2.11.6"

resolvers += "twitter" at "http://maven.twttr.com"

libraryDependencies ++= Seq("com.twitter" %% "cassovary-core" % "5.0.0",
                            "com.typesafe.akka" %% "akka-stream-experimental" % "1.0-M5",
                            "com.quantifind" %% "wisp" % "0.0.4",
                            "it.unimi.dsi" % "fastutil" % "6.6.0",
                            "org.scalatest" %% "scalatest" % "2.2.3" % "test",
                            "org.scala-lang.modules" %% "scala-pickling" % "0.10.0",
                            "com.github.melrief" %% "purecsv" % "0.0.1"
)

javaOptions in run ++= Seq(
  "-Xms256M",
  "-Xmx6G")
