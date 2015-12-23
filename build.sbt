scalaVersion := "2.11.7"

// Read here for optional jars and dependencies
libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.6.6" % "test")

// Read here for optional jars and dependencies
libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-actor" % "2.4.1")
libraryDependencies ++= Seq("com.typesafe.akka" % "akka-stream-experimental_2.11" % "2.0")
libraryDependencies ++= Seq("com.softwaremill.reactivekafka" %% "reactive-kafka-core" % "0.8.3")




scalacOptions in Test ++= Seq("-Yrangepos")