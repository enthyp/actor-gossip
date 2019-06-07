name := "spy-chat"

version := "0.1"

scalaVersion := "2.12.8"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.21"
libraryDependencies += "com.typesafe.akka" %% "akka-remote" % "2.5.21" // for now, better use Cluster, they say
libraryDependencies += "com.typesafe.akka" %% "akka-cluster-tools" % "2.5.23"

// Also, check Akka IO for buffered TCP connections in client-server architecture.

/*
* Apart from akka-actors, probably also akka-cluster, cluster-publish-subscribe,
* akka-streams, akka-http? Shall see.
* */
