name := "stripe-scala"

version := "1.0.0"

organization := "com.stripe"

scalaVersion := "2.9.0-1"

crossScalaVersions := Seq("2.9.0", "2.9.0-1")

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "org.apache.httpcomponents" % "httpclient" % "4.1.+",
  "net.liftweb" %% "lift-json" % "2.+",
  "org.scalatest" %% "scalatest" % "1.6.1"
  )

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

publishTo <<= (version) { version: String =>
  val nexus = "http://nexus.scala-tools.org/content/repositories/"
  if (version.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus + "snapshots/")
  else                                   Some("releases"  at nexus + "releases/")
}
