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
