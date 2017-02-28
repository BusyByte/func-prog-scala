organization := "net.nomadicalien"

name := "func-prog-scala"

version := "2.0.0-SNAPSHOT"

scalaVersion := "2.12.1"

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.8.8" % Test)

scalacOptions in Test ++= Seq("-Yrangepos")

