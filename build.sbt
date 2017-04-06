
lazy val scalazVersion                = "7.2.10"
lazy val specs2Version                = "3.8.8"

lazy val commonSettings = Seq(
  organization := "net.nomadicalien",
  scalaVersion := "2.12.1",
  version := "2.0.0-SNAPSHOT",
  libraryDependencies ++= Seq(
    "org.specs2" %% "specs2-core" % specs2Version % Test
  ),
  scalacOptions in Test ++= Seq("-Yrangepos")
)

lazy val aggregate = (project in file("."))
  .settings(name := "func-prog-scala-aggregate")
  .settings(commonSettings: _*)
  .aggregate(`exercises-and-notes`, `lightning-talk`)

lazy val `exercises-and-notes` = (project in file("exercises-and-notes"))
  .settings(commonSettings: _*)
  .settings(
    name := "func-prog-scala-exercises-and-notes"
  )

lazy val `lightning-talk` = (project in file("lightning-talk"))
  .settings(commonSettings: _*)
  .settings(
    name := "func-prog-scala-lightning-talk",
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % scalazVersion
    )

  )