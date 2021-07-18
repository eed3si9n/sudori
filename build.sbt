import Dependencies._

ThisBuild / semanticdbEnabled := true
ThisBuild / scalaVersion := "3.0.1"

lazy val coreMacrosProj = (project in file("core-macros"))
  .settings(nocomma {
    name := "Core Macros"
    libraryDependencies ++= Seq(
      collectionsProj.cross(CrossVersion.for3Use2_13),
      verify % Test,
    )
    testFrameworks += new TestFramework("verify.runner.Framework")
  })
