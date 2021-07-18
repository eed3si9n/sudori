import sbt._

object Dependencies {
  val sbtV = "1.5.4"
  val collectionsProj = "org.scala-sbt" % "collections" % sbtV

  val verify = "com.eed3si9n.verify" %% "verify" % "1.0.0"
}
