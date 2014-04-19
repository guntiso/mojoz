import sbt._
import Keys._

object MojozBuild extends Build {
  lazy val mojoz =
    Project("mojoz", file("."))
      .configs(IntegrationTest)
      .settings(Defaults.itSettings : _*)
}
