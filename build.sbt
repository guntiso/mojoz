lazy val dependencies = Seq(
  "org.snakeyaml"  % "snakeyaml-engine" % "2.5",
  // test
 ("org.hsqldb"     % "hsqldb"     % "2.7.1"  %    "test").classifier("jdk8"),
  "com.h2database" % "h2" % "1.4.200" % "test",
  "com.typesafe"   % "config"     % "1.4.2"  % "it,test",           // XXX POM fix - not in test scope
  "org.postgresql" % "postgresql" % "42.5.0" % "it,test",          // XXX POM fix - not in test scope
  "org.scalatest" %% "scalatest"  % "3.2.14" % "it,test"
)

val scalaV = "2.13.10"
lazy val commonSettings = Seq(
  name := "mojoz",
  organization := "org.mojoz",
  scalaVersion := scalaV,
  crossScalaVersions := Seq(
    scalaV,
    "2.12.17",
  ),
  scalacOptions ++= Seq("-deprecation", "-feature"),
  resolvers ++= Seq(
    "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  ),
  libraryDependencies ++= dependencies,
)

ThisBuild / sbt.Keys.versionScheme := Some("semver-spec")
ThisBuild / versionPolicyIntention := Compatibility.BinaryCompatible

lazy val mojoz = (project in file("."))
  .configs(IntegrationTest)
  .settings(commonSettings: _*)
  .settings(Defaults.itSettings: _*)
  .settings(
    Compile / unmanagedSourceDirectories ++= {
      val sharedSourceDir = (ThisBuild / baseDirectory).value / "compat"
      if (scalaVersion.value startsWith "2.12.")
        Seq(sharedSourceDir / "scala-2.12")
      else Nil
    },
  )

autoAPIMappings := true

Compile / doc / scalacOptions ++= (LocalProject("mojoz") / baseDirectory).map {
   bd => Seq("-sourcepath", bd.getAbsolutePath,
             "-doc-source-url", "https://github.com/guntiso/mojoz/blob/develop€{FILE_PATH}.scala")
}.value

publishTo := {
  val v: String = version.value
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

Test / publishArtifact := false

pomIncludeRepository := { _ => false }

licenses := Seq("MIT" -> url("http://www.opensource.org/licenses/MIT"))

pomExtra := (
  <url>https://github.com/guntiso/mojoz</url>
  <scm>
    <url>git@github.com:guntiso/mojoz.git</url>
    <connection>scm:git:git@github.com:guntiso/mojoz.git</connection>
  </scm>
  <developers>
    <developer>
      <id>guntiso</id>
      <name>Guntis Ozols</name>
      <url>https://github.com/guntiso/</url>
    </developer>
  </developers>
)
