lazy val dependencies = Seq(
  "org.yaml" % "snakeyaml" % "1.18",
  // test
  "org.hsqldb" % "hsqldb" % "2.5.0" % "test",
  "com.h2database" % "h2" % "1.4.200" % "test",
  "com.typesafe" % "config" % "1.2.0" % "it,test",                 // XXX POM fix - not in test scope
  "org.postgresql" % "postgresql" % "9.4.1212.jre7" % "it,test",   // XXX POM fix - not in test scope
  "org.scalatest" %% "scalatest" % "3.0.8" % "it,test"
)

lazy val commonSettings = Seq(
  name := "mojoz",
  organization := "org.mojoz",
  scalaVersion := "2.13.1",
  crossScalaVersions := Seq(
    "2.13.1",
    "2.12.11",
    "2.11.12",
    "2.10.7"
  ),
  scalacOptions ++= Seq("-deprecation", "-feature"),
  resolvers ++= Seq(
    "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  ),
  libraryDependencies ++= dependencies,
)

lazy val mojoz = (project in file("."))
  .configs(IntegrationTest)
  .settings(commonSettings: _*)
  .settings(Defaults.itSettings: _*)

autoAPIMappings := true

scalacOptions in (Compile, doc) ++= (baseDirectory in LocalProject("mojoz")).map {
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

publishArtifact in Test := false

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
