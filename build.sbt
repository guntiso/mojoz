lazy val dependencies = Seq(
  "org.yaml" % "snakeyaml" % "2.2",
  // test
 ("org.hsqldb" % "hsqldb" % "2.7.2"   % "test").classifier("jdk8"),
  "com.h2database" % "h2" % "2.2.224" % "test",
  "com.typesafe" % "config" % "1.4.3" % "it,test",                 // XXX POM fix - not in test scope
  "org.postgresql" % "postgresql" % "42.7.3" % "it,test",          // XXX POM fix - not in test scope
  "org.scalatest" %% "scalatest" % "3.2.18" % "it,test"
)

javacOptions ++= Seq("-source", "1.8", "-target", "1.8", "-Xlint")
initialize := {
  val _ = initialize.value
  val javaVersion = sys.props("java.specification.version")
  if (javaVersion != "1.8")
    sys.error("Java 1.8 is required for this project. Found " + javaVersion + " instead")
}

lazy val commonSettings = Seq(
  name := "mojoz",
  organization := "org.mojoz",
  scalaVersion := "2.13.13",
  crossScalaVersions := Seq(
    "2.13.13",
    "2.12.19",
    "2.11.12",
    "2.10.7"
  ),
  scalacOptions ++= Seq("-deprecation", "-feature"),
  resolvers ++= Seq(
    "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  ),
  libraryDependencies ++= dependencies,
)

ThisBuild / sbt.Keys.versionScheme := Some("pvp") // no semver here because of snakeyaml upgrade

lazy val mojoz = (project in file("."))
  .configs(IntegrationTest)
  .settings(commonSettings: _*)
  .settings(Defaults.itSettings: _*)
  .settings(
    Compile / unmanagedSourceDirectories ++= {
      val sharedSourceDir = (ThisBuild / baseDirectory).value / "compat"
      if (scalaVersion.value.startsWith("2.12.") ||
          scalaVersion.value.startsWith("2.11.") ||
          scalaVersion.value.startsWith("2.10."))
        Seq(sharedSourceDir / "scala-2.12")
      else Nil
    },
  )

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
