name := "mojoz"

organization := "org.mojoz"

scalaVersion := "2.11.8"

crossScalaVersions := Seq(
  "2.11.8",
  "2.10.6"
)

scalacOptions ++= Seq("-deprecation", "-feature")

retrieveManaged := true

resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.yaml" % "snakeyaml" % "1.13",
  // test
  "org.hsqldb" % "hsqldb" % "2.3.2" % "test",
  "com.h2database" % "h2" % "1.4.178" % "test",
  "com.typesafe" % "config" % "1.2.0" % "it,test",                 // XXX POM fix - not in test scope
  "org.postgresql" % "postgresql" % "9.3-1101-jdbc41" % "it,test", // XXX POM fix - not in test scope
  "org.tresql" %% "tresql" % "6.0-M17-SNAPSHOT" % "it,test",
  "org.scalatest" %% "scalatest" % "2.1.5" % "it,test"
)

scalacOptions in (Compile, doc) <++= (baseDirectory in
 LocalProject("mojoz")).map {
   bd => Seq("-sourcepath", bd.getAbsolutePath,
             "-doc-source-url", "https://github.com/guntiso/mojoz/blob/develop€{FILE_PATH}.scala")
 }

publishTo <<= version { v: String =>
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
