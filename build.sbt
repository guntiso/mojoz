name := "mojoz"

scalaVersion := "2.11.0"

scalacOptions ++= Seq("-deprecation", "-feature")

retrieveManaged := true

resolvers ++= Seq(
  "snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "org.yaml" % "snakeyaml" % "1.13",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
  // test
  "org.hsqldb" % "hsqldb" % "2.3.2" % "test",
  "com.typesafe" % "config" % "1.2.0" % "it",
  "org.postgresql" % "postgresql" % "9.3-1101-jdbc41" % "it",
  "org.tresql" %% "tresql" % "6.0-M2-SNAPSHOT" % "it,test",
  "org.scalatest" %% "scalatest" % "2.1.5" % "it,test"
)

scalacOptions in (Compile, doc) <++= (baseDirectory in
 LocalProject("mojoz")).map {
   bd => Seq("-sourcepath", bd.getAbsolutePath,
             "-doc-source-url", "https://github.com/guntiso/mojoz/blob/develop€{FILE_PATH}.scala")
 }
