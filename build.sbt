name := "KaimereApp"

version := "0.2.0"

scalaVersion := "2.11.12"

resolvers += "spray repo" at "http://repo.spray.io"

val sprayVersion = "1.3.4"
libraryDependencies += "io.spray" %% "spray-json" % sprayVersion

resolvers ++= Seq(
  "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "sonatype releases" at "https://oss.sonatype.org/content/repositories/releases")

libraryDependencies += "org.rogach" %% "scallop" % "3.1.1"

libraryDependencies += "com.github.wol4aravio" %% "kaimere" % "0.5.0.0-SNAPSHOT"
