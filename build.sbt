name := "KaimereApp"

version := "0.1"

scalaVersion := "2.11.12"

resolvers ++= Seq(
  "sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "sonatype releases" at "https://oss.sonatype.org/content/repositories/releases")

libraryDependencies += "com.github.wol4aravio" %% "kaimere" % "0.2.0-SNAPSHOT"