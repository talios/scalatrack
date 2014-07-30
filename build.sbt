import SonatypeKeys._

sonatypeSettings

scalaVersion := "2.11.2"

organization := "com.theoryinpractise"

name := "scalatrack"

version := "1.1-SNAPSHOT"

libraryDependencies ++= Seq(
  "com.typesafe.scala-logging" %% "scala-logging" % "3.0.0",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.1",
  "org.scala-stm" %% "scala-stm" % "0.7",
  "org.json4s" %% "json4s-native" % "3.2.9",
  "org.ocpsoft.prettytime" % "prettytime" % "3.2.5.Final")

pomExtra := {
  <url>http://github.com/talios/scalatrack</url>
    <licenses>
    <license>
      <name>Apache 2</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.txt</url>
    </license>
  </licenses>
  <scm>
    <connection>scm:git:github.com/talios/scalatrack</connection>
    <developerConnection>scm:git:git@github.com:talios/scalatrack</developerConnection>
    <url>github.com/talios/scalatrack</url>
  </scm>
    <developers>
    <developer>
      <id>talios</id>
      <name>Mark Derricutt</name>
      <url>http://www.theoryinpractice.net</url>
    </developer>
  </developers>
}

