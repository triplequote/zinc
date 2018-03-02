/**
 * Copyright (C) 2012 Typesafe, Inc. <http://www.typesafe.com>
 */

import sbt._
import sbt.Keys._

object Publish {
  val publishLocally = settingKey[Boolean]("publish-locally")

  lazy val settings: Seq[Setting[_]] = Seq(
    publishMavenStyle := true,
    publishLocally := false,
    publishTo := Some(
      if (publishLocally.value) Opts.resolver.mavenLocalFile
      else "Triplequote Repo" at "https://repo.triplequote.com/artifactory/libs-release"),
//      else if (isSnapshot.value) Opts.resolver.sonatypeSnapshots
//      else Opts.resolver.sonatypeStaging),
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    publishArtifact in Test := false,
    homepage := Some(url("https://github.com/typesafehub/zinc")),
    licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    pomExtra := {
      <scm>
        <url>https://github.com/typesafehub/zinc</url>
        <connection>scm:git:git@github.com:triplequote/zinc.git</connection>
      </scm>
      <developers>
        <developer>
          <id>harrah</id>
          <name>Mark Harrah</name>
          <url>https://github.com/harrah</url>
        </developer>
        <developer>
          <id>pvlugter</id>
          <name>Peter Vlugter</name>
          <url>https://github.com/pvlugter</url>
        </developer>
        <developer>
          <id>dragos</id>
          <name>Iulian Dragos</name>
          <url>https://github.com/dragos</url>
        </developer>
      </developers>
    },
    pomIncludeRepository := { _ => false }
  )
}
