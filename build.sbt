import Dependencies._

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val commonDependencies = Seq(
  breeze,
  breeze_viz
)

lazy val hdc = (project in file("hdc"))
//  .settings(commonSettings: _*)
//  .settings(bnfcSettings: _*)
  .settings(
    name := "hdc",
    scalacOptions ++= Seq(
      "-language:existentials",
      "-language:higherKinds",
      "-Xfatal-warnings"
    ),
    /* Publishing Settings */
    scmInfo := Some(
      ScmInfo(url("https://github.com/F1R3FLY-io/rhoHDC"), "git@github.com:F1R3FLY-io/rhoHDC.git")
    ),
    //git.remoteRepo := scmInfo.value.get.connection,
    publishMavenStyle := true,
    publishTo := Some("GitHub Package Registry" at "https://maven.pkg.github.com/leithaus/SNETPackages"),
    credentials += Credentials(Path.userHome / ".sbt" / "1.0" / ".githubcredentials"),
    Test/publishArtifact := false,
    licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    homepage := Some(url("https://www.f1r3fly.io")),
    libraryDependencies ++= commonDependencies,
    Test/javaOptions ++= Seq("-Xss240k", "-XX:MaxJavaStackTraceDepth=10000", "-Xmx128m")
  )

lazy val tinyrho = (project in file("tinyrho"))
//  .settings(commonSettings: _*)
//  .settings(bnfcSettings: _*)
  .settings(
    name := "tinyrho",
    scalacOptions ++= Seq(
      "-language:existentials",
      "-language:higherKinds",
      "-Xfatal-warnings"
    ),
    /* Publishing Settings */
    scmInfo := Some(
      ScmInfo(url("https://github.com/F1R3FLY-io/rhoHDC"), "git@github.com:F1R3FLY-io/rhoHDC.git")
    ),
    //git.remoteRepo := scmInfo.value.get.connection,
    publishMavenStyle := true,
    publishTo := Some("GitHub Package Registry" at "https://maven.pkg.github.com/leithaus/SNETPackages"),
    credentials += Credentials(Path.userHome / ".sbt" / "1.0" / ".githubcredentials"),
    Test/publishArtifact := false,
    licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    homepage := Some(url("https://www.f1r3fly.io")),
    libraryDependencies ++= commonDependencies,
    Test/javaOptions ++= Seq("-Xss240k", "-XX:MaxJavaStackTraceDepth=10000", "-Xmx128m")
  )

lazy val encoder = (project in file("encoder"))
//  .settings(commonSettings: _*)
//  .settings(bnfcSettings: _*)
  .settings(
    name := "encoder",
    scalacOptions ++= Seq(
      "-language:existentials",
      "-language:higherKinds",
      "-Xfatal-warnings"
    ),
    /* Publishing Settings */
    scmInfo := Some(
      ScmInfo(url("https://github.com/F1R3FLY-io/rhoHDC"), "git@github.com:F1R3FLY-io/rhoHDC.git")
    ),
    //git.remoteRepo := scmInfo.value.get.connection,
    publishMavenStyle := true,
    publishTo := Some("GitHub Package Registry" at "https://maven.pkg.github.com/leithaus/SNETPackages"),
    credentials += Credentials(Path.userHome / ".sbt" / "1.0" / ".githubcredentials"),
    Test/publishArtifact := false,
    licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
    homepage := Some(url("https://www.f1r3fly.io")),
    libraryDependencies ++= commonDependencies,
    Test/javaOptions ++= Seq("-Xss240k", "-XX:MaxJavaStackTraceDepth=10000", "-Xmx128m")
  )
  .aggregate( hdc, tinyrho )
  .dependsOn( hdc, tinyrho )

lazy val root = (project in file("."))
  .settings(
    name := "rhoHDC",
    libraryDependencies ++= commonDependencies
  ).aggregate( hdc, tinyrho, encoder )
