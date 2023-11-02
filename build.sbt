import Dependencies._
//import CompilerSettings._

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1-RC1-bin-20220904-b5fea82-NIGHTLY"
//ThisBuild / scalaVersion := "3.2.1"

ThisBuild / publishMavenStyle := true
//ThisBuild / publish / skip := true
ThisBuild / publishTo := Some(
  "GitHub Package Registry " at "https://maven.pkg.github.com/F1R3FLY-io/rhoHDC"
)
ThisBuild / credentials += Credentials(
  "GitHub Package Registry", // realm
  "maven.pkg.github.com", // host
  "leithaus", // user
              //sys.env.getOrElse("GITHUB_TOKEN", "abc123")
  "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" // password
)

//resolvers += Resolver.sonatypeOssRepos("snapshots")
//resolvers += Resolver.mavenRepo("GitHub Packages" at "https://maven.pkg.github.com/Owner/Repository")
resolvers += "GitHub Packages" at "https://maven.pkg.github.com/F1R3FLY-io/rhoHDC"


lazy val commonDependencies = Seq(
  //breeze,
  //breeze_viz
  macroloopCore,
  macroloopCollection,
  hyperVector_3
)

lazy val hdc = (project in file("hdc"))
//  .settings(commonSettings: _*)
//  .settings(bnfcSettings: _*)
  .settings(
    name := "hdc",
    scalacOptions ++= Seq(
      "-language:existentials",
      "-language:higherKinds",
      "-Xfatal-warnings",
      "-deprecation"
    ),
    /* Publishing Settings */
    // scmInfo := Some(
    //   ScmInfo(url("https://github.com/F1R3FLY-io/rhoHDC"), "git@github.com:F1R3FLY-io/rhoHDC.git")
    // ),
    // //git.remoteRepo := scmInfo.value.get.connection,
    // publishMavenStyle := true,
    // publishTo := Some("GitHub Package Registry" at "https://maven.pkg.github.com/leithaus/SNETPackages"),
    // credentials += Credentials(Path.userHome / ".sbt" / "1.0" / ".githubcredentials"),
    // Test/publishArtifact := false,
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
      "-Xfatal-warnings",
      "-deprecation"
    ),
    /* Publishing Settings */
    // scmInfo := Some(
    //   ScmInfo(url("https://github.com/F1R3FLY-io/rhoHDC"), "git@github.com:F1R3FLY-io/rhoHDC.git")
    // ),
    // //git.remoteRepo := scmInfo.value.get.connection,
    // publishMavenStyle := true,
    // publishTo := Some("GitHub Package Registry" at "https://maven.pkg.github.com/leithaus/SNETPackages"),
    // credentials += Credentials(Path.userHome / ".sbt" / "1.0" / ".githubcredentials"),
    // Test/publishArtifact := false,
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
      "-Xfatal-warnings",
      "-deprecation"
    ),
    /* Publishing Settings */
    // scmInfo := Some(
    //   ScmInfo(url("https://github.com/F1R3FLY-io/rhoHDC"), "git@github.com:F1R3FLY-io/rhoHDC.git")
    // ),
    // //git.remoteRepo := scmInfo.value.get.connection,
    // publishMavenStyle := true,
    // publishTo := Some("GitHub Package Registry" at "https://maven.pkg.github.com/leithaus/SNETPackages"),
    // credentials += Credentials(Path.userHome / ".sbt" / "1.0" / ".githubcredentials"),
    // Test/publishArtifact := false,
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
    libraryDependencies ++= commonDependencies,
    scalacOptions ++= Seq(
      "-language:existentials",
      "-language:higherKinds",
      "-Xfatal-warnings",
      "-deprecation"
    )
  ).aggregate( hdc, tinyrho, encoder )
