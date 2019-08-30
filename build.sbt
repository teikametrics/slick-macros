ThisBuild / organization := "teikametrics"
ThisBuild / scalaVersion := "2.12.8"
ThisBuild / licenses += ("MIT", url("http://opensource.org/licenses/MIT"))

lazy val commonSettings = Seq(
  addCompilerPlugin(
    "org.typelevel" % "kind-projector" % "0.10.3" cross CrossVersion.binary
  ),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)

lazy val slickMacros = (project in file("slick-macros"))
  .settings(
    commonSettings,
    name := "slick-macros",
    libraryDependencies ++= Seq(
      "com.typesafe.slick" %% "slick" % "3.3.0",
      "com.chuusai" %% "shapeless" % "2.3.3",
      scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided
    ),
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
    ),
    // `val q"... $_ ..." = tree` fires "unused pattern variables" warning
    scalacOptions --= Seq("-Ywarn-unused:patvars", "-Wunused:patvars")
  )

lazy val ideaSupport = (project in file("idea-support"))
  .settings(
    commonSettings,
    name := "slick-macros-idea",
    skip in publish := true // For now, until I implement it
  )

bintrayOrganization := Some("teikametrics")
bintrayRepository := "scala-public"

// This makes 'sbt publish' only stage the builds to bintray, not release them
// Run 'sbt bintrayRelease' after publishing to publicly release
ThisBuild / bintrayReleaseOnPublish := false
