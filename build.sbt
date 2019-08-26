ThisBuild / organization := "teikametrics"
ThisBuild / scalaVersion := "2.12.8"

lazy val commonSettings = Seq(
  addCompilerPlugin("org.typelevel" % "kind-projector" % "0.10.3" cross CrossVersion.binary),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
)

lazy val slickMacros = (project in file("slick-macros"))
  .settings(
    commonSettings,
    name := "slick-macros",
    libraryDependencies ++= Seq(
      "com.typesafe.slick" %% "slick" % "3.3.0",
      "com.chuusai" %% "shapeless" % "2.3.3",
      scalaOrganization.value % "scala-reflect" % scalaVersion.value % Provided,
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
    scalacOptions --= Seq("-Ywarn-unused:patvars", "-Wunused:patvars"),
  )

lazy val ideadSupport = (project in file("idea-support"))
  .settings(
    commonSettings,
    name := "slick-macros-idea",
  )
