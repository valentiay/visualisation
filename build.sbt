name         in ThisBuild := "visualisation"
version      in ThisBuild := "0.1"
scalaVersion in ThisBuild := "2.13.5"

lazy val commonSettings =
  List(
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0",
    scalaJSUseMainModuleInitializer := true,
  )

lazy val task1 =
  project.settings(commonSettings)

lazy val task2 =
  project.settings(commonSettings)
