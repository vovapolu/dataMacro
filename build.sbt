// scala.meta macros are at the moment only supported in 2.11.
scalaVersion in ThisBuild := "2.11.8"

lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += Resolver.bintrayIvyRepo("scalameta", "maven"),
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-beta4" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  // temporary workaround for https://github.com/scalameta/paradise/issues/10
  scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  // temporary workaround for https://github.com/scalameta/paradise/issues/55
  sources in (Compile, doc) := Nil // macroparadise doesn't work with scaladoc yet.
)

// Define macros in this project.
lazy val dataMacro = project.settings(
  metaMacroSettings,
  // A dependency on scala.meta is required to write new-style macros, but not
  // to expand such macros.  This is similar to how it works for old-style
  // macros and a dependency on scala.reflect.
  libraryDependencies += "org.scalameta" %% "scalameta" % "1.4.0",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)
