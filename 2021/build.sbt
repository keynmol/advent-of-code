import scala.scalanative.build.Mode
scalaVersion := "2.13.6"

libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29"

enablePlugins(ScalaNativePlugin)
mainClass := Some("Day12")

/* nativeConfig ~= {config => config.withMode(Mode.releaseFull)} */
