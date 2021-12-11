import scala.scalanative.build.Mode
scalaVersion := "2.13.6"

libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29"

enablePlugins(ScalaNativePlugin)

/* nativeConfig ~= {config => config.withMode(Mode.releaseFull)} */
