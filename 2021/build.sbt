import scala.scalanative.build.Mode
scalaVersion := "2.13.6"

enablePlugins(ScalaNativePlugin)
nativeCompileOptions += "-arch arm64"
/* nativeConfig ~= { */
/*   _.withTargetTriple("arm64-apple-darwin20.6.0") */
/* } */

val d = "Day18"

Compile / run / mainClass := Some(d)
Compile / mainClass := Some(d)
discoveredMainClasses := Seq(d)
mainClass := Some(d)
ThisBuild / mainClass := Some(d)
nativeLink / mainClass := Some(d)

/* nativeConfig ~= { config => config.withMode(Mode.releaseFast) } */
