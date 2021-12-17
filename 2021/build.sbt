import scala.scalanative.build.Mode
scalaVersion := "2.13.6"

enablePlugins(ScalaNativePlugin)
nativeCompileOptions += "-arch arm64"
nativeConfig ~= {
  _.withTargetTriple("arm64-apple-darwin20.6.0")
}

Compile / run / mainClass := Some("Day16")
Compile / mainClass := Some("Day16")
discoveredMainClasses := Seq("Day16")
mainClass := Some("Day16")
ThisBuild / mainClass := Some("Day16")
nativeLink / mainClass := Some("Day16")

/* nativeConfig ~= { config => config.withMode(Mode.releaseFast) } */
