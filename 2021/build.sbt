import scala.scalanative.build.Mode
scalaVersion := "2.13.6"

/* libraryDependencies += "org.scalameta" %%% "munit" % "0.7.29" */

enablePlugins(ScalaNativePlugin)
/* nativeLink / mainClass := Some("Day15") */
nativeCompileOptions += "-arch arm64"
nativeConfig ~= {
  _.withTargetTriple("arm64-apple-darwin20.6.0")
}

nativeConfig ~= {config => config.withMode(Mode.releaseFast)}
