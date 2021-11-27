import scala.scalanative.build.Mode
scalaVersion := "2.13.6"

// Set to false or remove if you want to show stubs as linking errors
/* nativeLinkStubs := true */
mainClass := Some("What")

enablePlugins(ScalaNativePlugin)

/* nativeConfig ~= {config => config.withMode(Mode.releaseFull)} */
