name := "ReplayDetector"

version := "0.1"

scalaVersion := "2.12.8"

// https://mvnrepository.com/artifact/com.amazonaws/aws-java-sdk
libraryDependencies += "com.amazonaws" % "aws-java-sdk" % "1.11.534"
// https://mvnrepository.com/artifact/commons-io/commons-io
libraryDependencies += "commons-io" % "commons-io" % "2.4"

assemblyMergeStrategy in assembly := {
  case PathList("reference.conf") => MergeStrategy.concat
  case PathList("META-INF", "MANIFEST.MF") => MergeStrategy.discard
  case x => MergeStrategy.first
}

javaCppPresetLibs ++= Seq(
  "ffmpeg" -> "3.2.1"
)

javaCppPresetLibs ++= Seq(
  "videoinput" -> "0.200"
)