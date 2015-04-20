name := "ScalaLabs-solutions"

version := "1.0"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-unchecked", "-deprecation")


resolvers += "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
	"commons-io" % "commons-io" % "2.4",
  "org.mongodb" %% "casbah" % "2.7.3",
  "com.typesafe" % "config" % "0.4.0",
  "org.reactivemongo" %% "reactivemongo" % "0.10.5.0.akka23",
  "com.quantifind" %% "sumac" % "0.3.0"
)

