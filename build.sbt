name := "ScalaWithCat"

version := "0.1"

scalaVersion := "2.13.3"

// https://mvnrepository.com/artifact/org.typelevel/cats-core
libraryDependencies += "org.typelevel" %% "cats-core" % "2.2.0"

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-language:higherKinds",
)