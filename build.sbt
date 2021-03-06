lazy val settings = Seq(
	name := "pamina"
	, organization := "de.ioswarm"
	, version := "0.0.1"
	, scalaVersion := "2.12.6"
	, scalacOptions ++= Seq(
		"-language:_"
		, "-unchecked"
		, "-deprecation"
		, "-encoding", "UTF-8"
	)
)

lazy val pamina = project.in(file("."))
	.settings(settings)
	.settings(
		name := "pamina"
	)

lazy val lib = new {
	object Version {
		val scalaTest = "3.0.5"
	}

	// test
	val scalaTest = "org.scalatest" %% "scalatest" % Version.scalaTest % Test

}

