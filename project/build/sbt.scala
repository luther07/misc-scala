import sbt._

class MiscScala(info: ProjectInfo) extends DefaultProject(info) {

   val junit = "junit" % "junit" % "4.6"

   override def mainScalaSourcePath = "src"

   override def testScalaSourcePath = "test"

   val scalatest = "org.scalatest" %% "scalatest" % "1.6.1" % "test" 

}
