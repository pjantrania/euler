import mill._, scalalib._
object euler extends ScalaModule {
  def scalaVersion = "2.13.4"

  object test extends Tests {
    def ivyDeps = Agg(
      ivy"org.scalactic::scalactic:3.2.2",
      ivy"org.scalatest::scalatest:3.2.2"
    )

    def testFrameworks = Seq("org.scalatest.tools.Framework")

    // https://github.com/lihaoyi/mill/issues/344#issuecomment-440902352
    // mill euler.test.testOne (TestClass)
    def testOne(args: String*) = T.command {
      super.runMain("org.scalatest.run", args: _*)
    }
  }
}
