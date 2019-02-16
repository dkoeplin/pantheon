package ichor

import ichor.core.Config

object Main {

  def main(args: Array[String]): Unit = {
    val file = "test/test.y"
    val parser = new ichor.ast.Parser
    val body = parser(file).getOrElse(sys.exit(-1))

    val config = new Config
    val codePrinter = ichor.ast.CodePrinter(config)
    val astPrinter  = ichor.ast.ASTPrinter(config)

    codePrinter.run(body)
    astPrinter.run(body)
  }

}
