package kaimere.app

import org.rogach.scallop.ScallopConf
import spray.json._


object Verifier extends App {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val str = opt[String](required = true)
    verify()
  }

  override def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    main(conf)
  }

  def main(conf: Conf): Unit = {
    val parsed = Parser.handleOperators(conf.str())
    println(conf.str())
    println(parsed)
  }

  object Parser {

    def handleSeqOperator(str: String, operator: String, joiner: String): String = {

      val operatorRegex = s"$operator\\{([ A-Za-z0-9_\\+\\-\\*\\/\\(\\)]+),( )?(\\w+),( )?(\\d+),( )?(\\d+)\\}"

      def parseStr(func: String): String = {
        operatorRegex.r.replaceAllIn(func, m => {
          val repeat = m.group(1)
          val key = m.group(3)
          val start = m.group(5).toInt
          val end = m.group(7).toInt
          (start to end).map(i => "(" + repeat.replace(key, i.toString) + ")").mkString(s" $joiner ")
        })
      }

      parseStr(str)
    }

    def handleSums(str: String): String = handleSeqOperator(str, operator = "Sum", joiner = "+")

    def handleProds(str: String): String = handleSeqOperator(str, operator = "Prod", joiner = "*")

    def handleOperators(str: String): String = {

      def handle = (s: String) => handleSums(handleProds(s))

      val numberOfOperators = Seq("Sum", "Prod")
        .map(operator => str.sliding(operator.length, 1).count(_ == operator))
        .sum

      (1 to numberOfOperators).foldLeft(str) { case (s, _) => handle(s) }

    }


  }

}