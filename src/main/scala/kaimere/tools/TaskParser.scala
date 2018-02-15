package kaimere.tools

import kaimere.real.objects._
import kaimere.real.optimization.general.OptimizationAlgorithm

object TaskParser {

  private def handleSeqOperator(str: String, operator: String, joiner: String): String = {

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

  private def handleSums(str: String): String = handleSeqOperator(str, operator = "Sum", joiner = "+")

  private def handleProds(str: String): String = handleSeqOperator(str, operator = "Prod", joiner = "*")

  def handleOperators(str: String): String = {

    def handle = (s: String) => handleSums(handleProds(s))

    val numberOfOperators = Seq("Sum", "Prod")
      .map(operator => str.sliding(operator.length, 1).count(_ == operator))
      .sum

    (1 to numberOfOperators).foldLeft(str) { case (s, _) => handle(s) }

  }

  def parseToTask(pathToTaskJson: String): (Function, OptimizationAlgorithm.Area, RealVector) = {



  }


}