package kaimere.app

import kaimere.tools.TaskParser
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
    val parsed = TaskParser.handleOperators(conf.str())
    println(conf.str())
    println(parsed)
  }

}