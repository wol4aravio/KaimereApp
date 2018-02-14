package kaimere.app

import org.rogach.scallop.ScallopConf
import spray.json._


object Verifier extends App {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val log = opt[String](required = true)
    verify()
  }

  override def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    main(conf)
  }

  def main(conf: Conf): Unit = {

  }


}