package kaimere.app

import org.rogach.scallop.ScallopConf
import spray.json._
import java.io._
import scala.io.Source

import kaimere.kernels.Matlab

object Reporter extends App {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val matlabEngine = opt[String](required = true)
    val log = opt[String](required = true)
    verify()
  }

  override def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    main(conf)
  }

  def initialize(conf: Conf): Unit = {
    Matlab.initialize(conf.matlabEngine())
  }

  def terminate(conf: Conf, simulinkModelSlx: String): Unit = {
    Matlab.unloadSimulinkModel(simulinkModelSlx)
    Matlab.terminate()
  }

  def main(conf: Conf): Unit = {
//    println("Initializing Matlab Engine")
//    initialize(conf)

    val logDirectory = new File(conf.log())
    val runConfig = Source.fromFile(logDirectory.listFiles().filter(_.getName.endsWith(".json")).head).mkString.parseJson

    val simulinkModelSlx =

    println(runConfig.prettyPrint)


//    println("Terminating Matlab Engine")
//    terminate(conf, simulinkModelSlx)
  }

}
