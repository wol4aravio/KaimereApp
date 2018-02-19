package kaimere.apps

import org.rogach.scallop.ScallopConf
import java.io._

import scala.io.Source
import spray.json._
import kaimere.kernels.Matlab
import kaimere.real.optimization.general.initializers.{ExactInitializer, PureRandomInitializer}
import kaimere.real.optimization.general.{MetaOptimizationAlgorithm, OptimizationAlgorithm, State}
import kaimere.real.optimization.general.instructions._
import kaimere.tools.etc._


object SimulinkOptimizer extends App {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val matlabEngine = opt[String](required = true)
    val simulinkModelSlx = opt[String](required = true)
    val simulinkModelJson = opt[String](required = true)
    val optimizationToolJson = opt[String](required = true)
    val instruction = opt[String]()
    val initialState = opt[String](default = Option.empty[String])
    verify()
  }

  override def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    main(conf)
  }

  def initialize(conf: Conf): Unit = {
    Matlab.initialize(conf.matlabEngine())
  }

  def terminate(conf: Conf): Unit = {
    Matlab.unloadSimulinkModel(conf.simulinkModelSlx())
    Matlab.terminate()
  }

  def parseTime(str: String): Double = {
    val Array(hours, minutes, seconds) = str.split(":").map(_.toInt)
    60.0 * (60.0 * hours + minutes) + seconds
  }

  def areaToJson(area: OptimizationAlgorithm.Area): JsValue = {
    JsArray(area.map { case (key, (min, max)) =>
      JsObject("key" -> JsString(key), "min" -> JsNumber(min), "max" -> JsNumber(max))}
      .toVector)
  }

  def main(conf: Conf): Unit = {
    println("Initializing Matlab Engine")
    initialize(conf)

    println("Loading Simulink Model")
    val model = Matlab.loadSimulinkModel(
      model = conf.simulinkModelSlx(),
      jsonConfig = conf.simulinkModelJson())

    println("Initializing Optimization Tools")
    val optimizationTool = Source.fromFile(conf.optimizationToolJson()).getLines().mkString("\n").parseJson |> OptimizationAlgorithm.fromJson
    val instruction =
      if(conf.instruction.isEmpty) null
      else Instruction.fromCsv(conf.instruction())

    val initialState =
      if (conf.initialState.isEmpty) None
      else Some(Source.fromFile(conf.initialState()).getLines().mkString("\n").parseJson.convertTo[State])

    println("Working")
    optimizationTool.initialize(model, model.parameterArea, state = initialState, initializer = PureRandomInitializer())
    val optimalParameters = optimizationTool.work(instruction)

    val result = model(optimalParameters)
    println("Done\n")

    println("Optimal Parameters:")
    val outputJson = JsArray(model.tunableBlocks.map(block => block.toJson(optimalParameters)))
    println(outputJson.prettyPrint)
    println("Criterion:")
    println(result)

    val logJson =
      s"""
         |{
         |   "simulinkModelSlx": "${conf.simulinkModelSlx()}",
         |   "simulinkModelJson": "${conf.simulinkModelJson()}",
         |   "algorithm": ${OptimizationAlgorithm.toJson(optimizationTool)},
         |   "blocks": $outputJson
         |}
        """.stripMargin.parseJson
    val out = new BufferedWriter(new FileWriter("log.json"))
    out.write(logJson.prettyPrint)
    out.close()

    println("Terminating Matlab Engine")
    terminate(conf)
  }

}
