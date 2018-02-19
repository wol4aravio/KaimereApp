package kaimere.apps

import org.rogach.scallop.ScallopConf
import java.io._

import spray.json._
import kaimere.kernels.Matlab
import kaimere.real.optimization.general.initializers.ExactInitializer
import kaimere.real.optimization.general.{MetaOptimizationAlgorithm, OptimizationAlgorithm}
import kaimere.real.optimization.general.instructions._

object SimulinkOptimizer_Old extends App {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val matlabEngine = opt[String](required = true)
    val simulinkModelSlx = opt[String](required = true)
    val simulinkModelJson = opt[String](required = true)
    val optimizationTools = opt[List[String]](required = true)
    val varsSets = opt[List[String]](required = true)
    val instructions = opt[List[String]](required = true)
    val cycles = opt[List[String]](required = true)
    val area = opt[List[String]](required = true)
    val log = opt[String](default = Option.empty[String])
    val verbose = opt[Boolean](default = Some(true))
    val bestOnly = opt[Boolean](default = Some(true))
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
    val toolsMap = conf.optimizationTools().map { str =>
      val Array(toolId, tool) = str.split(":")
      (toolId, OptimizationAlgorithm.fromCsv(tool))
    }.toMap[String, OptimizationAlgorithm]

    val varsSetsMap = conf.varsSets().map { str =>
      val Array(toolId, set) = str.split(":")
      (toolId, set.split(",") match {
        case Array("all") => Option.empty[Set[String]]
        case s => Some(s.toSet)
      })
    }.toMap[String, Option[Set[String]]]

    val instructionsMap = conf.instructions().map { str =>
      val Array(toolId, instruction) = str.split(":")
      if(conf.verbose()) (toolId, VerboseBest(GeneralInstruction.fromCsv(instruction)))
      else (toolId, GeneralInstruction.fromCsv(instruction))
    }.toMap[String, GeneralInstruction]


    val (metaTools, metaVars, metaInstructions) = conf.cycles().map { str =>
      val Array(toolsIds, setsId, instructionsIds, repeat) = str.split(";")
      val tools = toolsIds.split(">").map(id => toolsMap(id))
      val sets = setsId.split(">").map(id => varsSetsMap(id))
      val instructions = instructionsIds.split(">").map(id => instructionsMap(id))

      val repeatedTools = Range(0, repeat.toInt).foldLeft(Array.empty[OptimizationAlgorithm]) { case (seq, _) => seq ++ tools }
      val repeatedSets = Range(0, repeat.toInt).foldLeft(Array.empty[Option[Set[String]]]) { case (seq, _) => seq ++ sets }
      val repeatedInstructions = Range(0, repeat.toInt).foldLeft(Array.empty[GeneralInstruction]) { case (seq, _) => seq ++ instructions }

      (repeatedTools, repeatedSets, repeatedInstructions)
    }.reduce[(Array[OptimizationAlgorithm], Array[Option[Set[String]]], Array[GeneralInstruction])] { case (left, right) =>
      val (tools_1, sets_1, instructions_1) = left
      val (tools_2, sets_2, instructions_2) = right
      (tools_1 ++ tools_2, sets_1 ++ sets_2, instructions_1 ++ instructions_2)
    }

    val metaTool = MetaOptimizationAlgorithm(metaTools, metaVars, metaInstructions)

    val area = conf.area().map { str =>
      val Array(name, min, max) = str.split(':')
      name -> (min.toDouble, max.toDouble)
    }.toMap[String, (Double, Double)]

    metaTool.initialize(model, area, initializer = ExactInitializer(target = 0.0))

    println("Working")
    val optimalParameters =
      if (conf.log.isEmpty) metaTool.work(null)
      else {
        val targetFolder = new File(conf.log())
        if (targetFolder.exists()) StateLogger.deleteFolder(targetFolder)
        targetFolder.mkdir()
        metaTool.work(StateLogger(conf.log(), null, bestOnly = conf.bestOnly()))
      }

    val result = model(optimalParameters)
    println("Done\n")

    println("Optimal Parameters:")
    val outputJson = JsArray(model.tunableBlocks.map(block => block.toJson(optimalParameters)))
    println(outputJson.prettyPrint)
    println("Criterion:")
    println(result)

    if (conf.log.isDefined) {
      val configJson =
        s"""
           |{
           |   "simulinkModelSlx": "${conf.simulinkModelSlx()}",
           |   "simulinkModelJson": "${conf.simulinkModelJson()}",
           |   "algorithm": ${metaTool.toJson},
           |   "area": ${areaToJson(area)},
           |   "blocks": $outputJson
           |}
          """.stripMargin.parseJson
      val out = new BufferedWriter(new FileWriter(s"${conf.log()}/config.json"))
      out.write(configJson.prettyPrint)
      out.close()
    }

    println("Terminating Matlab Engine")
    terminate(conf)
  }

}
