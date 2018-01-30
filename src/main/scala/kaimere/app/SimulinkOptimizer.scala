package kaimere.app

import org.rogach.scallop.ScallopConf
import spray.json._
import kaimere.kernels.Matlab
import kaimere.real.optimization.general.{Instruction, MetaOptimizationAlgorithm, OptimizationAlgorithm}
import kaimere.real.optimization.general.Instruction.{MaxTime, Verbose}

object SimulinkOptimizer extends App {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val matlabEngine = opt[String](required = true)
    val simulinkModelSlx = opt[String](required = true)
    val simulinkModelJson = opt[String](required = true)
    val optimizationTools = opt[List[String]](required = true)
    val targetVars = opt[List[String]](required = true)
    val maxTime = opt[List[String]](required = true)
    val area = opt[List[String]](required = true)
    val cycles = opt[Int](default = Some(1))
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

  def main(conf: Conf): Unit = {
    println("Initializing Matlab Engine")
    initialize(conf)

    println("Loading Simulink Model")
    val model = Matlab.loadSimulinkModel(
      model = conf.simulinkModelSlx(),
      jsonConfig = conf.simulinkModelJson())

    println("Initializing Optimization Tool")
    val optimizationTool =
      if (conf.optimizationTools().size > 1) {
        val cycles = conf.cycles()
        val algorithms = conf.optimizationTools().map(OptimizationAlgorithm.fromCsv)
        val targetVars: Seq[Option[Set[String]]] = conf.targetVars().map {
          {
            case "all" => Option.empty[Set[String]]
            case v => Some(v.split(",").toSet)
          }
        }
        val instructions = conf.maxTime()
          .zipWithIndex
          .map { case (t, id) => Verbose(algorithms(id), MaxTime(parseTime(t), verbose = true)) }
        MetaOptimizationAlgorithm(
          algorithms = (1 to cycles).foldLeft(Seq.empty[OptimizationAlgorithm]) { case (a, _) => a ++ algorithms },
          targetVars = (1 to cycles).foldLeft(Seq.empty[Option[Set[String]]]) { case (t, _) => t ++ targetVars },
          instructions = (1 to cycles).foldLeft(Seq.empty[Instruction]) { case (i, _) => i ++ instructions })
      }
      else {
        OptimizationAlgorithm.fromCsv(conf.optimizationTools().head)
      }

    val area = conf.area().map { str =>
      val Array(name, min, max) = str.split(':')
      name -> (min.toDouble, max.toDouble)
    }.toMap[String, (Double, Double)]
    optimizationTool.initialize(model, area)

    println("Working")
    val parameters =
      if (conf.optimizationTools().size > 1) optimizationTool.work(null)
      else optimizationTool.work(Verbose(optimizationTool, MaxTime(parseTime(conf.maxTime().head), verbose = true)))
    val result = model(parameters)
    println("Done\n")

    println("Optimal Parameters:")
    model.tunableBlocks.foreach(block => println(block.prettyPrint(parameters)))
    println("Criterion:")
    println(result)


    terminate(conf)
  }

}
