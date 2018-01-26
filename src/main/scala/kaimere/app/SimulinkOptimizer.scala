package kaimere.app

import org.rogach.scallop.ScallopConf
import spray.json._

import kaimere.kernels.Matlab
import kaimere.real.optimization.general.OptimizationAlgorithm
import kaimere.real.optimization.general.OptimizationAlgorithm.MergeStrategy
import kaimere.real.optimization.general.Instruction.MaxTime

object SimulinkOptimizer extends App {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val matlabEngine = opt[String](required = true)
    val simulinkModelSlx = opt[String](required = true)
    val simulinkModelJson = opt[String](required = true)
    val optimizationTool = opt[String](required = true)
    val area = opt[List[String]](required = true)
    val time = opt[Double](required = true)
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

  def main(conf: Conf): Unit = {
    println("Initializing Matlab Engine")
    initialize(conf)

    println("Loading Simulink Model")
    val model = Matlab.loadSimulinkModel(
      model = conf.simulinkModelSlx(),
      jsonConfig = conf.simulinkModelJson())

    println("Initializing Optimization Tool")
    val optimizationTool = OptimizationAlgorithm(scala.io.Source.fromFile(conf.optimizationTool()).mkString.parseJson)
    val area = conf.area().map { str =>
      val Array(name, min, max) = str.split(':')
      name -> (min.toDouble, max.toDouble)
    }.toMap[String, (Double, Double)]
    optimizationTool.initialize(model, area, Vector(area.mapValues { case (min, max) => 0.5 * (min + max)}), MergeStrategy.force)

    println("Working")
    val parameters = optimizationTool.work(MaxTime(conf.time(), verbose = true))
    val result = model(parameters)
    println("Done\n")

    println("Optimal Parameters:")
    println(parameters.vals)
    println("Criterion:")
    println(result)


    terminate(conf)
  }

}
