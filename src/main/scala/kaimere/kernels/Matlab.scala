package kaimere.kernels

import java.io.File
import java.lang.reflect.Method
import java.net.{URL, URLClassLoader}
import java.nio.file.{Path, Paths}

import spray.json._
import kaimere.real.objects.RealVector

object Matlab {

  var initialized: Boolean = false

  var engine: Object = null

  var start: Method = null
  var close: Method = null
  var eval: Method = null
  var getVariable: Method = null

  def initialize(engineLocation: String): Unit = {
    val jarFile = new File(engineLocation)
    val myClassLoader = new URLClassLoader(Array[URL](jarFile.toURL()))
    val engineClass = myClassLoader.loadClass("com.mathworks.engine.MatlabEngine")

    Matlab.start = engineClass.getMethod("startMatlab")
    Matlab.close = engineClass.getMethod("close")
    Matlab.eval = engineClass.getMethod("eval", classOf[String])
    Matlab.getVariable = engineClass.getMethod("getVariable", classOf[String])

    Matlab.engine = Matlab.start.invoke(null)
    Matlab.initialized = true
  }

  def terminate(): Unit = {
    if (Matlab.initialized) {
      Matlab.close.invoke(Matlab.engine)
      Matlab.initialized = false
    }
  }

  def eval(command: String): Unit =
    eval.invoke(engine, command)

  def getVariable(name: String): Double = {
    getVariable.invoke(engine, name).asInstanceOf[Double]
  }

  def loadSimulinkModel(model: String, jsonConfig: String): Simulink.Model  = {
    val json = scala.io.Source.fromFile(jsonConfig).mkString.parseJson.asJsObject
    val name = json.getFields("name")(0).asInstanceOf[JsString].value
    val criterionIntegral = json.getFields("criterionIntegral")(0).asInstanceOf[JsString].value
    val tunableBlocks = json.getFields("tunable")(0).asInstanceOf[JsArray]
    val blocks = tunableBlocks.elements
      .map { j =>
        val Seq(JsString(t), JsString(n), JsString(v)) = j.asJsObject.getFields("type", "name", "var")
        t match {
          case "Constant" => Simulink.Blocks.Constant(s"$name/$n", v)
          case _ => throw new Simulink.Exceptions.UnsupportedBlock(t)
        }
      }
    val outputs = json.getFields("outputs")(0).asInstanceOf[JsArray].elements.map(_.asInstanceOf[JsString].value)
    val terminalValue = json.getFields("terminalValue")(0).asInstanceOf[JsArray].elements.map(_.asInstanceOf[JsNumber].value.toDouble)
    val terminalPenalty = json.getFields("terminalPenalty")(0).asInstanceOf[JsArray].elements.map(_.asInstanceOf[JsNumber].value.toDouble)
    val terminalTolerance = json.getFields("terminalTolerance")(0).asInstanceOf[JsArray].elements.map(_.asInstanceOf[JsNumber].value.toDouble)
    val path = Paths.get(model).toAbsolutePath().toString()
    eval(s"load_system('$path')")
    Simulink.Model(name, outputs, terminalValue, terminalPenalty, terminalTolerance, criterionIntegral, blocks)
  }

  def unloadSimulinkModel(model: String): Unit = {
    eval(s"save_system('$model')")
    eval(s"close_system('$model')")
  }

}
