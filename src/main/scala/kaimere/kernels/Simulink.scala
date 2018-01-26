package kaimere.kernels

import kaimere.real.objects.RealVector
import kaimere.real.objects.Function

object Simulink {

  case class Model(name: String, outputs: Vector[String],
                   terminalValue: Vector[Double], terminalPenalty: Vector[Double], terminalTolerance: Vector[Double],
                   criterionIntegral: String, tunableBlocks: Vector[Blocks.Tunable], normCoeff: Double = 2.0) extends Function {
    override def apply(v: RealVector): Double = {
      tunableBlocks.foreach(_.tune(v))
      Matlab.eval(s"sim('$name');")
      Matlab.eval(s"criterionIntegral = $criterionIntegral.Data(end);")
      val valueOfIntegralCriterion = Matlab.getVariable("criterionIntegral")
      val penalties = outputs.zipWithIndex
        .map { case (varName, id) =>
          Matlab.eval(s"$varName = $varName.Data(end);")
          val exactValue = Matlab.getVariable(s"$varName")
          val idealValue = terminalValue(id)
          val penalty = terminalPenalty(id)
          val tolerance = terminalTolerance(id)
          getPenalty(exactValue, idealValue, penalty, tolerance)
        }
      valueOfIntegralCriterion + penalties.sum
    }

    def getPenalty(exactValue: Double, idealValue: Double,
                   penalty: Double, tolerance: Double): Double = {
      val delta = math.abs(idealValue - exactValue)
      math.pow(penalty * delta, normCoeff) * (if (delta < tolerance) 0 else 1)
    }
  }

  object Exceptions {

    class UnsupportedBlock(name: String) extends Exception

  }

  object Blocks {

    abstract class Tunable(parameterName: String) {
      def tune(v: RealVector): Unit
    }

    case class Constant(name: String, parameterName: String) extends Tunable(parameterName) {
      override def tune(v: RealVector): Unit = Matlab.eval(s"set_param('$name', 'Value', num2str(${v(parameterName)}))")
    }

  }

}
