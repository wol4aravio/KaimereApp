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

    abstract class Tunable(name: String) {
      def extract(v: RealVector): String
      def tune(v: RealVector): Unit
      def prettyPrint(v: RealVector): String = s"$name: ${extract(v)}"
    }

    case class Constant(name: String, parameterName: String) extends Tunable(name) {

      override def extract(v: RealVector): String = {v(parameterName)}.toString

      override def tune(v: RealVector): Unit = Matlab.eval(s"set_param('$name', 'Value', num2str(${extract(v)}))")

    }

    case class RepeatingSequenceInterpolated(name: String, prefix: String, numberOfParameters: Int) extends Tunable(name) {

      override def extract(v: RealVector): String = {
        val selectedVars = Range(0, numberOfParameters).map(key => v(s"${prefix}_${key.toString}"))
        s"[${selectedVars.mkString(", ")}]"
      }

      override def tune(v: RealVector): Unit = Matlab.eval(s"set_param('$name', 'OutValues', '${extract(v)}')")

    }

  }

}
