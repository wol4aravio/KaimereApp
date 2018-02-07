package kaimere.kernels

import kaimere.real.objects.RealVector
import kaimere.real.objects.Function

object Simulink {

  case class Model(name: String, state: Vector[String], control: Vector[String],
                   criterionIntegral: String, criterionTerminal: String,
                   terminalCondition: Vector[(String, Double, Double, Double)],
                   tunableBlocks: Vector[Blocks.Tunable], normCoeff: Double = 1.0) extends Function {
    override def apply(v: RealVector): Double = {
      tunableBlocks.foreach(_.tune(v))
      Matlab.eval(s"sim('$name');")

      val valueOfIntegralCriterion =
        if (criterionIntegral != "null") {
          Matlab.eval(s"criterionIntegral = $criterionIntegral.Data(end);")
          Matlab.getVariable("criterionIntegral")
        }
        else 0.0

      val valueOfTerminalCriterion =
        if (criterionTerminal != "null") {
          Matlab.eval(s"criterionTerminal = $criterionTerminal.Data(end);")
          Matlab.getVariable("criterionTerminal")
        }
        else 0.0

      val penalties = terminalCondition
        .map { case (stateName, idealValue, penalty, tolerance) =>
          Matlab.eval(s"$stateName = $stateName.Data(end);")
          val exactValue = Matlab.getVariable(s"$stateName")
          getPenalty(exactValue, idealValue, penalty, tolerance)
        }
      valueOfIntegralCriterion + valueOfTerminalCriterion + penalties.sum
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
