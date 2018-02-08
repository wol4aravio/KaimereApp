package kaimere.app

import org.rogach.scallop.ScallopConf
import spray.json._
import java.io._

import scala.io.Source
import kaimere.real.optimization.general.State
import kaimere.kernels.{Matlab, Simulink}
import kaimere.real.objects.RealVector

object Reporter extends App {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val log = opt[String](required = true)
    val matlabEngine = opt[String](required = true)
    val graphicsJson = opt[String](required = true)
    val width = opt[Int](default = Some(500))
    val height = opt[Int](default = Some(500))
    val delay = opt[Double](default = Some(0.1))
    val saveTo = opt[String](required = true)
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

  def extractStates(dir: File): Seq[State] = {
    val dirs = dir.listFiles().filter(_.isDirectory).sortBy(_.getName.toInt)
    val files = dir.listFiles().filter(d => !d.isDirectory && d.getName.matches("\\d+.json"))
    if (dirs.length == 0)
      files.sortBy(_.getName.dropRight(5).toInt).map(Source.fromFile(_).mkString.parseJson.convertTo[State])
    else
      dirs.foldLeft(Seq.empty[State]) { case (s, d) => s ++ extractStates(d)}
  }

  def parseGraphicsJson(json: JsValue): ((Int, Int), (String, String), Seq[(String, (Int, Int, Int, Int), Seq[(Option[String], String, String)])]) = {
    val Seq(JsNumber(rows), JsNumber(cols)) = json.asJsObject.getFields("rows", "cols")
    val Seq(JsString(criterionPosition), JsString(penaltyPosition)) = json.asJsObject.getFields("criterionPosition", "penaltyPosition")
    val Seq(JsArray(toPlot)) = json.asJsObject.getFields("toPlot")

    ((rows.toInt, cols.toInt),
      (criterionPosition, penaltyPosition),
      toPlot.map { jsValue_1 =>
        val Seq(JsString(position), JsArray(xLim), JsArray(yLim), JsArray(objects)) =
          jsValue_1.asJsObject.getFields("position", "xLim", "yLim", "objects")
        val Seq(xMin, xMax) = xLim.map(_.asInstanceOf[JsNumber].value.toInt)
        val Seq(yMin, yMax) = yLim.map(_.asInstanceOf[JsNumber].value.toInt)
        val objectsToDraw = objects.map { jsValue_2 =>
          val Seq(JsString(x), JsString(y), JsString(name)) = jsValue_2.asJsObject.getFields("x", "y", "name")
          (if (x == "null") Option.empty[String] else Some(x), y, name)
        }
        (position, (xMin, xMax, yMin, yMax), objectsToDraw)
      })
  }

  def drawGif(bestValues: Seq[RealVector], model: Simulink.Model,
              width: Int, height: Int,
              whatToDraw: ((Int, Int), (String, String), Seq[(String, (Int, Int, Int, Int), Seq[(Option[String], String, String)])]),
              areaCriterionPenalty: (Double, Double, Double, Double),
              delay: Double, filename: String, fontSize: Int = 16
             ): Unit = {

    val (rows, cols) = whatToDraw._1

    val (positionCriterion, positionPenalty) = whatToDraw._2
    val (minCriterion, maxCriterion, minPenalty, maxPenalty) = areaCriterionPenalty

    val subplot = whatToDraw._3.map(_._1)
    val limits = whatToDraw._3.map(_._2)
    val allAxes = whatToDraw._3.map(_._3)

    val numberOfPlots = subplot.size
    val numberOfSlides = bestValues.size


    Matlab.eval(s"slideShow = figure('Units', 'pixels', 'Position', [0, 0, $width, $height], 'PaperPositionMode', 'auto', 'Visible', 'off');")
    Matlab.eval("axis tight manual;")
    Matlab.eval(s"filename = '$filename';")
    bestValues.zipWithIndex.foreach { case (v, slideId) =>

      model(v)

      // Plot Criterion
      Matlab.eval(s"I_(${slideId + 1}) = criterion;")
      Matlab.eval(s"subplot($rows, $cols, $positionCriterion);")
      Matlab.eval(s"plot(1:${slideId + 1}, I_, 'LineWidth', 2);")
      Matlab.eval(s"xlim([1, $numberOfSlides]);")
      Matlab.eval(s"ylim([$minCriterion, $maxCriterion]);")
      Matlab.eval("a = get(gca, 'XTickLabel');")
      Matlab.eval(s"set(gca, 'XTickLabel', a, 'FontSize', $fontSize);")
      Matlab.eval(s"title('Criterion on Iteration #${slideId + 1}', 'FontSize', $fontSize);")

      // Plot Penalty
      Matlab.eval(s"P_(${slideId + 1}) = penalty;")
      Matlab.eval(s"subplot($rows, $cols, $positionPenalty);")
      Matlab.eval(s"plot(1:${slideId + 1}, P_, 'LineWidth', 2);")
      Matlab.eval(s"xlim([1, $numberOfSlides]);")
      Matlab.eval(s"ylim([$minPenalty, $maxPenalty]);")
      Matlab.eval("a = get(gca, 'XTickLabel');")
      Matlab.eval(s"set(gca, 'XTickLabel', a, 'FontSize', $fontSize);")
      Matlab.eval(s"title('Penalty on Iteration #${slideId + 1}', 'FontSize', $fontSize);")

      // Plot remaining objects
      Range(0, numberOfPlots).foreach { plotId =>
        val position = subplot(plotId)
        val axes = allAxes(plotId).map { case (x, y, name) =>
          if (x.isEmpty) (s"$y.Time", s"$y.Data", s"'$name'")
          else (s"${x.get}.Data", s"$y.Data", s"'name'")
        }
        val xAxis = axes.map(_._1)
        val yAxis = axes.map(_._2)
        val names = axes.map(_._3)
        val (xMin, xMax, yMin, yMax) = limits(plotId)
        Matlab.eval(s"subplot($rows, $cols, $position);")
        Matlab.eval(s"plot([${xAxis.mkString(", ")}], [${yAxis.mkString(", ")}], 'LineWidth', 2);")
        Matlab.eval(s"xlim([$xMin, $xMax]);")
        Matlab.eval(s"ylim([$yMin, $yMax]);")
        Matlab.eval("a = get(gca, 'XTickLabel');")
        Matlab.eval(s"set(gca, 'XTickLabel', a, 'FontSize', $fontSize);")
        Matlab.eval(s"lg = legend(${names.mkString(", ")}, 'Location', 'southoutside', 'Orientation', 'horizontal');")
        Matlab.eval(s"lg.FontSize = $fontSize;")
        Matlab.eval(s"title('Iteration: ${slideId + 1}', 'FontSize', $fontSize);")
      }

      // Save part of GIF
      Matlab.eval("drawnow")
      Matlab.eval("frame = getframe(slideShow);")
      Matlab.eval("im = frame2im(frame);")
      Matlab.eval("[imind, cm] = rgb2ind(im, 256);")
      Matlab.eval(s"if ($slideId == 0) imwrite(imind, cm, filename, 'gif', 'Loopcount', inf, 'DelayTime', $delay); else imwrite(imind, cm, filename, 'gif', 'WriteMode', 'append', 'DelayTime', $delay); end")
    }
  }

  def main(conf: Conf): Unit = {
    println("Initializing Matlab Engine")
    initialize(conf)

    val logDirectory = new File(conf.log())
    val runConfig = Source.fromFile(logDirectory.listFiles().filter(_.getName.endsWith(".json")).head).mkString.parseJson
    val Seq(JsString(simulinkModelSlx), JsString(simulinkModelJson)) = runConfig.asJsObject().getFields("simulinkModelSlx", "simulinkModelJson")

    println("Loading Simulink Model")
    val model = Matlab.loadSimulinkModel(
      model = simulinkModelSlx,
      jsonConfig = simulinkModelJson)

    println("Parsing states")
    val states = extractStates(logDirectory)

    println("Selecting best parameters from each state")
    var (minCriterion, maxCriterion) = (Double.PositiveInfinity, Double.NegativeInfinity)
    var (minPenalty, maxPenalty) = (Double.PositiveInfinity, Double.NegativeInfinity)
    val bestValues = states
      .map(_.getBestBy(model))
      .map { case (v, _) =>
        model(v)

        val criterion = Matlab.getVariable("criterion")
        if (criterion > maxCriterion) maxCriterion = criterion
        if (criterion < minCriterion) minCriterion = criterion

        val penalty = Matlab.getVariable("penalty")
        if (penalty > maxPenalty) maxPenalty = penalty
        if (penalty < minPenalty) minPenalty = penalty

        v
      }

    println("Making graphics")
    val graphicsJson = Source.fromFile(conf.graphicsJson()).mkString.parseJson
    drawGif(
      bestValues, model,
      conf.width(), conf.height(), parseGraphicsJson(graphicsJson),
      (minCriterion, maxCriterion, minPenalty, maxPenalty),
      conf.delay(), conf.saveTo())

    println("Terminating Matlab Engine")
    terminate(conf, simulinkModelSlx)
  }

}
