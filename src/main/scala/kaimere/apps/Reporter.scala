package kaimere.apps

import org.rogach.scallop.ScallopConf
import spray.json._
import sys.process._
import java.io._

import scala.io.Source
import kaimere.real.optimization.general.State
import kaimere.kernels.{Matlab, Simulink}
import kaimere.real.objects.RealVector
import kaimere.real.optimization.general.instructions.StateLogger

object Reporter extends App {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val logFolders = opt[List[String]](required = true)
    val matlabEngine = opt[String](required = true)
    val simulinkModelSlx = opt[String](required = true)
    val simulinkModelJson = opt[String](required = true)
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

  def terminate(conf: Conf): Unit = {
    Matlab.unloadSimulinkModel(conf.simulinkModelSlx())
    Matlab.terminate()
  }

  def extractStates(dir: File): Seq[State] = {
    val dirs = dir.listFiles().filter(_.isDirectory).sortBy(_.getName.toInt)
    val files = dir.listFiles().filter(d => !d.isDirectory && d.getName.matches("\\d+.json"))
    if (dirs.length == 0)
      files.sortBy(_.getName.dropRight(5).toInt).map(Source.fromFile(_).mkString.parseJson.convertTo[State])
    else
      dirs.foldLeft(Seq.empty[State]) { case (s, d) => s ++ extractStates(d) }
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

  def makeGif(folder: String, delay: Double, save_to: String): Unit = {

    val libStream = getClass.getResourceAsStream("/gif_maker.py")
    val tmpFile = File.createTempFile("gif_maker_", ".py")
    tmpFile.createNewFile()

    val buffer = scala.io.Source.fromInputStream(libStream)
    val writer = new PrintWriter(tmpFile)
    buffer.getLines().foreach(l => writer.println(l))
    writer.close()

    Process(s"python ${tmpFile.getAbsoluteFile} --folder $folder --delay $delay --save_to $save_to") !

    tmpFile.deleteOnExit()

  }


  def drawGif(objectsToDraw: Seq[Map[String, (Seq[Double], Seq[Double])]],
              criterionValues: Seq[Double], penaltyValues: Seq[Double],
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

    val tmpFolder = new File(s"${filename}_images")
    if (tmpFolder.exists()) StateLogger.deleteFolder(tmpFolder)
    tmpFolder.mkdir()

    objectsToDraw.zipWithIndex.foreach { case (objects, slideId) =>

      println(s"Processing image ${slideId + 1}/${objectsToDraw.length}")

      // Image Initialization
      Matlab.eval(s"img = figure('Units', 'pixels', 'Position', [0, 0, $width, $height], 'PaperPositionMode', 'auto', 'Visible', 'off');")
      Matlab.eval("axis tight manual;")

      // Plot Criterion
      Matlab.eval(s"subplot($rows, $cols, $positionCriterion);")
      Matlab.eval(s"plot(1:${slideId + 1}, [${criterionValues.take(slideId + 1).mkString(", ")}], 'LineWidth', 2);")
      Matlab.eval(s"xlim([1, ${objectsToDraw.length}]);")
      Matlab.eval(s"ylim([$minCriterion, $maxCriterion]);")
      Matlab.eval("a = get(gca, 'XTickLabel');")
      Matlab.eval(s"set(gca, 'XTickLabel', a, 'FontSize', $fontSize);")
      Matlab.eval(s"title('Criterion on Iteration #${slideId + 1}', 'FontSize', $fontSize);")

      // Plot Penalty
      Matlab.eval(s"subplot($rows, $cols, $positionPenalty);")
      Matlab.eval(s"plot(1:${slideId + 1}, [${penaltyValues.take(slideId + 1).mkString(", ")}], 'LineWidth', 2);")
      Matlab.eval(s"xlim([1, ${objectsToDraw.length}]);")
      Matlab.eval(s"ylim([$minPenalty, $maxPenalty]);")
      Matlab.eval("a = get(gca, 'XTickLabel');")
      Matlab.eval(s"set(gca, 'XTickLabel', a, 'FontSize', $fontSize);")
      Matlab.eval(s"title('Penalty on Iteration #${slideId + 1}', 'FontSize', $fontSize);")

      // Plot remaining objects
      Range(0, numberOfPlots).foreach { plotId =>
        val objectMap = objects
        val position = subplot(plotId)
        val axes = allAxes(plotId).map { case (x, y, name) =>
          if (x.isEmpty) (s"[${objectMap(y)._1.mkString(", ")}]", s"[${objectMap(y)._2.mkString(", ")}]", s"'$name'")
          else (s"[${objectMap(x.get)._2.mkString(", ")}]", s"[${objectMap(y)._2.mkString(", ")}]", s"'$name'")
        }
        val xAxis = axes.map(_._1)
        val yAxis = axes.map(_._2)
        val names = axes.map(_._3)
        val (xMin, xMax, yMin, yMax) = limits(plotId)
        Matlab.eval(s"subplot($rows, $cols, $position);")
        Matlab.eval("hold on")
        xAxis.indices.foreach(id => Matlab.eval(s"plot(${xAxis(id)}, ${yAxis(id)}, 'LineWidth', 2);"))
        Matlab.eval("hold off")
        Matlab.eval(s"xlim([$xMin, $xMax]);")
        Matlab.eval(s"ylim([$yMin, $yMax]);")
        Matlab.eval("a = get(gca, 'XTickLabel');")
        Matlab.eval(s"set(gca, 'XTickLabel', a, 'FontSize', $fontSize);")
        Matlab.eval(s"lg = legend(${names.mkString(", ")}, 'Location', 'southoutside', 'Orientation', 'horizontal');")
        Matlab.eval(s"lg.FontSize = $fontSize;")
        Matlab.eval(s"title('Iteration: ${slideId + 1}', 'FontSize', $fontSize);")
      }

      // Save file
      Matlab.eval(s"saveas(img, '${filename}_images/${StateLogger.numToStr(slideId)}.tif')")
    }

    println("Merging to GIF")
    makeGif(s"${filename}_images", delay, filename)
  }

  def main(conf: Conf): Unit = {
    println("Initializing Matlab Engine")
    initialize(conf)

    println("Loading Simulink Model")
    val model = Matlab.loadSimulinkModel(
      model = conf.simulinkModelSlx(),
      jsonConfig = conf.simulinkModelJson())

    println("Parsing states")
    val states = conf.logFolders().map(f => extractStates(new File(f))).reduce(_ ++ _)

    println("Selecting best parameters from each state")
    val graphicsJson = Source.fromFile(conf.graphicsJson()).mkString.parseJson
    val whatToDraw = parseGraphicsJson(graphicsJson)
    val varsToExtract = whatToDraw._3
      .map(_._3)
      .reduce(_ ++ _)
      .map{ case (x, y, _) =>
          if (x.isDefined) Seq(x.get, y)
          else Seq(y)
      }
      .reduce(_ ++ _)
      .toSet

    val characteristics = states
      .map { state =>
        if (state.length == 1) state.toVectors().head
        else state.getBestBy(model)._1
      }
      .zipWithIndex
      .map { case (v, id) =>

        println(s"Processing state ${id + 1}/${states.length}")

        model(v)

        val criterion = Matlab.getVariable("criterion")
        val penalty = Matlab.getVariable("penalty")

        (criterion, penalty, varsToExtract.map{ varName => (varName, Matlab.getTimeSeries(varName)) }.toMap)
      }

    val criterionValues = characteristics.map(_._1)
    val penaltyValues = characteristics.map(_._2)

    val objectsToDraw = characteristics.map(_._3)

    println("Making graphics")
    drawGif(
      objectsToDraw, criterionValues, penaltyValues,
      conf.width(), conf.height(), whatToDraw,
      (criterionValues.min, criterionValues.max, penaltyValues.min, penaltyValues.max),
      conf.delay(), conf.saveTo())

    println("Terminating Matlab Engine")
    terminate(conf)
  }

}