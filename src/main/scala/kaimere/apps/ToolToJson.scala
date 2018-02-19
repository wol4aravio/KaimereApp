package kaimere.apps

import java.io.{BufferedWriter, FileWriter}

import kaimere.real.optimization.general.{MetaOptimizationAlgorithm, OptimizationAlgorithm}
import kaimere.real.optimization.general.instructions.GeneralInstruction
import org.rogach.scallop.ScallopConf
import spray.json._

object ToolToJson extends App {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val algorithm = opt[String](required = true)
    val parts = opt[List[String]]()
    val varsSets = opt[List[String]]()
    val instructions = opt[List[String]]()
    val cycles = opt[List[String]]()
    val saveTo = opt[String](required = true)
    verify()
  }

  override def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    main(conf)
  }

  def main(conf: Conf): Unit = {

    val json = conf.algorithm() match {
      case "meta" => {
        val (metaTools, metaVars, metaInstructions) = conf.cycles().map { str =>

          val toolsMap = conf.parts().map { str =>
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
            (toolId, GeneralInstruction.fromCsv(instruction))
          }.toMap[String, GeneralInstruction]


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

        metaTool.toJson
      }
      case csv => OptimizationAlgorithm.toJson(OptimizationAlgorithm.fromCsv(csv))
    }

    val out = new BufferedWriter(new FileWriter(conf.saveTo()))
    out.write(json.prettyPrint)
    out.close()


  }

}
