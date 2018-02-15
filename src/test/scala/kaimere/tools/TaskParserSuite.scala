package kaimere.tools

import org.scalatest.FunSuite

class TaskParserSuite extends FunSuite {

  test("Sum/Prod Parser") {

    val input = "Prod{Sum{x_k_j, _k,1, 5},_j, 1, 3}"
    val output = "((x11) + (x21) + (x31) + (x41) + (x51)) * ((x12) + (x22) + (x32) + (x42) + (x52)) * ((x13) + (x23) + (x33) + (x43) + (x53))"

    assert(TaskParser.handleOperators(input) == output)

  }

}
