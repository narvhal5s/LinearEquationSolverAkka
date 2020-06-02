package pl.edu.pw

import akka.actor.{ActorSystem, Props}

import scala.io.Source

object LinearEquationMain extends App {
  def initMatrix(nRows: Int, nCols: Int) = Array.tabulate(nRows, nCols)((x, y) => 0f)
  val filename = "src/main/resources/example_equation.txt"
  val file = Source.fromFile(filename)
  val equationSize: Int = file.getLines.next().toInt
  var A = Array.ofDim[Float](equationSize, equationSize)
  var B: Array[Float] = Array.ofDim[Float](equationSize)
  println(equationSize)
  println()
  var lineNumber = 0;
  var rowNumber = 0;
  for (line <- file.getLines) {
    rowNumber = 0;
    for (value <- line.split(" ")) {
      if (rowNumber == equationSize) {
        B(lineNumber) = value.toFloat
      } else {
        A(lineNumber)(rowNumber) = value.toFloat
      }
      rowNumber = rowNumber + 1
    }
    lineNumber = lineNumber + 1
  }
  file.close()


  val system = ActorSystem("HelloSystem")
  // default Actor constructor
  val helloActor = system.actorOf(Props[HelloActor], name = "helloactor")
  helloActor ! "hello"
  helloActor ! "buenos dias"
}

