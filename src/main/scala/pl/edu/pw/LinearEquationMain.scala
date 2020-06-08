package pl.edu.pw


import scala.io.Source
import java.io.File
import java.io.PrintWriter

object LinearEquationMain {

  def main(args: Array[String]): Unit = {
    val usage: String = "input_filename method(J for Jacobi/G for Gauss) iteration_number threads (optional)output_filename"
    val usageExample: String = "Example: java -jar LinearEquationSolver.jar equation.txt G 10 4 result.txt"
    if (args.length < 4) {
      println("Podano za mało argumentów")
      println(usage)
      println(usageExample)
      return
    }

    val filename = args(0)
    val method = args(1)
    val iteration = args(2).toInt
    val threads = args(3).toInt
    val file = Source.fromFile(filename)
    val equationSize: Int = file.getLines.next().toInt
    val A = Array.ofDim[Float](equationSize, equationSize)
    val B: Array[Float] = Array.ofDim[Float](equationSize)
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
    var result:Array[Float] = Array.ofDim(equationSize)
    val start = System.currentTimeMillis()
    if (method == "J") {
      result = JacobiCalculation.calculate(A, B, iteration, threads)
    } else {
     result = SeidleGausseCalculation.calculate(A, B, iteration, threads)
    }
    println("Equation processing time: " + (System.currentTimeMillis() - start))

    if(args.length > 4){
     val writeFilename = args(4)
      val writer = new PrintWriter(new File(writeFilename))
      for (j <- result.indices) {
        writer.write(result(j) + "\n")
      }
      writer.close()
    }
  }
}

