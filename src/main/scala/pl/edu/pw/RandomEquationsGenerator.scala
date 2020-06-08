package pl.edu.pw
import java.io._

import scala.util.Random

object RandomEquationsGenerator extends App {
  val size = 5000
  val file = new File("src/main/resources/5000equation.txt")
  val bw = new BufferedWriter(new FileWriter(file))
  val random = new Random()
  bw.write(size + "\n")
  for(i <- 0 until size) {
    for(j <- 0 until size){
      bw.write(random.nextInt(100) + " ")
    }
    bw.write(random.nextFloat().toString)
    bw.write("\n")
  }
  bw.close()
}
