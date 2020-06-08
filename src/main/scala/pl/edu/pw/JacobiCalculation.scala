package pl.edu.pw

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.util.Timeout
import akka.actor.typed.scaladsl.AskPattern._
import pl.edu.pw.JacobiActor.{JacobiOutput, Reply}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object JacobiCalculation {
  def calculate(A: Array[Array[Float]], B: Array[Float], iteration: Int, threads:Int): Array[Float] = {
    val equationSize = A.length
    val N = Array.ofDim[Float](equationSize)

    val start = System.currentTimeMillis()
    for (i <- N.indices) {
      N(i) = 1 / A(i)(i)
    }


    val M = Array.ofDim[Float](equationSize, equationSize)
    for (i <- M.indices) {
      for (j <- M.indices) {
        if (i == j) M(i)(j) = 0
        else M(i)(j) = -(A(i)(j) * N(i))
      }
    }
    println("Matrix calculation time: " + (System.currentTimeMillis() - start))
      val initX = Array.ofDim[Float](equationSize)
      for (i <- initX.indices) {
        initX(i) = 0
      }

    val result: Array[Future[Reply]] = Array.ofDim(equationSize)
    implicit val system: ActorSystem[JacobiActor.JacobiInput] = ActorSystem(JacobiActor(), "xd")
    implicit val ec = system.executionContext


    implicit val timeout: Timeout = Timeout(300, java.util.concurrent.TimeUnit.SECONDS)
    var systems:Array[ActorSystem[JacobiActor.JacobiInput]] = Array.ofDim(threads)
    var actors: Array[ActorRef[JacobiActor.JacobiInput]] = Array.ofDim(threads)
    for(i <- 0 until threads ){
      systems(i) = ActorSystem(JacobiActor(),"actor" + i)
      actors(i) = systems(i)
    }
    for (n <- 0 until iteration) {
      for (i <- 0 until equationSize) {
        result(i) = actors(i % threads).ask(ref => JacobiActor.JacobiInput(M(i), N(i), B(i), initX, ref))
      }
      for (i <- result.indices) {
        result(i).onComplete {
          case Success(JacobiOutput(x)) => {
            initX(i) = x
          }
          case Failure(ex) => println(s"An error occurred ${ex.getMessage}")
        }
        Await.result(result(i),Duration(1000, "millis") )
      }
    }
    for(i <-systems.indices){
      systems(i).terminate()
    }
    system.terminate()
    initX
  }
}
