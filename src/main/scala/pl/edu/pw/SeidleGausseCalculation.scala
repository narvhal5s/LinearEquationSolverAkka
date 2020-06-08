package pl.edu.pw

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.util.Timeout
import akka.actor.typed.scaladsl.AskPattern._
import pl.edu.pw.SeidleGausseActor.{GaussOutput, Reply}

import scala.concurrent.Future
import scala.util.{Failure, Success}

object SeidleGausseCalculation {
  def calculate(A: Array[Array[Float]], B: Array[Float], iteration: Int, threads: Int): Array[Float] = {
    val equationSize = A.length
    val U = Array.ofDim[Float](equationSize, equationSize)
    val L = Array.ofDim[Float](equationSize, equationSize)
    val D = Array.ofDim[Float](equationSize, equationSize)

    val start = System.currentTimeMillis()
    for (i <- 0 until equationSize) {
      for (j <- 0 until equationSize) {
        if (i < j) {
          U(i)(j) = A(i)(j)
        } else if (i > j) {
          L(i)(j) = A(i)(j)
        } else {
          D(i)(j) = A(i)(j)
        }
      }
    }

    for (i <- 0 until equationSize) {
      D(i)(i) = 1 / D(i)(i)
    }

    for (i <- 0 until equationSize) {
      B(i) = B(i) * D(i)(i)
    }

    for (i <- 0 until equationSize) {
      for (j <- 0 until equationSize) {
        L(i)(j) = L(i)(j) * D(i)(i)
      }
    }

    for (i <- 0 until equationSize) {
      for (j <- 0 until equationSize) {
        U(i)(j) = U(i)(j) * D(i)(i)
      }
    }
    println("Matrix calculation time: " + ( System.currentTimeMillis() - start))

    val result: Array[Future[Reply]] = Array.ofDim(equationSize)
    implicit val system: ActorSystem[SeidleGausseActor.GaussInput] = ActorSystem(SeidleGausseActor(), "xd")
    implicit val ec = system.executionContext

    val initX = Array.ofDim[Float](equationSize)
    for (i <- initX.indices) {
      initX(i) = 0
    }
    implicit val timeout: Timeout = Timeout(300, java.util.concurrent.TimeUnit.SECONDS)
    var completed: Boolean = false;
    var systems:Array[ActorSystem[SeidleGausseActor.GaussInput]] = Array.ofDim(threads)
    var actors: Array[ActorRef[SeidleGausseActor.GaussInput]] = Array.ofDim(threads)
    for(i <- 0 until threads ){
      systems(i) = ActorSystem(SeidleGausseActor(),"actor" + i)
      actors(i) = systems(i)
    }
    for (n <- 0 until iteration) {
      for (i <- 0 until equationSize) {
        result(i) = actors(i % threads).ask(ref => SeidleGausseActor.GaussInput(L(i), U(i), B(i), initX, i,ref))
        completed = false
        while(!completed) {
          if (result(i).isCompleted) {
            completed = true
          }
        }
          result(i).onComplete{
            case Success(GaussOutput(x)) => {
              initX(i) = x
            }
            case Failure(ex) => println(s"An error occurred ${ex.getMessage}")
          }
        }
    }
    for(i <-systems.indices){
      systems(i).terminate()
    }
    system.terminate()

    initX
  }
}
