package pl.edu.pw

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

object JacobiActor {

  sealed trait Command {}
  case class JacobiInput(
                                M: Array[Float],
                                N: Float,
                                B: Float,
                                initValues: Array[Float],
                                replyTo: ActorRef[Reply]
                              ) extends Command

  sealed trait Reply
  case class JacobiOutput(
                                 x: Float
                               ) extends Reply

  def apply(): Behavior[JacobiInput] = Behaviors.receive { (context, message) =>
    var result: Float = 0
    var previousResult: Float = 0
      result = message.N * message.B
      for (j <- 0 until message.M.length) {
        result = result + message.M(j) * message.initValues(j)
      }
      previousResult = result
    message.replyTo ! JacobiOutput(previousResult)
    Behaviors.same
  }
}
