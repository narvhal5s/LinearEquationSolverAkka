package pl.edu.pw

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.LoggerOps
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

object SeidleGausseActor {

  sealed trait Command {}
  case class GaussInput(
                          L: Array[Float],
                          U: Array[Float],
                          B: Float,
                          initValues: Array[Float],
                          i:Int,
                          replyTo: ActorRef[Reply]
                        ) extends Command

  sealed trait Reply
  case class GaussOutput(
                           x: Float
                         ) extends Reply

  def apply(): Behavior[GaussInput] = Behaviors.receive { (context, message) =>
    var result = message.B
    for(j <- 0 until message.i){
      result = result - message.L(j) * message.initValues(j)
    }
    for( j <- message.i + 1 until message.initValues.length){
      result = result - message.U(j) * message.initValues(j)
    }
    message.replyTo ! GaussOutput(result)
    Behaviors.same
  }
}
