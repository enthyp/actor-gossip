package chat.server

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}

import scala.io.StdIn

/*
* Top level actor.
* */

object Supervisor {
  def props: Props =
    Props(new Supervisor)
}

class Supervisor extends Actor with ActorLogging {
  override def preStart(): Unit =
    log.info("Chat server started")

  override def postStop(): Unit =
    log.info("Chat server stopped")

  override def receive: Actor.emptyBehavior.type =
    Actor.emptyBehavior
}
