package chat.server

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

import scala.collection.mutable

object Room {
  def props(name: String): Props = Props(new Room(name))
}

class Room(name: String) extends Actor with ActorLogging {
  private val users = mutable.HashMap[String, UserSession]

  override def receive: Receive = ???
}
