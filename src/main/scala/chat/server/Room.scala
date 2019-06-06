package chat.server

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import chat.ChatMessage

import scala.collection.mutable

object Room {
  def props(name: String): Props = Props(new Room(name))

  sealed trait RoomMessage
  case class Subscribe(user: String, ref: ActorRef) extends RoomMessage
  case class Unsubscribe(user: String) extends RoomMessage
  case class Publish(user: String, msg: ChatMessage) extends RoomMessage
}

class Room(name: String) extends Actor with ActorLogging {
  private val users = mutable.HashMap[String, ActorRef]()

  import Room._

  override def receive: Receive = {
    case Subscribe(user, ref) =>
      users += (user -> ref)
    case Unsubscribe(user) =>
      users -= user
    case Publish(user, msg) =>
      (users - user).foreach { case (u, ref) => ref ! msg }
  }
}
