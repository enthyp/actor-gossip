package chat.server

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.cluster.pubsub.DistributedPubSub
import akka.cluster.pubsub.DistributedPubSubMediator.{Publish, Subscribe}
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
  val mediator = DistributedPubSub(context.system).mediator
  val topic = name
  mediator ! Subscribe(topic, self)
  println(s"$name room subscriber up and running!")

  private val users = mutable.HashMap[String, ActorRef]()

  override def receive: Receive = {
    case Room.Subscribe(user, ref) =>
      users += (user -> ref)

    case Room.Unsubscribe(user) =>
      users -= user

    case Room.Publish(user, msg: ChatMessage) =>
      (users - user).foreach { case (_, ref) => ref ! msg }
      mediator ! Publish(topic, msg)

    case msg @ ChatMessage(from, _) =>
      (users - from).foreach { case (_, ref) => ref ! msg }
  }
}
