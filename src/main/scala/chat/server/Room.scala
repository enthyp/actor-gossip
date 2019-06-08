package chat.server

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.cluster.pubsub.DistributedPubSub
import akka.cluster.pubsub.DistributedPubSubMediator.{Publish, Subscribe}

import scala.collection.mutable


object Room {
  def props(name: String): Props = Props(new Room(name))

  trait PublishableMessage

  sealed trait RoomMessage

  case class RequestSubscribe(user: String, ref: ActorRef) extends RoomMessage
  case class ResponseJoined(room: String) extends RoomMessage

  case class Unsubscribe(user: String) extends RoomMessage
  case class Joined(user: String) extends RoomMessage
  case class Left(user: String) extends RoomMessage
  case class Publish(msg: PublishableMessage) extends RoomMessage

  case object RequestChatHistory extends RoomMessage
  case class ResponseChatHistory(history: List[chat.ChatMessage]) extends RoomMessage
}

class Room(name: String) extends Actor with ActorLogging {
  val mediator = DistributedPubSub(context.system).mediator
  val topic = name
  mediator ! Subscribe(topic, self)

  private val userSessions = new mutable.HashMap[String, ActorRef]()
  private val users = new mutable.HashSet[String]()  // TODO: for \names command
  private val history = new chat.server.RoomHistory(10)

  override def receive: Receive = {
    case Room.RequestSubscribe(user, ref) =>
      userSessions += (user -> ref)
      users += user
      mediator ! Publish(topic, Room.Joined(user))

      val msg = chat.ChatMessage("#SERVER", s"$user joins the room.")
      mediator ! Publish(topic, msg)

      ref ! Room.ResponseJoined(name)

    case Room.Unsubscribe(user) =>
      userSessions -= user
      users -= user
      mediator ! Publish(topic, Room.Left(user))

      val msg = chat.ChatMessage("#SERVER", s"$user leaves the room.")
      mediator ! Publish(topic, msg)

    case Room.Publish(msg) =>
      mediator ! Publish(topic, msg)

    case msg @ chat.ChatMessage(from, _) =>
      if (from != "#SERVER") history.addMsg(msg)
      userSessions.foreach { case (_, ref) => ref ! msg }

    case Room.Joined(user) =>
      users += user

    case Room.Left(user) =>
      users -= user

    case Room.RequestChatHistory =>
      val historyList: List[chat.ChatMessage] = history
      sender() ! Room.ResponseChatHistory(historyList)
  }
}
