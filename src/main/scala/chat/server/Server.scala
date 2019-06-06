package chat.server

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

import scala.collection.mutable

object Server {
  def props(): Props = Props(new Server)

  // server internals
  sealed trait ServerMessage
  case object GetChatRooms extends ServerMessage
  case class ChatRooms(rooms: List[String]) extends ServerMessage

  case class Join(user: String, room: String) extends ServerMessage
  case class Joined(room: String, roomRef: ActorRef) extends ServerMessage

  case class Logout(user: String) extends ServerMessage
}

class Server extends Actor with ActorLogging {
  override def preStart(): Unit = log.info("Starting server.")
  override def postStop(): Unit = log.info("Server stopped.")

  val sessions = new mutable.HashMap[String, ActorRef]
  val rooms = new mutable.HashMap[String, ActorRef]
  rooms += ("bob" -> context.actorOf(Room.props("bob"), "bob_room"))
  rooms += ("pyp" -> context.actorOf(Room.props("pyp"), "pyp_room"))

  import Server._
  import chat.{Login, LoggedIn, NameTaken}
  import Room.Subscribe

  override def receive: Receive = {
    case Login(user) =>
      log.info(s"Login: $user")

      if (sessions.contains(user))
        sender() ! NameTaken(user)
      else {
        val newSession = context.actorOf(UserSession.props(user, sender(), self), s"session_$user")
        sessions += (user -> newSession)
        sender() ! LoggedIn(user, newSession)
      }

    case GetChatRooms =>
      log.info("GetChatRooms: ")
      sender() ! ChatRooms(rooms.keys.toList)

    case Join(user, room) =>
      log.info(s"Join $room from $user")
      if (rooms.contains(room)) {
        rooms(room) ! Subscribe(user, sender())
        sender() ! Joined(room, rooms(room))
      } else {
        // TODO: some response
      }

    case Logout(user) =>
      log.info(s"Logout: $user")
      sessions -= user

    case other =>
      log.error(s"Unexpected message: $other")
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val portConfig = ConfigFactory.load("server")
    val system = ActorSystem("chat-server-system", portConfig)

    val serverActorRef = system.actorOf(Server.props(), "server")
  }
}