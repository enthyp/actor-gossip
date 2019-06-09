package chat.server

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props}
import akka.cluster.pubsub.DistributedPubSub
import akka.cluster.pubsub.DistributedPubSubMediator.{Publish, Subscribe}

import scala.collection.mutable


object Server {
  def props(): Props = Props(new Server)

  // server internal messages
  sealed trait ServerMessage
  case object RequestChatRooms extends ServerMessage
  case class ResponseChatRooms(rooms: List[String]) extends ServerMessage

  case class UserLoggedIn(user: String) extends ServerMessage
  case class Logout(user: String) extends ServerMessage
  case object ShutdownSession extends ServerMessage
  case class UserLoggedOut(user: String) extends ServerMessage

  case class RequestJoin(user: String, room: String) extends ServerMessage
  case class ResponseJoined(room: String, roomRef: ActorRef) extends ServerMessage
  case class ResponseNoRoom(room: String) extends ServerMessage

  case class RequestCreateRoom(user: String, room: String) extends ServerMessage
  case class ResponseRoomCreated(room: String) extends ServerMessage
  case class ResponseRoomExists(room: String) extends ServerMessage

  case class RequestDeleteRoom(user: String, room: String) extends ServerMessage
  case class ResponseRoomDeleted(room: String) extends ServerMessage
  case class ResponseNoPerm(room: String) extends ServerMessage
}

class Server extends Actor with ActorLogging {

  // Mediator for server2server communication
  val mediator = DistributedPubSub(context.system).mediator
  val serverTopic = "users" // TODO: names of created rooms should internally be stored with a prefix!
  mediator ! Subscribe(serverTopic, self)

  // Local representation of global network state
  val sessions = new mutable.HashMap[String, ActorRef]()
  val users = new mutable.HashSet[String]()
  val rooms = new mutable.HashMap[String, ActorRef]()
  val admins = new mutable.HashMap[String, String]()

  import Server._

  override def receive: Receive = {

    case chat.RequestLogin(user, ref) =>
      log.info(s"Login request as: $user")

      if (users.contains(user))
        sender() ! chat.ResponseNameTaken(user)
      else {
        val newSession = context.actorOf(UserSession.props(user, ref, self), s"session_$user")
        sessions += (user -> newSession)
        users += user

        sender() ! chat.ResponseLoggedIn(user, newSession)
        mediator ! Publish(serverTopic, UserLoggedIn(user))
      }

    case UserLoggedIn(user) =>
      log.info(s"Login broadcast for: $user")
      users += user

    case chat.Logout(user) =>
      log.info(s"Logout request from: $user")
      val session = sessions remove user
      if (session.isDefined) session.get ! ShutdownSession
      users -= user
      mediator ! Publish(serverTopic, UserLoggedOut(user))

    case UserLoggedOut(user) =>
      log.info(s"Logout broadcast for: $user")
      users -= user

    case RequestChatRooms =>
      log.info("Chat rooms request")
      sender() ! ResponseChatRooms(rooms.keys.toList)

    case RequestJoin(user, room) =>
      log.info(s"Join $room from $user request")

      if (rooms.contains(room)) {
        rooms(room) ! Room.RequestSubscribe(user, sender())
      } else {
        sender() ! ResponseNoRoom(room)
      }

    case RequestCreateRoom(nick, room) =>
      if (!rooms.contains(room)) {
        rooms += (room -> context.actorOf(Room.props(room), s"${room}_room"))
        admins += (room -> nick)
        sender() ! ResponseRoomCreated(room)
      } else
        sender() ! ResponseRoomExists(room)
// TODO: broadcast to others!
    case RequestDeleteRoom(nick, room) =>
      if (rooms.contains(room)) {
        if (admins(room) == nick) {
          val session = rooms(room)
          rooms -= room
          admins -= room
          session ! PoisonPill
          sender() ! ResponseRoomDeleted(room)
        } else
          sender() ! ResponseNoPerm(room)
      } else
        sender() ! ResponseNoRoom(room)

    case other =>
      log.error(s"Unexpected message: $other")
  }
}