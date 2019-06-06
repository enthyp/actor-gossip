package chat.server

import akka.actor.{Actor, ActorLogging, ActorRef, FSM, Props}
import akka.util.Timeout
import chat._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import akka.pattern.ask
import chat.server.Room.{Publish, Unsubscribe}
import chat.server.Server.{ChatRooms, GetChatRooms, Logout}
import chat.server.UserSession.{Data, State}


object UserSession {
  def props(nick: String, client: ActorRef, server: ActorRef): Props =
    Props(new UserSession(nick, client, server))

  // session states
  sealed trait State
  case object Connected extends State
  case object Chatting extends State

  // session data
  sealed trait Data
  case object ConnectionData extends Data
  case class ConvData(room: ActorRef) extends Data
}

class UserSession(val nick: String, val client: ActorRef, val server: ActorRef) extends Actor
  with ActorLogging
  with FSM[State, Data] {
  import UserSession._

  implicit val timeout = Timeout(1 seconds)
  import scala.concurrent.ExecutionContext.Implicits.global

  startWith(Connected, ConnectionData)

  when(Connected) {
    case Event(RequestChatRooms, ConnectionData) =>
      log.info(s"Request for chat rooms received.")
      val rooms: Future[ChatRooms] = (server ? GetChatRooms).mapTo[ChatRooms]

      rooms.onComplete {
        case Success(msg: ChatRooms) =>
          client ! RespondChatRooms(msg.rooms)
        case Failure(e) =>
          e.printStackTrace()
          client ! RespondChatRooms(List())
      }

      stay

    case Event(chat.Join(user, room), ConnectionData) =>
      log.info(s"Join received.")
      server ! Server.Join(user, room)
      stay

    case Event(Server.Joined(room, roomRef), ConnectionData) =>
      log.info(s"Joined received.")
      client ! chat.Joined(room)
      goto(Chatting) using ConvData(roomRef)

    case Event(RequestLogout, ConnectionData) =>
      log.info(s"Logout received.")
      server ! Logout(nick)
      stop()
  }

  when(Chatting) {
    case Event(Leave, ConvData(room)) =>
      log.info(s"Leave received.")
      room ! Unsubscribe(nick)
      client ! LeftRoom
      goto(Connected) using ConnectionData

    case Event(msg @ ChatMessage(from, _), ConvData(room)) =>
      if (from != nick) {
        client ! msg
      } else {
        room ! Publish(nick, msg)
      }

      stay
  }

  onTransition {
    case Connected -> Chatting =>
      // TODO: get chat log from Room
  }

  whenUnhandled {
    case other =>
      log.error(s"Unexpected message: $other")
      stay
  }

  initialize()
}
