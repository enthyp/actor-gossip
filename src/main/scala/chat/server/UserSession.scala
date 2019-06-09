package chat.server

import akka.actor.{Actor, ActorLogging, ActorRef, FSM, Props}
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
  case class ConvData(room: String, roomRef: ActorRef) extends Data
}

class UserSession(val nick: String, val client: ActorRef, val server: ActorRef) extends Actor
  with ActorLogging
  with FSM[State, Data] {

  import UserSession._

  startWith(Connected, ConnectionData)

  when(Connected) {
    case Event(Server.ShutdownSession, ConnectionData) =>
      log.info(s"Shutdown ordered.")
      stop()

    case Event(chat.RequestChatRooms, ConnectionData) =>
      log.info(s"Request for chat rooms received.")
      server ! Server.RequestChatRooms
      stay

    case Event(Server.ResponseChatRooms(roomsList), ConnectionData) =>
      log.info(s"Chat rooms response received.")
      client ! chat.ResponseChatRooms(roomsList)
      stay

    case Event(chat.RequestJoin(room), ConnectionData) =>
      log.info(s"Join $room received.")
      server ! Server.RequestJoin(nick, room)
      stay

    case Event(Room.ResponseJoined(room), ConnectionData) =>
      log.info(s"Joined $room received.")
      client ! chat.ResponseJoined(room)
      goto(Chatting) using ConvData(room, sender())

    case Event(Server.ResponseNoRoom(room), ConnectionData) =>
      log.info(s"No room $room received.")
      client ! chat.ResponseNoRoom(room)
      stay

    case Event(chat.RequestCreateRoom(room), ConnectionData) =>
      log.info(s"Request to create room $room received.")
      server ! Server.RequestCreateRoom(nick, room)
      stay

    case Event(chat.RequestDeleteRoom(room), ConnectionData) =>
      log.info(s"Request to delete room $room received.")
      server ! Server.RequestDeleteRoom(nick, room)
      stay
  }

  onTransition {
    case Connected -> Chatting =>
      nextStateData match {
        case ConvData(_, roomRef) =>
          roomRef ! Room.RequestChatHistory
      }
  }

  when(Chatting) {
    case Event(Server.ShutdownSession, ConvData(_, roomRef)) =>
      log.info(s"Shutdown ordered.")
      roomRef ! Room.Unsubscribe(nick)
      stop()

    case Event(chat.RequestLeave, ConvData(room, roomRef)) =>
      log.info(s"Leave room $room received.")
      roomRef ! Room.Unsubscribe(nick)
      client ! chat.ResponseLeft(room)
      goto(Connected) using ConnectionData

    case Event(Room.ResponseChatHistory(history), _) =>
      client ! chat.ResponseChatHistory(history)
      stay

    case Event(msg@chat.ChatMessage(_, _), ConvData(_, roomRef)) =>
      if (sender() == roomRef) {
        client ! msg
      } else if (sender() == client) {
        roomRef ! Room.Publish(msg)
      }

      stay
  }

  whenUnhandled {
    case other =>
      log.error(s"Unexpected message: $other")
      stay
  }

  initialize()
}
