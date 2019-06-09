package chat.client

import akka.pattern.ask
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSelection, FSM, PoisonPill, Props}
import akka.util.Timeout
import chat.RequestCreateRoom
import chat.client.Client.{Data, State}

import scala.util.{Failure, Success}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object Client {
  def props(uiActorRef: ActorRef, serverActorRef: ActorSelection): Props =
    Props(new Client(uiActorRef, serverActorRef))

  // client side messages
  sealed trait ClientMessage
  case class InputLineMsg(line: String) extends ClientMessage

  case class ChatRoomsMsg(rooms: List[String]) extends ClientMessage
  case class JoinedMsg(room: String) extends ClientMessage
  case class ChatMsg(line: String) extends ClientMessage
  case object LeftMsg extends ClientMessage
  case object LoggedOutMsg extends ClientMessage

  // client states
  sealed trait State
  case object Connecting extends State
  case object Connected extends State
  case object Chatting extends State

  // client data
  sealed trait Data
  case object Uninitialized extends Data
  case class SessionData(nick: String, sessionRef: ActorRef) extends Data
  case class ConvData(nick: String, room: String, roomRef: ActorRef) extends Data
}

class Client(uiActorRef: ActorRef, serverActorRef: ActorSelection) extends Actor
  with ActorLogging
  with FSM[State, Data] {
  import Client._

  override def postStop(): Unit = {
    stateData match {
      case SessionData(nick, _) => serverActorRef ! chat.Logout(nick)
      case ConvData(nick, _, _) => serverActorRef ! chat.Logout(nick)
      case Uninitialized =>
    }
  }

  startWith(Connecting, Uninitialized)

  when (Connecting) {
    case Event(InputLineMsg(line), Uninitialized) =>
      if (line.forall(_.isLetter)) {
        implicit val timeout = Timeout(3 seconds)
        val response = serverActorRef ? chat.RequestLogin(line, self)

        response.onComplete {
          case Success(msg: chat.ResponseLoggedIn) =>
            self ! msg
          case Success(chat.ResponseNameTaken(nick)) =>
            uiActorRef ! UIActor.ResponseError("Nickname taken", s"$nick is taken, choose another one!")
          case Failure(_) =>
            uiActorRef !UIActor.ResponseError("Connection timeout.", "Failed to connect to server.")
        }

        stay
      }
      else {
        uiActorRef ! UIActor.ResponseError("Erroneous nickname", "Nickname can contain letters only!")
        stay using Uninitialized
      }

    case Event(chat.ResponseLoggedIn(nick, remoteActor), Uninitialized) =>
      uiActorRef ! UIActor.ResponseConnected
      goto(Connected) using SessionData(nick, remoteActor)

  }

  import Parser._

  when(Connected) {
    case Event(InputLineMsg(line), SessionData(nick, remoteRef)) =>
      try {
        parseLine(line) match {
          case Left(cmd) =>
            cmd match {
              case CreateRoomCmd(room) =>
                remoteRef ! chat.RequestCreateRoom(room)
                stay
              case DeleteRoomCmd(room) =>
                remoteRef ! chat.RequestDeleteRoom(room)
                stay
              case JoinCmd(room) =>
                remoteRef ! chat.RequestJoin(room)
                stay
              case GetRoomsCmd =>
                remoteRef ! chat.RequestChatRooms
                stay
              case LogoutCmd =>
                serverActorRef ! chat.Logout(nick)
                sender() ! LoggedOutMsg
                goto(Connecting) using Uninitialized
              case UnknownCmd(command) =>
                uiActorRef ! UIActor.ResponseError("Error", s"Unknown command: $command")
                println()
                stay
              case _ =>
                uiActorRef ! UIActor.ResponseError("Error", s"Command $cmd is not supported in this state.")
                stay
            }
          case Right(_) =>
            uiActorRef ! UIActor.ResponseError("Error", "Only commands are supported in this state.")
            stay
        }
      } catch {
        case ParsingError(message, _) =>
          uiActorRef ! UIActor.ResponseError("Error", s"Parsing error occurred: $message")
          stay
      }

    case Event(UIActor.Disconnect, SessionData(nick, _)) =>
      serverActorRef ! chat.Logout(nick)
      stay

    case Event(chat.ResponseJoined(room), SessionData(nick, sessionRef)) =>
      uiActorRef ! Client.JoinedMsg(room)
      goto(Chatting) using ConvData(nick, room, sessionRef)

    case Event(chat.ResponseNoRoom(room), _: SessionData) =>
      uiActorRef ! UIActor.ResponseError("Info", s"No such room: $room!")
      stay

    case Event(chat.ResponseChatRooms(rooms), _: SessionData) =>
      uiActorRef ! Client.ChatRoomsMsg(rooms)
      stay

    case Event(chat.ResponseRoomCreated(room: String), _: SessionData) =>
      uiActorRef ! UIActor.ResponseError("Info", s"Chat room $room created!")
      stay

    case Event(chat.ResponseRoomExists(room), _: SessionData) =>
      uiActorRef ! UIActor.ResponseError("Info", s"Room: $room exists already!")
      stay

    case Event(chat.ResponseRoomDeleted(room: String), _: SessionData) =>
      uiActorRef ! UIActor.ResponseError("Info", s"Chat room $room deleted!")
      stay

    case Event(chat.ResponseNoPerm(room: String), _: SessionData) =>
      uiActorRef ! UIActor.ResponseError("Error", s"You have no permission to delete $room!")
      stay
  }

  when(Chatting) {
    case Event(chat.ChatMessage(from, msg), _: ConvData) =>
      uiActorRef ! Client.ChatMsg(s">>> $from: $msg")
      stay

    case Event(chat.ResponseChatHistory(history), _: ConvData) =>
      history.map({case chat.ChatMessage(from, msg) => s">>> $from: $msg"})
        .foreach(msg => uiActorRef ! Client.ChatMsg(msg))
      stay

    case Event(chat.ResponseLeft(roomVal), ConvData(nick, room, remoteRef)) =>
      if (roomVal == room) {
        uiActorRef ! Client.LeftMsg
        goto(Connected) using SessionData(nick, remoteRef)
      } else {
        stay
      }

    case Event(InputLineMsg(line), ConvData(nick, _, remoteRef)) =>
      try {
        parseLine(line) match {
          case Left(cmd) =>
            cmd match {
              case LeaveCmd => remoteRef ! chat.RequestLeave
              case UnknownCmd(command) =>
                uiActorRef ! UIActor.ResponseError("Error", s"Unknown command: $command")
              case _ =>
                uiActorRef ! UIActor.ResponseError("Error", s"Command $cmd is not supported in this state.")
            }
          case Right(msg) => remoteRef ! chat.ChatMessage(nick, msg)
        }
      } catch {
        case ParsingError(message, _) =>
          uiActorRef ! UIActor.ResponseError("Error", s"Parsing error occurred: $message")
      }

      stay

    case Event(PoisonPill, ConvData(nick, _, _)) =>
      serverActorRef ! chat.Logout(nick)
      stop()
  }

  whenUnhandled {
    case Event(message, _) =>
      log.error(s"Unexpected message: $message")
      stay
  }

  initialize()
}