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
        val response = (serverActorRef ? chat.RequestLogin(line)).mapTo[chat.ResponseLoggedIn]

        var nextState: Client.State = Connecting
        var nextData: Client.Data = Uninitialized
        response.onComplete {
          case Success(chat.ResponseLoggedIn(nick, remoteActor)) =>
            uiActorRef ! UIActor.Connected
            nextState = Connected
            nextData = SessionData(nick, remoteActor)
          case Failure(_) =>
            uiActorRef !UIActor.Error("Connection timeout.", "Failed to connect to server.")
        }

        goto(nextState) using nextData
      }
      else {
        uiActorRef ! UIActor.Error("Erroneous nickname", "Nickname can contain letters only!")
        stay using Uninitialized
      }

    case Event(chat.ResponseNameTaken(nick: String), Uninitialized) =>
      uiActorRef ! UIActor.Error("Nickname taken", s"$nick is taken, choose another one!")
      stay using Uninitialized
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
                goto(Connecting) using Uninitialized
              case UnknownCmd(command) =>
                println(s"Unknown command: $command")
                stay
              case _ =>
                println(s"Command $cmd is not supported in this state.")
                stay
            }
          case Right(_) =>
            println("Only commands are supported in this state.")
            stay
        }
      } catch {
        case ParsingError(message, _) =>
          println(s"Parsing error occurred: $message")
          stay
      }

    case Event(chat.ResponseJoined(room), SessionData(nick, sessionRef)) =>
      println(s"Joined $room!")
      goto(Chatting) using ConvData(nick, room, sessionRef)

    case Event(chat.ResponseNoRoom(room), _: SessionData) =>
      println(s"No such room: $room!")
      stay

    case Event(chat.ResponseChatRooms(rooms: List[String]), _: SessionData) =>
      println("CHAT ROOMS:")
      rooms.foreach(r => println(s"-> $r"))
      stay

    case Event(chat.ResponseRoomCreated(room: String), _: SessionData) =>
      println(s"Chat room $room created!")
      stay

    case Event(chat.ResponseRoomDeleted(room: String), _: SessionData) =>
      println(s"Chat room $room deleted!")
      stay
  }

  when(Chatting) {
    case Event(chat.ChatMessage(from, msg), _: ConvData) =>
      println(s">>> $from: $msg")
      stay

    case Event(chat.ResponseChatHistory(history), _: ConvData) =>
      history.map({case chat.ChatMessage(from, msg) => s">>> $from: $msg"}).foreach(println)
      stay

    case Event(chat.ResponseLeft(roomVal), ConvData(nick, room, remoteRef)) =>
      if (roomVal == room) {
        println(s"Left $room.")
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
              case UnknownCmd(command) => println(s"Unknown command: $command")
              case _ => println(s"Command $cmd is not supported in this state.")
            }
          case Right(msg) => remoteRef ! chat.ChatMessage(nick, msg)
        }
      } catch {
        case ParsingError(message, _) => println(s"Parsing error occurred: $message")
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