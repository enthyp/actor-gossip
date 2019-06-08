package chat.client

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSelection, FSM, PoisonPill, Props}
import chat.client.Client.{Data, State}


object Client {
  def props(serverActorRef: ActorSelection): Props =
    Props(new Client(serverActorRef))

  // client internal messages
  sealed trait ClientMessage
  final case class InputLineMsg(line: String) extends ClientMessage

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

class Client(serverActorRef: ActorSelection) extends Actor
  with ActorLogging
  with FSM[State, Data] {
  import Client._

  override def postStop(): Unit = {
    stateData match {
      case SessionData(nick, _) => serverActorRef ! chat.Logout(nick)
      case ConvData(nick, _, _) => serverActorRef ! chat.Logout(nick)
    }
  }

  startWith(Connecting, Uninitialized)

  onTransition {
    case Connected -> Connecting =>
      println("Logged out.")
    case _ -> Connecting =>
      println("Choose login: ")
  }

  when (Connecting) {
    case Event(InputLineMsg(line), Uninitialized) =>
      if (line.forall(_.isLetter))
        serverActorRef ! chat.RequestLogin(line)
      else
        println("Nickname can contain letters only!")
      stay using Uninitialized

    case Event(chat.ResponseLoggedIn(nick, remoteActor), Uninitialized) =>
      println("Logged in.")
      goto(Connected) using SessionData(nick, remoteActor)

    case Event(chat.ResponseNameTaken(nick: String), Uninitialized) =>
      println(s"$nick is taken, choose another one!")
      stay using Uninitialized
  }

  import Parser._

  when(Connected) {
    case Event(InputLineMsg(line), SessionData(nick, remoteRef)) =>
      try {
        parseLine(line) match {
          case Left(cmd) =>
            cmd match {
              case JoinCmd(room) =>
                remoteRef ! chat.RequestJoin(room)
                stay
              case GetRoomsCmd =>
                remoteRef ! chat.RequestChatRooms
                stay
              case LogoutCmd =>
                serverActorRef ! chat.Logout(nick)
                goto(Connecting) using Uninitialized
              case UnknownCmd =>
                println(s"Unknown command: $cmd")
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
              case UnknownCmd => println(s"Unknown command: $cmd")
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