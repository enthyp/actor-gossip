package chat.server

import akka.actor.{Actor, ActorRef}
import akka.util.Timeout
import chat.{ChatRooms, GetChatRooms, Join}

import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

import scala.concurrent.Future
import scala.util.{Failure, Success}

trait SessionManager { this: Actor =>

  val server: ActorRef

  protected def manageSession: Receive = {
    case GetChatRooms =>
      implicit val timeout: Timeout = Timeout(5 seconds)
      val rooms: Future[ChatRooms] = (server ? GetChatRooms).mapTo[ChatRooms]

      rooms.onComplete {
        case Success(msg) =>
          sender() ! msg
        case Failure(e) =>
          e.printStackTrace()
          sender() ! ChatRooms(List())
      }

    case Join(user, room) =>
      println(s"join $room by $user")
  }
}
