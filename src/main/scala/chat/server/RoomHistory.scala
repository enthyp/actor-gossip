package chat.server

import scala.collection.mutable
import scala.language.implicitConversions

object RoomHistory {
  implicit def history2list(roomHistory: RoomHistory): List[chat.ChatMessage] = {
    if (roomHistory.history.nonEmpty)
      List.tabulate(roomHistory.history.length)(_ => roomHistory.history.dequeue)
    else
      List()
  }
}

class RoomHistory(val maxSize: Int) {

  private val history = mutable.Queue[chat.ChatMessage]()

  def addMsg(elem: chat.ChatMessage): Unit = {
    history.enqueue(elem)
    if (history.length > maxSize) { history.dequeue() }
  }
}