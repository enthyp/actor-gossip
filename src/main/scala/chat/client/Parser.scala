package chat.client

object Parser {

  sealed trait Command
  final case class JoinCmd(room: String) extends Command
  final case object GetRoomsCmd extends Command
  final case object LeaveCmd extends Command
  final case object LogoutCmd extends Command
  final case object UnknownCmd extends Command

  final case class ParsingError(private val message: String = "",
                                private val cause: Throwable = None.orNull)
    extends Exception(message, cause)

  def parseLine(line: String): Either[Command, String] = {
    val trimmed = line.trim()

    trimmed.split(" ", 2) match {
      case Array() =>
        Right("")
      case Array(head) =>
        if (head.take(1) == "\\")
          Left(parseCommand(head.drop(1), None))
        else
          Right(head)
      case Array(head, rem) =>
        if (head.take(1) == "\\")
          Left(parseCommand(head.drop(1), Some(rem)))
        else
          Right(trimmed)
    }
  }

  def parseCommand(command: String, rem: Option[String]): Command = {
    command match {
      case "rooms" =>
        if (rem.isDefined)
          throw ParsingError(message = "Command: \\rooms takes no parameters.")
        else
          GetRoomsCmd
      case "join" =>
        if (rem.isEmpty || rem.get.split(" ").length > 1)
          throw ParsingError(message = "Command: \\join takes exactly one parameter.")
        else
          JoinCmd(rem.get)
      case "leave" =>
        if (rem.isDefined)
          throw ParsingError(message = "Command: \\leave takes no parameters.")
        else
          LeaveCmd
      case "logout" =>
        if (rem.isDefined)
          throw ParsingError(message = "Command: \\logout takes no parameters.")
        else
          LogoutCmd
      case _ =>
        UnknownCmd
    }
  }
}