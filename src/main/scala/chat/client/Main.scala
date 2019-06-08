package chat.client

import akka.actor.{ActorSystem, PoisonPill}
import com.typesafe.config.ConfigFactory


object Main {
  def main(args: Array[String]): Unit = {

    val usage =
      """
        | usage: run [-ch client_host] [-cp client_port] [-sh server_host] [-sp server_port]
        | By default client is run on 127.0.0.1:2553, targeting local server
        | on 127.0.0.1:2551
      """.stripMargin

    var clientHost = "127.0.0.1"
    var clientPort = 2553
    var serverHost = clientHost
    var serverPort = 2551

    if (args.length > 0) {
      val argsList = args.toList

      def parseOptions(args: List[String]): Unit = {
        args match {
          case Nil =>
          case "-ch" :: host :: tail =>
            clientHost = host
            parseOptions(tail)
          case "-cp" :: port :: tail =>
            clientPort = port.toInt
            parseOptions(tail)
          case "-sh" :: host :: tail =>
            serverHost = host
            parseOptions(tail)
          case "-sp" :: port :: tail =>
            serverPort = port.toInt
            parseOptions(tail)
          case opt =>
            println(s"Unknown option: $opt")
            println(usage)
            System.exit(1)
        }
      }

      try {
        parseOptions(argsList)
      } catch {
        case _: NumberFormatException =>
          println("Port numbers must be integers.")
          println(usage)
          System.exit(1)
        case e: Throwable =>
          println(s"Error occurred: $e")
          println(usage)
          System.exit(1)
      }
    }

    val config = ConfigFactory.parseString(
      s"""
         |akka.remote.netty.tcp.hostname="$clientHost"
         |akka.remote.netty.tcp.port=$clientPort
       """.stripMargin)
      .withFallback(ConfigFactory.load("client"))

    val system = ActorSystem("chat-client-system", config)

    val serverActorRef =
      system.actorSelection(s"akka.tcp://chat-server-system@$serverHost:$serverPort/user/server")

    val clientActorRef = system.actorOf(Client.props(serverActorRef), "client")

    var running = true
    while (running) {
      scala.io.StdIn.readLine() match {
        case "\\quit" =>
          clientActorRef ! PoisonPill
          running = false
        case "" =>
        case line =>
          clientActorRef ! Client.InputLineMsg(line)
      }
    }

    system.terminate()
  }
}
