package chat.client

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory
import scalafx.Includes.when
import scalafx.application.{JFXApp, Platform}
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Scene
import scalafx.scene.control._
import scalafx.scene.layout.{BorderPane, ColumnConstraints, GridPane, HBox}
import scalafx.scene.paint.Color._

object UIActor {
  def props: Props = Props(new UIActor)

  sealed trait UIMessage {
    def header: String = ""
    def content: String = ""
  }
  case class Connect(host: String, port: Int, user: String) extends UIMessage
  case class Error(reason: String, text: String) extends UIMessage {
    override def header: String = reason
    override def content: String = text
  }
  case object Connected extends UIMessage
}

class UIActor extends Actor with ActorLogging {

  var clientActor: Option[ActorRef] = None

  import UIActor._

  override def preStart(): Unit = log.info("REST")

  def receive: Receive = {
    case msg =>
      context.become(initial)
      self ! msg
  }

  def initial: Receive = {
    case Connect(host, port, user) =>
      if (clientActor.isEmpty) {
        val serverActorRef =
          context.actorSelection(s"akka.tcp://chat-server-system@$host:$port/user/server")
        clientActor = Some(context.actorOf(Client.props(self, serverActorRef), "client"))
      }

      clientActor.get ! Client.InputLineMsg(user)

    case Connected =>
      context.become(connected)
      Platform.runLater {
        UI.stateMain()
      }

    case msg: Error =>
      Platform.runLater {
        UI.connectionFailure(msg)
      }
  }

  def connected: Receive = {
    case _ =>
      println("blob")
  }
}

object UI extends JFXApp {

  val system = ActorSystem("chat-client-system", ConfigFactory.load("client"))
  val uiActor = system.actorOf(UIActor.props, "UI")

  stage = new JFXApp.PrimaryStage {

    scene = new Scene {
      title.value = "Server choice"
      fill = LightGrey
      root = connectWindow

      onShown = _ => {
        minWidth = 300
        minHeight = 200
      }
    }
  }

  def connectWindow: GridPane = {
    val inputGrid = new GridPane {
      alignment = Pos.TopCenter
      hgap = 10
      vgap = 10
      padding = Insets(10, 10, 10, 10)
    }

    inputGrid.add(new Label("Server IP"), 0, 0)
    val hostField = new TextField { text = "127.0.0.1" }
    inputGrid.add(hostField, 1, 0)

    inputGrid.add(new Label("Server port"), 0, 1)
    val portField = new TextField { text = "2551" }
    inputGrid.add(portField, 1, 1)

    inputGrid.add(new Label("Nickname"), 0, 2)
    val nickField = new TextField
    inputGrid.add(nickField, 1, 2)

    val column1 = new ColumnConstraints()
    column1.setPercentWidth(30)
    val column2 = new ColumnConstraints()
    column2.setPercentWidth(70)
    inputGrid.columnConstraints = List(column1, column2)

    val connectButton = new Button {
      text = "Connect"
      onAction = _ => {
        uiActor ! UIActor.Connect(
          hostField.text.value,
          portField.text.value.toInt,
          nickField.text.value
        )
      }
    }

    val buttonRow = new HBox {
      children = List(connectButton)
      alignment = Pos.BottomRight
    }

    inputGrid.add(buttonRow, 1, 3)
    inputGrid
  }

  def connectionFailure(error: UIActor.Error): Unit = {
    dialog(error).showAndWait()
  }

  def dialog[A <: UIActor.UIMessage](msg: A): Dialog[A] = {
    val dialog = new Dialog[A] {
      title = msg.header
      contentText = msg.content
    }
    dialog.getDialogPane.getButtonTypes.addAll(ButtonType.OK)
    dialog
  }

  def stateMain(): Unit = {
    stage.getScene.setRoot(mainWindow)
  }

  def mainWindow: BorderPane = {
    new BorderPane()
  }

  def chatWindow: BorderPane = {
    new BorderPane()
  }

  override def stopApp(): Unit = {
    system.terminate()
  }
}