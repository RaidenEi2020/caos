package caos.frontend.widgets

import caos.frontend.Documentation
import caos.frontend.widgets.WidgetInfo.Visualize
import caos.view.{Mermaid, View}
import org.scalajs.dom
import org.scalajs.dom.{MouseEvent, html}

import scala.runtime.Nothing$
import scala.util.{Failure, Success}
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.ExecutionContext.Implicits.global

class ServerWidget(getRequest:()=>String, name:String, errorBox: OutputArea, doc:Documentation)
  extends Widget[Unit](name,doc) {

  private var box:Block = _
  private var txt:Block = _
  protected val divBox = name.replace(' ','_') + "Box"

  override val get: Unit = ()

  override def init(div: Block, visible: Boolean): Unit = {
    val serverDiv = panelBox(div, visible).append("div")
      .attr("id", "serverDiv")

    mkServerInput
    mkServerButton
    mkServerOutput

    //dom.document.getElementById(titleId).firstChild.firstChild.firstChild.asInstanceOf[html.Element]
    //  .onclick = { (e: MouseEvent) => if (!isVisible) server() }

  }
  
  override def update(): Unit = if(isVisible) server()

  def mkServerInput: Unit = {
    val serverInput = dom.document.createElement("input").asInstanceOf[html.Input]
    serverInput.setAttribute("id", "serverInput")
    serverInput.setAttribute("type", "text")
    serverInput.setAttribute("value", "Write your command")

    val serverDiv = dom.document.getElementById("serverDiv").asInstanceOf[html.Div]
    serverDiv.appendChild(serverInput)
  }

  def mkServerButton: Unit = {
    val serverButton = dom.document.createElement("button").asInstanceOf[html.Button]
    serverButton.setAttribute("id", "serverButton")
    serverButton.innerText = "Execute"
    serverButton.onclick = (_:dom.Event) => {
      server()
    }
    val serverDiv = dom.document.getElementById("serverDiv").asInstanceOf[html.Div]
    serverDiv.appendChild(serverButton)
  }

  def mkServerOutput: Unit = {
    val serverOutput = dom.document.createElement("div").asInstanceOf[html.Div]
    serverOutput.setAttribute("id", "serverOutput")
    serverOutput.innerText = "Output:"
    val serverDiv = dom.document.getElementById("serverDiv").asInstanceOf[html.Div]
    serverDiv.appendChild(serverOutput)
  }

  def server(): Unit = {
    val serverInput = dom.document.getElementById("serverInput").asInstanceOf[html.Input]
    val serverOutput = dom.document.getElementById("serverOutput").asInstanceOf[html.Div]
    val command = serverInput.value
    println("got the request: " + getRequest())
    
    fetchFromServer(command).onComplete {
      case Success(responseText) =>
        serverOutput.innerText = s"Output:\n$responseText"
      case Failure(exception) =>
        serverOutput.innerText = s"Output:\n${exception.getMessage}"
    }
  }

  def fetchFromServer(command: String): scala.concurrent.Future[String] = {

    val url = s"http://localhost:8080/start-process?cmd=${java.net.URLEncoder.encode(command, "UTF-8")}"

    val promise = org.scalajs.dom.window.fetch(url).toFuture
    promise.map { response =>
      if (response.ok) {
        response.text().toFuture
      } else {
        throw new Exception("Failed to fetch data from the server")
      }
    }.flatten
  }


  def run(arg: String): String = {
    arg
  }
}
