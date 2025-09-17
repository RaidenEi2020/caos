package caos.frontend
package widgets

import caos.view.View
import org.scalajs.dom.{document, html}
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


class RemoteVisualizeText[Stx](cmds:()=>List[(String,String)], generateHtml: String => String, remember: Boolean,
                               name:String, errorBox: OutputArea, doc:Documentation)
  extends Widget[Unit](name,doc):

  private var box:Block = _
  private var txt:Block = _
  protected val divBox = name.replace(' ','_') + "Box"
  private val token = generateToken(32)
  var firstTime = true

  override val get: Unit = {}

  /**
   * Executed once at creation time, to append the content to the inside of this box
   *
   * @param div     Placeholder that will receive the "append" with the content of the box
   * @param visible is true when this box is initially visible (i.e., expanded).
   */
  override def init(div: Block, visible: Boolean): Unit = {
    val down: (Either[String, String], (() => Unit, String)) =
      Left("clear") -> (
        () => { clear },
        "Clear")

    box = panelBox(div, visible,buttons=down::Nil).append("div")
      .attr("id", divBox)
  }

  /**
   * Block of code that should read the dependencies and:
   *  - update its output value, and
   *  - produce side-effects (e.g., redraw a diagram)
   */
  override def update(): Unit =
    if (isVisible || !firstTime) then
      try {
        firstTime=false
        if (isVisible){
          if (!remember) {
            clear
          }
          for (service, cmd) <- cmds() do {
            remoteCall("run-process", cmd, createAndAppendDiv, service, x => errorBox.error(x))
          }
        }
      }
      catch Widget.checkExceptions(errorBox,name)

  def generateToken(length: Int): String = {
    val chars = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
    (1 to length).map { _ =>
      chars((Math.random() * chars.length).toInt)
    }.mkString
  }

  def clear: Unit = {
    val widgetDiv = document.getElementById(divBox).asInstanceOf[html.Div]
    val children = widgetDiv.getElementsByTagName("div")
    while (children.length > 0) {
      widgetDiv.removeChild(children(0))
    }
  }

  def buildUrl(cmd: String, ip: String, service: String): String = {
    s"http://$ip/$service?cmd=${java.net.URLEncoder.encode(cmd, "UTF-8")}&token=$token"
  }

  def fetchFromServer(url: String): Future[String] = {
    org.scalajs.dom.window.fetch(url)
      .toFuture
      .map { response =>
        if (response.ok)
          response.text().toFuture
        else
          sys.error("Failed to fetch data from the server")
      }
      .flatten
  }

  def createAndAppendDiv(htmlDiv: String): Unit = {
    val replyDiv = document.createElement("div").asInstanceOf[html.Div]
    replyDiv.innerHTML = htmlDiv
    replyDiv.setAttribute("style", "border: 3px solid #ccc; padding: 0.5em; margin-bottom: 0.5em; border-radius: 6px;")
    val widgetDiv = document.getElementById(divBox).asInstanceOf[html.Div]
    widgetDiv.appendChild(replyDiv)
  }

  def remoteCall(service: String, cmd: String, callback: String => Unit, ip: String, onError: String => Unit): Unit = {
    fetchFromServer(buildUrl(cmd, ip, service)).onComplete {
      case Success(x) => callback(generateHtml(x))
      case Failure(_) => onError(s"Failed to start '$cmd' at $ip/$service.")
    }
  }

