import akka.actor.{Actor, ActorSystem, Props}
import akka.io.IO
import spray.routing._
import spray.http._
import MediaTypes._
import spray.can.Http
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

object WebServer { //extends App {
  implicit val system = ActorSystem("on-spray-can")
  val service = system.actorOf(Props[MyServiceActor], "demo-service")

  implicit val timeout = Timeout(5.seconds)
  implicit val executionContext = system.dispatcher

	IO(Http).ask(Http.Bind(service, interface = "localhost", port = 8080)).map {
    case Http.Bound(address) =>
      java.awt.Desktop.getDesktop().browse(java.net.URI.create("http://localhost:8080"))
    case Http.CommandFailed(cmd) =>
      println(s"[ERROR] REST interface could not bind to localhost:8080 ${cmd.failureMessage}")
      system.shutdown()
  }

  sys addShutdownHook {
    println("\nInterrupt caught, shutting down...")
  }
}


class MyServiceActor extends Actor with MyService {
  def actorRefFactory = context
  def receive = runRoute(myRoute)
}

trait MyService extends HttpService {
  val myRoute =
    pathSingleSlash {
      get {
        respondWithMediaType(`text/html`) {
          complete {
            <html>
              <head>
                <link rel="stylesheet" type="text/css" href="css/style.css"></link>
              </head>
              <body>
                <div id="content">
                  <h1>Example Track</h1>
                  <canvas></canvas>
                </div>

                <script src="js/vexflow-min.js"></script>
                <script src="js/test.js"></script>
              </body>
            </html>
          }
        }
      }
    } ~
    pathPrefix("css") {
      get {
        getFromResourceDirectory("css")
      }
    } ~
    pathPrefix("js") {
      get {
        getFromResourceDirectory("js")
      }
    }
}
