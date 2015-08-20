package ru.naumen

import akka.actor.{Actor, ActorSystem, Props}
import akka.io.IO
import spray.can.Http
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import ru.naumen.service.GlueService
import com.typesafe.config.Config

object Boot extends App {
  implicit val system = ActorSystem("on-spray-can")

  val service = system.actorOf(Props[ServiceActor], "demo-service")
  val config: Config = system.settings.config
  val repositoryPath: String = config.getString("glue.rep-path")

  implicit val timeout = Timeout(15.seconds)
  // start a new HTTP server on port 8080 with our service actor as the handler
  IO(Http) ? Http.Bind(service, interface = "localhost", port = 8080)
}


class ServiceActor extends Actor with MyService with GlueService{

  def actorRefFactory = context

  def receive = runRoute(myRoute ~ glueRoute)
}