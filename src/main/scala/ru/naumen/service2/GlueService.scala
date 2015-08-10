package ru.naumen.service2

import spray.routing.HttpService
import spray.json.DefaultJsonProtocol
import spray.httpx.SprayJsonSupport._
import GlueServiceConverters._
import spray.http.MediaTypes
import akka.util.Timeout
import scala.concurrent.duration._
import akka.actor.Props
import GlueServiceActor._
import ru.naumen.indexes.GlueSearcher
import scala.reflect.ClassTag
import scala.concurrent.Future
import akka.pattern.ask
import ru.naumen.rep.GlueBase


/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 16.06.15
 * Time: 16:49
 * Since: 
 *
 */
trait GlueService extends HttpService with DefaultJsonProtocol with GlueServiceOps {
  implicit val unitMessageFormat = jsonFormat6(Unit)
  implicit val elementMessageFormat = jsonFormat5(Element)
  implicit val timeout = Timeout(15 seconds)
  implicit override def actorContext = actorRefFactory.dispatcher



  val glueActorRef = actorRefFactory.actorOf(Props[GlueServiceActor], "glue-actor")

//  override def searcher[S <: GlueSearcher : ClassTag]: Future[S] = (glueActorRef ? Searcher(of[S])).mapTo[S]

  override def ?(message: Any): Future[Any] = glueActorRef ? message

  val glueRoute =
    get{
      path("root") {
          complete{ root() }
      } ~
      path("element" / Rest){ key =>
            complete{getById(key)}
      } ~
      path("find") {
        parameter('query.as[String]){q =>
          complete{find(q)}
        }

      }
    } ~
    path("init"){
      (get | put){
        complete{(glueActorRef ? Init).mapTo[GlueBase].map[Element](e=>e)}
      } ~
      post{
        complete{(glueActorRef ? Update).mapTo[String]}
      }
    }
}