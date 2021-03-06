package ru.naumen.service

import spray.routing.HttpService
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
import ru.naumen.rep.{GlueUnit, GlueBase}
import org.json4s.Formats
import spray.httpx.Json4sJacksonSupport


/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 16.06.15
 * Time: 16:49
 * Since: 
 *
 */
trait GlueService extends HttpService with GlueServiceOps with Json4sJacksonSupport {
//  implicit val unitMessageFormat = jsonFormat7(Unit)
//  implicit val elementMessageFormat = jsonFormat5(Element)
  implicit val timeout = Timeout(15 seconds)
  implicit override def actorContext = actorRefFactory.dispatcher
  override implicit def json4sJacksonFormats: Formats = org.json4s.DefaultFormats



  val glueActorRef = actorRefFactory.actorOf(Props[GlueServiceActor], "glue-actor")

  override def ?(message: Any): Future[Any] = glueActorRef ? message

  val glueRoute =
    get{
      path("root") {
          complete{ root().map[Element](base2Element) } //todo use implicit
      } ~
      path("element" / Rest){ key =>
            complete{ element(key).map[Element](base2Element) }
      } ~
      path("unit"){
        parameter('id.as[String]){id =>
          complete{ unit(id).map[Unit](unit2Unit) }
        }
      } ~
      path("text"){
        parameter('id.as[String]){ id =>
          parameter('ext.as[String]){ ext =>
              complete{ unit(id).map(_.elements.get(ext).map(_.text)) }
          }
        }
      } ~
      path("bytes"){
        parameter('id.as[String]){ id =>
          parameter('ext.as[String]){ ext =>
            complete{ unit(id).map(_.elements.get(ext).map(_.bytes)) }
          }
        }
      } ~
      path("find") {
        parameter('query.as[String]){q =>
          complete{ find(q).map(_.map(unit2Unit)) }
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
