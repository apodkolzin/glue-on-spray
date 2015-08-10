package ru.naumen.service2

import GlueServiceConverters._
import ru.naumen.indexes._
import ru.naumen.lucene.{GlueLuceneSearcher, GlueLuceneIndex}
import scala.collection.mutable.ArrayBuffer
import akka.actor.Actor
import ru.naumen.rep.{GlueScanner, GlueRep}
import scala.concurrent.{ExecutionContextExecutor, Future}
import ru.naumen.Boot
import scala.reflect.ClassTag
import ru.naumen.service2.GlueServiceConverters.Element
import ru.naumen.service2.GlueServiceConverters.Unit
import ru.naumen.service2.GlueServiceActor.Searcher


/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 16.06.15
 * Time: 14:10
 * Since: 
 *
 */
class GlueServiceActor extends Actor with GlueRep with GlueIndexes{
  override val indexes = GlueServiceActor.indexes
  override val repositoryPath = Boot.repositoryPath
  override val indexer: GlueIndexes = this
  override val scanner: GlueScanner = new GlueScanner(this)
  override val updated = search[GlueUpdatedSearcher]

  import GlueServiceActor._
  override def receive = {
    case Init   => init(); sender ! rootFolder
    case Update => update(); sender ! "Ok"
    case Searcher(c) => sender ! searcher(c)
  }
}

object GlueServiceActor{
  case class Searcher(c: Class[_ <: GlueSearcher])
  case object Init
  case object Update

  val indexes = ArrayBuffer[GlueIndex[_]](
    new GlueLuceneIndex,
    new GlueFolderIndex,
    new GlueServiceIndex,
    new GlueMetaIndex,
    new GlueHashes)
}

trait GlueServiceOps{

  def getById(id: String): Future[Element] = searcher[GlueServiceSercher].map[Element](_.find(id))
  def root(): Future[Element] = searcher[RootSearcher].map[Element](_.root)
  def find(query: String): Future[Array[Unit]] = searcher[GlueLuceneSearcher].map(_.find(query).map(unit2Unit).map(_.get))


//  def searcher[S <: GlueSearcher: ClassTag]: Future[S]// = (glueActorRef ? Searcher(of[S])).mapTo[S]
  def searcher[S <: GlueSearcher: ClassTag]: Future[S] = (this ? Searcher(of[S])).mapTo[S]
  def ?(message: Any): Future[Any]


  implicit def actorContext: ExecutionContextExecutor
  private def of[S <: GlueSearcher: ClassTag]: Class[S] = implicitly[ClassTag[S]].runtimeClass.asInstanceOf[Class[S]]
}

class RootSearcher(folders: GlueFolderIndex) extends GlueSearcher{
  def root = folders.root
}

