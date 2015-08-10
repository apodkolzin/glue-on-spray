package ru.naumen.rep

import java.io.File
import scala.collection.{mutable, immutable}
import scala.reflect.ClassTag
import javax.xml.bind.{Unmarshaller, JAXBContext}
import ru.naumen.git.GlueGitManager
import ru.naumen.indexes._
import ru.naumen.lucene.{GlueLuceneIndex, GlueLuceneSearcher}
import scala.collection.mutable.ArrayBuffer

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 28.11.14
 * Time: 16:44
 * Since: 
 *
 */
trait GlueRep extends GlueInitializer with GlueUpdater with GlueSearcherFactory

trait GlueScannerOps{
  val scanner: GlueScanner
  val indexer: GlueIndexes

  def scan(tuple: (GlueKey, GlueFolder)) = scanner.scan(tuple._1, tuple._2)
}

trait GlueUpdater extends GlueScannerOps{
  val repositoryPath: String
  val updated: GlueUpdatedSearcher

  val git = new GlueGitManager(repositoryPath)
  def update() {updated.find(git.update: _*).map(scan)}

}

trait GlueInitializer extends GlueScannerOps{
  val repositoryPath: String

  var _rootFolder: GlueFolder = null
  def rootFolder: GlueFolder = _rootFolder
  def init() { indexer.clear; _rootFolder = scanner.scan(GlueKey(repositoryPath)) }

}

trait GlueSearcherFactory extends GlueScannerOps {
  def searcher[S <: GlueSearcher](clazz: Class[S]): S = {
    val cons = clazz.getConstructors()(0)
    val vals = cons.getParameters.map(param=>indexer.getIndex(param.getType).orNull)
    cons.newInstance(vals: _*).asInstanceOf[S]
  }
  def search[S <: GlueSearcher: ClassTag]: S = searcher[S](implicitly[ClassTag[S]].runtimeClass.asInstanceOf[Class[S]])

  def find(id: String) = search[GlueUnitSearcher].find(id)
}


case class GlueKey (file: File) {
  def name = { file.getName } // имя файла с расширением
  def fullpath = file.getAbsolutePath.stripSuffix("/")
  def parent = GlueKey(file.getParentFile)
  def exists = Option(file).exists(_.exists)
}

object GlueKey {
  def apply(path: String): GlueKey = new GlueKey(new File(path))
  implicit def keyAsExt(key: GlueKey): GlueKeyExt = new GlueKeyExt(key)
}

class GlueKeyExt(val key: GlueKey){
  private var n: String = ""
  private var e: String = ""

  {
    import GlueElementLoaderFactory._
    n = key.name
    val i = n.lastIndexOf('.')
    if (i >= 0 ) {
      e = loaders
        .keySet
        .find(s => n.endsWith("." + s))
        .getOrElse(n.substring(i + 1))
      if (!e.isEmpty) n = n.substring(0, n.indexOf(e) - 1)
    }
  }

  // имя юнита
  def uname = n
  // расширение файла
  def ext = e
}



