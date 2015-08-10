package ru.naumen.indexes

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import ru.naumen.rep._
import scala.Some
import ru.naumen.lucene.GlueLuceneIndex
import scala.reflect.ClassTag
import scala.util.Try


/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 01.12.14
 * Time: 16:33
 * Since: 
 *
 */
trait GlueIndexes {
  val indexes: ArrayBuffer[GlueIndex[_]]

  def clear                         =  indexes.foreach(cache => cache.clear)
  def register(cache: GlueIndex[_]) = {indexes += cache; this }
  def index(item: AnyRef)             {indexes.foreach(_.index(item))}
  def getIndex(cls: Class[_])       =  indexes.find(cache => cls.isAssignableFrom(cache.getClass))

}

/*
trait GlueIndexes  extends GlueIndexes{
  val caches = ArrayBuffer[GlueIndex[_]]()

  def clear                         =  caches.foreach(cache => cache.clear)
  def register(cache: GlueIndex[_]) = {caches += cache; this }
  def index(item: AnyRef)             {caches.foreach(_.index(item))}
  def getIndex(cls: Class[_])       =  caches.find(cache => cls.isAssignableFrom(cache.getClass))

}
trait GlueIndexes {
  val caches = ArrayBuffer[GlueIndex[_]]()

  def clear                         =  caches.foreach(cache => cache.clear)
  def register(cache: GlueIndex[_]) = {caches += cache; this }
  def index(item: AnyRef)             {caches.foreach(_.index(item))}
  def getIndex(cls: Class[_])       =  caches.find(cache => cls.isAssignableFrom(cache.getClass))

}*/

trait GlueIndex[T] {
  def index(item: AnyRef)(implicit c: ClassTag[T]) = {
    Try(index_(item.asInstanceOf[T])).toOption

//    item match {
//    case t: T => index_(t)
//    case _    => None
//  }
  }
  def index_(item: T): Option[T]
  def clear()
}

class GlueFolderIndex extends GlueIndex[GlueFolder] {
  private val map = new mutable.HashMap[String, GlueFolder]
  private var _root: Option[GlueFolder] = None

  override def clear(): Unit = { map.clear(); _root = None }
  override def index_(folder: GlueFolder) = {
    if (_root.isEmpty) _root = Some(folder)
    map.put(folder.fullpath, folder)
  }

  def get(key: GlueKey): Option[GlueFolder] = map.get(key.fullpath)
  def root = _root.orNull
}

class GlueMetaIndex extends GlueIndex[GlueMeta] {
  private val map = new mutable.HashMap[String, GlueUnit]

  override def clear()  { map.clear }

  override def index_(meta: GlueMeta): Option[GlueMeta] = { map.put(meta.id, meta.unit); Some(meta) }

  def get(id: String) = map.get(id)
}