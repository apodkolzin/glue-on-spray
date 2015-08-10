package ru.naumen.indexes

import scala.collection.mutable
import java.security.MessageDigest
import java.math.BigInteger
import ru.naumen.rep.{GlueMeta, GlueFile, GlueText, GlueElement}

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 20.01.15
 * Time: 17:29
 * Since: 
 *
 */
class GlueHashes extends GlueIndex[GlueElement]{
  val hashes = new mutable.HashMap[String, GlueElement]
  private val sames = new mutable.HashMap[GlueElement, List[GlueElement]]

  override def clear(): Unit = hashes.clear()
  override def index_(item: GlueElement) = {
    val hash = md5sum(item)
    if (hashes.contains(hash))
      sames.put(hashes(hash), item :: sames.get(item).getOrElse(Nil))
    else
      hashes.put(hash, item)
    hashes.get(hash)
  }

  def md5sum(item: GlueElement) = GlueHashes.md5sum(content(item))

  def content(element: GlueElement): Array[Byte] = {
     element match {
       case text: GlueText => text.content.getBytes
       case file: GlueFile => file.content
       case meta: GlueMeta => meta.id.getBytes
     }
  }

  def same(item: GlueElement) = hashes.get(md5sum(item))
}

object GlueHashes{
  def md5sum(content: Array[Byte]): String = {
    val digest = MessageDigest.getInstance("MD5")
    digest.update(content)
    new BigInteger(1, digest.digest()).toString(16)
  }
  def md5str(s: String) = md5sum(s.getBytes)
}

class GlueSameSearcher(val hashes: GlueHashes) extends GlueSearcher{
  def findSame(item: GlueElement): GlueElement = {
    hashes.same(item).orNull
  }

  def findByContent(content: Array[Byte]): String= {
    val md5sum: String = GlueHashes.md5sum(content)
    hashes.hashes.get(md5sum).map(_.unit.metaId).orNull
  }

}