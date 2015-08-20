package ru.naumen.rep

import scala.collection.mutable
import java.util
import scala.io.Source
import java.io.{CharArrayWriter, BufferedWriter, File}
import ru.naumen.indexes.GlueHashes
import scala.collection.mutable.ArrayBuffer
import javax.xml.bind.annotation.{XmlAccessorType, XmlAccessType, XmlRootElement}
import java.nio.file.{Files, Paths}

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 01.12.14
 * Time: 16:34
 * Since: 
 *
 */

trait GlueBase

trait GlueElement extends GlueBase{
  var unit: GlueUnit = null
  var ext: String = ""
  var name: String = ""
  var fullpath: String = ""

  def bytes: Array[Byte] = Files.readAllBytes(Paths.get(fullpath))

  def text: String = Source.fromFile(new File(fullpath)).mkString
}

class GlueUnit (val name: String, val folder: GlueFolder, var searher: GlueSearcherFactory) extends GlueBase{
  val elements = new mutable.HashMap[String, GlueElement]()

  def title(): String = metaElement.map(_.title).orNull
  def get(ext: String): Option[GlueElement] = elements.get(ext)

  def birt: Array[Byte] = birtElement.map(_.content).getOrElse(linkedBirt)
  def groovy: String    = groovyElement.map(_.content).getOrElse(linkedGroovy)
  def meta = metaElement
  def metaId = meta.map(_.id).getOrElse("")

  private def metaElement: Option[GlueMeta]   = get(GlueUnit.meta).asInstanceOf[Option[GlueMeta]]
  private def groovyElement: Option[GlueText] = get(GlueUnit.groovy).asInstanceOf[Option[GlueText]]
  private def birtElement: Option[GlueFile]   = get(GlueUnit.birt).asInstanceOf[Option[GlueFile]]

  private def linkedGroovy: String    = linked(_.groovyId)(_.groovy)(null)
  private def linkedBirt: Array[Byte]  = linked(_.birtId)(_.birt)(null)

  private def linked[T](linkedId: GlueMeta => String)(f: GlueUnit => T)(default: => T): T = {
    metaElement.map{meta =>
      val unit = searher.find(linkedId(meta))
      Option(unit).map(f).getOrElse(default)
    }.getOrElse(default)
  }
}

object GlueUnit{
  val groovy = "groovy"
  val birt   = "rptdesign"
  val meta   = "meta.xml"
}

class GlueFile extends GlueElement{

  def content(): Array[Byte] = bytes
}

class GlueText extends GlueElement{
  def content() = text
}

@XmlAccessorType( XmlAccessType.FIELD )
@XmlRootElement(name = "Meta")
class GlueMeta extends GlueElement{
  val id = ""
  val uuid = ""
  val title = ""
  val titleEn = ""
  val groovyId = ""
  val birtId = ""
  var keywords = new util.ArrayList[String]()
}

class GlueFolder(val name: String, val parent: GlueFolder, val fullpath: String) extends GlueBase{
  val folderMap = new mutable.HashMap[String, GlueFolder]
  val unitMap = new mutable.HashMap[String, GlueUnit]()

  def units(): Array[GlueUnit] = unitMap.values.toArray
  def subFolders(): Array[GlueFolder] = folderMap.values.toArray
  def unit(name: String): Option[GlueUnit] = unitMap.get(name)
  def folder(name: String): Option[GlueFolder] = folderMap.get(name)
}
