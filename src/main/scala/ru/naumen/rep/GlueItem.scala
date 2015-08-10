package ru.naumen.rep

import scala.collection.mutable
import java.util
import scala.io.Source
import java.io.{CharArrayWriter, BufferedWriter, File}
import ru.naumen.indexes.GlueHashes
import scala.collection.mutable.ArrayBuffer
import javax.xml.bind.annotation.{XmlAccessorType, XmlAccessType, XmlRootElement}

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 01.12.14
 * Time: 16:34
 * Since: 
 *
 */

trait GlueBase {
  def id: String
}

trait GlueElement extends GlueBase{
  var id: String = ""
  var unit: GlueUnit = null
  var ext: String = ""
  var name: String = ""
  var fullpath: String = ""
}

class GlueUnit (val name: String, val folder: GlueFolder, var searher: GlueSearcherFactory) extends GlueBase{
  def id = folder.id + name  // для кеширования не возможно использовать индификатор меты metaId
  val elements = new mutable.HashMap[String, GlueElement]()

  def title(): String = metaElement.map(_.title).orNull
  def get(ext: String): Option[GlueElement] = elements.get(ext)

  def birt: Array[Byte] = birtElement.map(_.content).getOrElse(linkedBirt)
  def groovy: String    = groovyElement.map(_.content).getOrElse(linkedGroovy)
  def meta = metaElement
  def metaId = meta.map(_.id).getOrElse("")

//  @XmlElement def groovyFile = Option(groovy).map(s => new FileObject(name + ".groovy", "", s.getBytes)).orNull
//  @XmlElement def birtFile   = Option(birt).map(a => new FileObject(name + ".rptdesign", "", a)).orNull

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

  def content(): Array[Byte] = {
    val result = new ArrayBuffer[Byte]()
    val source = scala.io.Source.fromFile(fullpath)
    source.foreach(c=> result += c.toByte)
    result.toArray
  }
}

class GlueText extends GlueElement{
  def content() = Source.fromFile(new File(fullpath)).mkString
}

@XmlAccessorType( XmlAccessType.FIELD )
@XmlRootElement(name = "Meta")
class GlueMeta extends GlueElement{
  val uuid = ""
  val title = ""
  val titleEn = ""
  val groovyId = ""
  val birtId = ""
  var keywords = new util.ArrayList[String]()
}

class GlueFolder(val id: String, val name: String, val parent: GlueFolder, val fullpath: String) extends GlueBase{
  val folderMap = new mutable.HashMap[String, GlueFolder]
  val unitMap = new mutable.HashMap[String, GlueUnit]()

  def units(): Array[GlueUnit] = unitMap.values.toArray
  def subFolders(): Array[GlueFolder] = folderMap.values.toArray
  def unit(name: String): Option[GlueUnit] = unitMap.get(name)
  def folder(name: String): Option[GlueFolder] = folderMap.get(name)
}
