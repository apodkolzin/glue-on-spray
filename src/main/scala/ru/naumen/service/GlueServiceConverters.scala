package ru.naumen.service

import ru.naumen.rep._
import ru.naumen.indexes.GlueHashes
import scala.collection.JavaConversions._

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 16.06.15
 * Time: 14:11
 * Since: 
 *
 */
object GlueServiceConverters
{
  type Item = {/*def id: String; */def name: String}
  type UUID = {def uuid: String}

  case class Element(id: String, name: String, parent: Option[String], children: Array[String], unit: Option[Unit])
  case class Unit(id: String, name: String, folder: String, title: Option[String], groovy: Option[String], birt: Option[String], keywords: Array[String])

  implicit def unit2Unit(unit: GlueUnit): Option[Unit] =  Option(unit).map(u =>
    new Unit(
      u.uuid,
      u.name,
      u.folder.fullpath,
      Option(u.title),
      Option(u.groovy),
      Option(u.birt).map(new String(_)),
      u.meta.map(_.keywords.toList.toArray).getOrElse(Array())))

  implicit def item2uuid(item: Item): UUID = {
    val id: String = item match {
      case folder:  GlueFolder  => folder.fullpath
      case unit:    GlueUnit    => unit.folder.fullpath + unit.name
      case element: GlueElement => element.fullpath
    }
    new { def uuid = GlueHashes.md5str(id) }

  }


  implicit def item2Element(item: Item): Element = {
    val t: TElement = item match {
      case folder:  GlueFolder  => convert(folder)
      case unit:    GlueUnit    => convert(unit)
      case element: GlueElement => convert(element)
    }
    Element(item.uuid, item.name, Option(t.parent).map(uuid), t.children.map(uuid), t.unit)
  }

  implicit def base2Element[T >: GlueBase](base: T): Element = base.asInstanceOf[Item]


  private def convert(element: GlueElement): TElement = new {
    def parent: Item = element.unit
    def children: Array[Item] = Array[Item]()
    def unit: GlueUnit = element.unit
  }

  private def convert(folder: GlueFolder): TElement = new {
    def parent: Item = folder.parent
    def children: Array[Item] = {
      val folders: Array[_ <: Item] = folder.subFolders.sortBy(_.name)
      val units: Array[_ <: Item] = folder.units().sortBy(_.name)
      folders ++ units
    }
    def unit: GlueUnit = null
  }

  private def convert(gunit: GlueUnit): TElement = new {
    def parent: Item = gunit.folder
    def children: Array[Item] = gunit.elements.values.toArray
    def unit: GlueUnit = gunit
  }

  private def uuid(item: Item) = item.uuid

  private type TElement = {
    def parent: Item
    def children: Array[Item]
    def unit: GlueUnit
  }
}
