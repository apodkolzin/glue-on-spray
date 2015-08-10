package ru.naumen.rep

import ru.naumen.rep.GlueElementLoaderFactory
import ru.naumen.indexes.GlueIndexes

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 10.06.15
 * Time: 17:37
 * Since: 
 *
 */
trait GlueFactory{
  def newFolder(key: GlueKey, parent: GlueFolder): GlueFolder
  def newUnit(key: GlueKey, folder: GlueFolder):   GlueUnit
  def newElement(key: GlueKey, unit: GlueUnit):    GlueElement
}

class GlueFactoryImpl(val glue:GlueIndexes with GlueSearcherFactory) extends GlueFactory {
  import GlueElementLoaderFactory._

  override def newElement(key: GlueKey, unit: GlueUnit): GlueElement = {
    val element: GlueElement = load(key)
    element.unit = unit
    unit.elements.put(element.ext, element)
    glue.index(element)
    element
  }

  override def newUnit(key: GlueKey, folder: GlueFolder) = {
    val ekey = new GlueKeyExt(key)
    val unit: GlueUnit = new GlueUnit( ekey.uname, folder, glue)
    folder.unitMap.put(unit.name, unit)
    glue.index(unit)
    unit
  }

  override def newFolder(key: GlueKey, parent: GlueFolder)= {
    val folder: GlueFolder = new GlueFolder(key.name, parent, key.fullpath)
    Option(parent).foreach(_.folderMap.put(folder.name, folder))
    glue.index(folder)
    folder
  }
}
