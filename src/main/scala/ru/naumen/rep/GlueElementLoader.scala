package ru.naumen.rep

import javax.xml.bind.{Unmarshaller, JAXBContext}
import scala.collection.mutable

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 10.06.15
 * Time: 17:36
 * Since: 
 *
 */
trait GlueElementLoader{
  def load(key: GlueKey)(implicit extend: GlueKey => GlueKeyExt): GlueElement

}

class GlueFileLoader extends GlueElementLoader{
  override def load(key: GlueKey)(implicit extend: GlueKey => GlueKeyExt): GlueElement = {
    val el = new GlueFile()
    el.ext = key.ext
    el.name = key.name
    el.fullpath = key.fullpath
    el
  }
}

class GlueTextLoader extends GlueElementLoader{
  override def load(key: GlueKey)(implicit extend: GlueKey => GlueKeyExt): GlueElement = {
    val el = new GlueText()
    el.ext = key.ext
    el.name = key.name
    el.fullpath = key.fullpath
    el
  }
}

class GlueMetaLoader extends GlueElementLoader  {
  override def load(key: GlueKey)(implicit extend: GlueKey => GlueKeyExt): GlueElement = {
    val el = loadFromFile(key)
    el.ext = key.ext
    el.name = key.name
    el.fullpath = key.fullpath
    el
  }

  def loadFromFile(key: GlueKey): GlueMeta = {
    val context = JAXBContext.newInstance(classOf[GlueMeta])
    val unmarshaller: Unmarshaller = context.createUnmarshaller
    unmarshaller.unmarshal(key.file).asInstanceOf[GlueMeta]
  }
}

object GlueElementLoaderFactory {
  import GlueUnit._

  val loaders = mutable.LinkedHashMap[String, GlueElementLoader](
    groovy -> new GlueTextLoader,
    birt   -> new GlueFileLoader,
    meta   -> new GlueMetaLoader)

  private def loader(ext: String): GlueElementLoader = {
    loaders.get(ext).getOrElse(new GlueFileLoader)
  }

  def load(key: GlueKey)(implicit extend: GlueKey => GlueKeyExt) = loader(key.ext).load(key)

}
