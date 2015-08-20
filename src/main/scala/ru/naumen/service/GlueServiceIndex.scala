package ru.naumen.service

import ru.naumen.indexes.{GlueHashes, GlueSearcher, GlueIndex}
import ru.naumen.rep.{GlueMeta, GlueFolder, GlueBase}
import scala.collection.mutable
import GlueServiceConverters._

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 16.06.15
 * Time: 14:05
 * Since: 
 *
 */
class GlueServiceIndex extends GlueIndex[GlueBase]{

  val ids = mutable.Map[String, GlueBase]()

  override def clear() {ids.clear()}

  override def index_(item: GlueBase): Option[GlueBase] = {
    ids(item.asInstanceOf[Item].uuid) = item
    Some(item)
  }
}

class GlueServiceSercher(index: GlueServiceIndex) extends GlueSearcher {
  def find(id: String): GlueBase = index.ids(id)
}