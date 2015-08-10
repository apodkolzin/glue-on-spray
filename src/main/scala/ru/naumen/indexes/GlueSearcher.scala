package ru.naumen.indexes

import ru.naumen.rep.{GlueFolder, GlueKey, GlueUnit}

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 01.12.14
 * Time: 16:33
 * Since: 
 *
 */
trait GlueSearcher

class GlueUnitSearcher(val metas: GlueMetaIndex) extends GlueSearcher {
  def find(key: String): GlueUnit = metas.get(key).orNull
}

//todo apodkolzin описать зачем нужен этот сложный серчер
//todo apodkolzin нужно хорошее тестирование: создание папок различной степени вложенности, обновление метаинформации
class GlueUpdatedSearcher(val folders: GlueFolderIndex) extends GlueSearcher {
  def find(keys: GlueKey*): Array[(GlueKey, GlueFolder)] = {
    val res = keys
      .toSet
      .flatMap(what)
      .map(pack)
      .toArray
    res
  }

  private def pack(key: GlueKey) = (key, folder(key.parent).orNull) //todo (?) apodkolzin use Option


  //todo apodkolzin написать зачем так нужно сложно
  private def what(current: GlueKey): Option[GlueKey] = {
    // если существует соответствующий файл
    if (current.exists ) {
      // получаем папку, в которой он находится
      val  parent = current.parent
      // проверяем если ли она в репозитории
      if (folder(parent).isDefined)
        // если есть то
        Some(current)
      else
      // если нет то в рекурсии поиска родительской папки, уже загруженной в репозиторий
        what(parent)
    }
    else
      None
  }

  private def folder(key: GlueKey) = folders.get(key)
}

