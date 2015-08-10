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

/**
 * Реализует поиск элементов репозитория, измененных (добавленных, измененных, удаленных) при обновлении репозитория.
 * Требуется для синхронизации загруженного репозитория с изменениями на файловой системе.
 *
 * При добавлении нового файла или папки - выполняется поиск в репозитории родительской папки GlueFolder для последующий к ней привязки нового элемента
 * Поиск того, что было дабавлено: папка или файл реализовано в методе what
 *
 * @param folders - индекс папок в репозитории
 */
class GlueUpdatedSearcher(val folders: GlueFolderIndex) extends GlueSearcher {
  def find(keys: GlueKey*): Array[(GlueKey, GlueFolder)] = {
    val res = keys
      .toSet
      .flatMap(what)
      .toArray
    res
  }

  /**
   *  Находит высший новый элемент в иерархии файлов и папок, который был добавлен, и папку, в которой он находится
   *  Этот элемент в последующем будет привязан к найденной папке и с него запуститься (пере)индексация
   * @param current - ключ, соответсвующий обновляемому файлу
   * @return пара (ключ, папка), где
   *         папка - ближайший предок из репозитория для входного ключа current и
   *         ключ - либо current либо его высший предок, отсутвующий в репозиторий
   */
  private def what(current: GlueKey): Option[(GlueKey, GlueFolder)] = {
    // если файл,соответствующий ключ, действительно существует
    if (current.exists ) {
      // получаем папку, в которой он находится
      val  parent = current.parent
      // если данная папка найдена в репозитории
      // то возвращаем пару (ключ, папка)
      // иначе повторяем поиск по ключу уже этой папки
      folder(parent).map((current, _)).orElse(what(parent))
    }
    else
      None
  }

  private def folder(key: GlueKey) = folders.get(key)
}

