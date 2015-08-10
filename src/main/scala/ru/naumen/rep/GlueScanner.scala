package ru.naumen.rep

import java.io.File
import ru.naumen.indexes.GlueIndexes

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 08.06.15
 * Time: 12:31
 * Since: 
 *
 */

// реализация сканера для инициализации репозитория  Glue
class GlueScanner(val indexes: GlueIndexes with GlueSearcherFactory) extends GlueItemFactory with ScannerFactory[GlueElement, GlueFolder]{
  val factory: GlueFactory = new GlueFactoryImpl(indexes)

  override def dirScanner = {folder: GlueFolder =>  new DirScanner[GlueFolder]{
    override def factory: ScannerFactory[_, GlueFolder] = GlueScanner.this
    override def handleDir: (File) => GlueFolder = newFolder(folder)
  }}

  override def fileScanner = {folder: GlueFolder =>  new FileScanner[GlueElement]{
    override def handleFile: (File) => GlueElement = newElement(folder)
  }}

  def scan(root: GlueKey): GlueFolder = dirScanner(null).scan(root.file)

  def scan(key: GlueKey, folder: GlueFolder) = createScanner(key.file, folder).scan(key.file)

}

trait GlueItemFactory{
  val factory: GlueFactory

  def newElement(folder: GlueFolder)(file: File): GlueElement = {
    val key = GlueKey(file)
    val unit: GlueUnit = folder.unit(key.uname).getOrElse(factory.newUnit(key, folder))
    factory.newElement(key, unit)
  }

  def newFolder(folder: GlueFolder)(file: File): GlueFolder = factory.newFolder(GlueKey(file), folder)
}

// абстрактная часть сканера файловой системы, без реализации обработки
trait Scanner[T]{
  def scan(file: File): T
}

trait FileScanner[F] extends Scanner[F]{
  def handleFile: File => F
  def scan(file: File) =  handleFile(file)
}

trait DirScanner[D] extends Scanner[D]{
  def factory: ScannerFactory[_, D]
  def handleDir: File => D

  def scan(file: File) = {
    val result: D = handleDir(file)
    file
      .listFiles()
      .toSeq
      .filter(!_.getName.startsWith("."))
      .foreach(f => factory.createScanner(f, result).scan(f))
    result
  }
}

trait ScannerFactory[F, D]{
  def dirScanner: D => DirScanner[D]
  def fileScanner: D => FileScanner[F]

  def createScanner(file: File, d: D): Scanner[_] = {
    if (file.isDirectory)
      dirScanner(d)
    else
      fileScanner(d)
  }
}

