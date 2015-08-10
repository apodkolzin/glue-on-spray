package ru.naumen.lucene

import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.util.Version
import org.apache.lucene.store.{RAMDirectory, Directory}
import org.apache.lucene.index.{IndexReader, Term, IndexWriter, IndexWriterConfig}
import org.apache.lucene.document.{Field, Document}
import org.apache.lucene.search.{ScoreDoc, IndexSearcher}
import scala.collection.JavaConversions._
import ru.naumen.indexes.{GlueIndex, GlueSearcher, GlueMetaIndex}
import ru.naumen.rep.{GlueMeta, GlueUnit}
import org.apache.lucene.queryparser.classic.QueryParser
import scala.collection.mutable

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 27.02.15
 * Time: 12:56
 * Since: 
 *
 */
class GlueLuceneSearcher(val index: GlueLuceneIndex, val units: GlueMetaIndex) extends GlueSearcher{
  lazy val parser = new QueryParser("content", index.analyzer)

  def find(query: String): Array[GlueUnit] = {
    val set = mutable.LinkedHashSet[String]()
    set ++= index.find(query)
    set.map(units.get).filter(_.isDefined).map(_.get).toArray
  }

}

class GlueLuceneIndex extends GlueIndex[GlueMeta] with RamLuceneIndex[GlueMeta]{

  //todo apodkolzin ошибочно ищет при перезагрузке репозитория, предположительно из-за  смены идентификаторов
  def key(meta: GlueMeta) = meta.id
  def content(meta: GlueMeta) = Seq(meta.title, meta.titleEn, meta.id) ++ meta.keywords

  override def clear() {}
  override def index_(item: GlueMeta): Option[GlueMeta] = { relucene(item); Some(item) }
}


trait RamLuceneIndex[T] {
  lazy val analyzer = new StandardAnalyzer(Version.LUCENE_36)
  lazy val index: Directory = new RAMDirectory
  lazy val parser = new QueryParser(Version.LUCENE_36, "content", analyzer)

  def key(obj: T): String
  def content(obj: T): Seq[String]

  def minQuery: Int = 2

  def find(queryString: String): Seq[String] = {

    if (Option(queryString).map(_.length).getOrElse(0) <= minQuery)
      return Seq()
    val reader = IndexReader.open(index)
    val searcher = new IndexSearcher(reader)
    try {
      parser.setAllowLeadingWildcard(true)
      val query = parser.parse(queryString)
      val docs: Array[ScoreDoc] = searcher.search(query, 20).scoreDocs
      docs.toSeq.map(tip => searcher.doc(tip.doc).get("key"))
    }
    finally {
      reader.close
    }
  }

  def relucene(obj: T) {
    unlucene(obj)
    lucene(obj)
  }

  def lucene(obj: T)  {
    val writer: IndexWriter = indexWriter
    try {
      addDoc(writer, key(obj), content(obj):_*)
    }
    finally {
      writer.close
    }
  }

  def unlucene(obj: T)  {
    val writer: IndexWriter = indexWriter
    try  {
      writer.deleteDocuments(new Term("key", key(obj)))
    }
    finally {
      writer.close
    }
  }

  private def indexWriter: IndexWriter = {
    val conf: IndexWriterConfig = new IndexWriterConfig(Version.LUCENE_36, analyzer)
    //    conf.setRAMBufferSizeMB(128.0)
    new IndexWriter(index, conf)
  }

  private def addDoc(writer: IndexWriter, key: String, content: String*) {
    val doc: Document = new Document
    doc.add(new Field("key", key, Field.Store.YES, Field.Index.ANALYZED))
    val join: String = content.mkString(" ")
    doc.add(new Field("content", join, Field.Store.YES, Field.Index.ANALYZED))
    writer.addDocument(doc)
  }


}