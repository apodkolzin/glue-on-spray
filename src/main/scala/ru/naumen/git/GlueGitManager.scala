package ru.naumen.git
import org.eclipse.jgit.api._
import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.treewalk.CanonicalTreeParser
import java.io.File
import collection.JavaConversions._
import org.eclipse.jgit.transport.UsernamePasswordCredentialsProvider
import ru.naumen.rep.GlueKey

/**
 * Created by IntelliJ IDEA.
 * User: Andrew F. Podkolzin
 * Date: 12.12.14
 * Time: 14:40
 * Since: 
 *
 */
class GlueGitManager(val root: String) {

  private lazy val git: Git = Git.open(new File(root))
  private lazy val repository: Repository = git.getRepository()

  def update: Seq[GlueKey] = {
    supdate map gluekey
  }

  def supdate: Seq[String] = {
    val oldHead = repository.resolve("HEAD^{tree}")
    //pull
    val cp = new UsernamePasswordCredentialsProvider("apodkolzin", "");
    git.pull().setCredentialsProvider(cp).call()
    val head = repository.resolve("HEAD^{tree}")

    val reader = repository.newObjectReader()
    val oldTreeIter = new CanonicalTreeParser()
    oldTreeIter.reset(reader, oldHead)
    val newTreeIter = new CanonicalTreeParser()
    newTreeIter.reset(reader, head)

    git
      .diff()
      .setNewTree(newTreeIter)
      .setOldTree(oldTreeIter)
      .call()
      .toSeq
      .map(_.getNewPath)



    /*
    // a RevWalk allows to walk over commits based on some filtering that is
    // defined
    val walk = new RevWalk(repository)

    val commit = walk.parseCommit(head.getObjectId())
    val tree = commit.getTree()
    System.out.println("Having tree: " + tree);

    // now use a TreeWalk to iterate over all files in the Tree recursively
    // you can set Filters to narrow down the results if needed
    val treeWalk = new TreeWalk(repository)
    treeWalk.addTree(tree)
    treeWalk.setRecursive(true)
    while (treeWalk.next()) {
      System.out.println("found: " + treeWalk.getPathString())
    }
    */
  }

  def fullpath(path: String) = root + "/" + path
  def gluekey(path: String ) = GlueKey(fullpath(path))

}
