package nlp_serde.annotators.relations

import org.sameersingh.nlp_serde.Token
import nlp_serde.immutable.Dep
import scala.collection.mutable.{ArrayBuffer, HashMap}
import scala.collection.mutable

/**
 * @author sameer
 * @since 9/21/14.
 */
class DepTreeTraversal(tree: Seq[Dep]) {

  class Node(val idx: Int, var parent: Node = null, var label: String = "", val children: ArrayBuffer[Node] = new ArrayBuffer[Node])

  val nodes = new HashMap[Int, Node]
  var root: Node = null
  for (d <- tree) {
    // get parent node
    val source = if (d.source > 0) Some(d.source) else None
    val pnode = source.map(t => nodes.getOrElseUpdate(d.source, new Node(t)))
    // new node for target
    if (pnode.isEmpty) {
      assert(d.label == "root")
      val tnode = nodes.getOrElseUpdate(d.target, new Node(d.target, null, d.label))
      tnode.parent = null
      tnode.label = d.label
      assert(root == null)
      root = tnode
    } else {
      val p = pnode.get
      val tnode = nodes.getOrElseUpdate(d.target, new Node(d.target))
      tnode.parent = p
      tnode.label = d.label
      p.children += tnode
    }
  }

  def pathToRoot(a: Int): List[Int] = {
    val n = nodes(a)
    if (n == root) List(a)
    else {
      n.idx :: pathToRoot(n.parent.idx)
    }
  }

  def commonAncestor(a1: Int, a2: Int): Int = {
    val pa1 = pathToRoot(a1).reverse
    val pa2 = pathToRoot(a2).reverse

    (pa1 zip pa2).takeWhile(ab => ab._1 == ab._2).map(_._1).last
  }

  def dfs(a: Int): Seq[Int] = {
    val n = nodes(a)
    if (n.children.isEmpty) Seq(n.idx)
    else n.children.foldLeft(Seq.empty[Int])((l, c) => l ++ dfs(c.idx)) ++ Seq(n.idx)
  }

  def bfs(a: Int): Seq[Int] = {
    val queue = new mutable.Queue[Int]
    val result = new ArrayBuffer[Int]
    queue += a
    while (!queue.isEmpty) {
      val a = queue.dequeue()
      result += a
      queue ++= nodes(a).children.map(_.idx)
    }
    result
  }
}
