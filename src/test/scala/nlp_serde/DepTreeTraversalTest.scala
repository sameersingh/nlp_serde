package nlp_serde

import nlp_serde.annotators.relations.DepTreeTraversal
import nlp_serde.immutable.Dep
import org.junit.Assert._
import org.junit.Test

/**
 * @author sameer
 * @since 9/21/14.
 */
class DepTreeTraversalTest {

  @Test
  def testEdge = {
    val deps = Seq(Dep("root", 0, 1))
    val t = new DepTreeTraversal(deps)
    assertEquals(List(1), t.pathToRoot(1))
    assertEquals(1, t.commonAncestor(1, 1))
    assertEquals(Seq(1), t.dfs(1))
    assertEquals(Seq(1), t.bfs(1))
  }

  @Test
  def testChain = {
    val len = 5
    val deps = (0 to len).map(i => if (i == 0) Dep("root", i, i + 1) else Dep("lab" + i, i, i + 1))
    val t = new DepTreeTraversal(deps)
    for (i <- 1 to (len + 1)) {
      assertEquals((1 to i).reverse.toList, t.pathToRoot(i))
      assertEquals((i to len + 1).reverse, t.dfs(i))
      assertEquals((i to len + 1), t.bfs(i))
      for (j <- 1 to (len + 1)) {
        assertEquals(math.min(i, j), t.commonAncestor(i, j))
      }
    }
  }

  @Test
  def testBinaryTree = {
    val depth = 4

    val deps = (1 until math.pow(2, depth).toInt).map(i => {
      if (i == 1) Dep("root", 0, 1)
      else {
        Dep("lab" + i, ((i - 2) / 2) + 1, i)
      }
    })

    println(deps)

    val t = new DepTreeTraversal(deps)
    for (i <- 1 until math.pow(2, depth).toInt) {
      print(i + "\t")
      print(t.pathToRoot(i) + "\t")
      print(t.dfs(i) + "\t")
      print(t.bfs(i) + "\n")
    }
  }
}
