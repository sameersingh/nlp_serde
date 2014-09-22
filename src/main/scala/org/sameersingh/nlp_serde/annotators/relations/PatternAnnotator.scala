package org.sameersingh.nlp_serde.annotators.relations

import org.sameersingh.nlp_serde.annotators.Annotator
import org.sameersingh.nlp_serde._
import org.sameersingh.nlp_serde.immutable
import edu.stanford.nlp.dcoref.Dictionaries.MentionType
import scala.Some
import org.sameersingh.nlp_serde.Relation

/**
 * Slight reliance on Stanford dependencies
 * @author sameer
 * @since 9/21/14.
 */
class PatternAnnotator extends Annotator {

  def lexicalizedPath(s: Sentence, m1: Mention, m2: Mention, t: DepTreeTraversal): String = {
    def tokenToStr(t: Token) = if (t.ner.getOrElse("O") == "O") t.lemma.get else t.ner.get

    val h1 = s.tokens(m1.headTokenIdx - 1)
    val h2 = s.tokens(m2.headTokenIdx - 1)
    val p1 = t.pathToRoot(h1.idx)
    val p2 = t.pathToRoot(h2.idx)
    val p = t.commonAncestor(h1.idx, h2.idx)
    val tp = s.tokens(p - 1)
    // paths to common ancestor
    val p1p = p1.takeWhile(_ != p)
    val p2p = p2.takeWhile(_ != p)
    (p1p.flatMap(i => Seq(tokenToStr(s.tokens(i - 1)), "<-", t.nodes(i).label, "<-"))
      ++ Seq(tokenToStr(tp))
      ++ p2p.reverse.flatMap(i => Seq("->", t.nodes(i).label, "->", tokenToStr(s.tokens(i - 1))))).drop(1).dropRight(1).mkString("")
  }

  override def process(doc: Document): Document = {
    // check for annotations
    assert(doc.sentences.forall(s => s.depTree.isDefined))

    // all sentences
    for (s <- doc.sentences) {
      val traversal = new DepTreeTraversal(s.depTree.get)
      println("Sen: " + s.text)
      println("Deps: " + s.depTree.get)

      // all pairs of mentions (that are not the same, and are not coreferent
      for (m1 <- s.mentions; m2 <- s.mentions;
           if (m1 != m2);
           if (m1.posInSentence < m2.posInSentence);
           if (m1.entityId.isDefined);
           if (m2.entityId.isDefined);
           if (m1.mentionType != Some(MentionType.NOMINAL.toString));
           if (m2.mentionType != Some(MentionType.NOMINAL.toString));
           if (m1.headTokenIdx != m2.headTokenIdx);
           if (m1.entityId.get != m2.entityId.get)) {
        print("\"%s\",\"%s\": " format(m1.text, m2.text))
        // println("e:%d,%d\th:%d,%d" format(m1.entityId.get, m2.entityId.get, m1.headTokenIdx, m2.headTokenIdx))
        // println("%s, %s" format(m1.mentionType, m2.mentionType))
        val lp = lexicalizedPath(s, m1, m2, traversal)
        if (lp.length > 0) s.relations += new Relation(immutable.Relation(m1.id, m2.id, Set(lp), Map.empty))
        println(lp)
      }
    }
    doc
  }
}
