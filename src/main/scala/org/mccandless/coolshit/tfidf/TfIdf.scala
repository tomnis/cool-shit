package org.mccandless.coolshit.tfidf

import scala.math.{log, max}

import Types._

object Types {
  type Term = String
  type Document = List[Term]
  type Corpus = List[Document]
  // we don't want to waste memory with explicit storage of zeros
  type SparseVector = Map[Term, Double]

  type Embedding = List[SparseVector]
}

trait VectorSpace {
  def embed(D: Corpus): Embedding
}


trait NaiveHistogram extends VectorSpace {
  def histogram(d: Document): SparseVector = d.groupBy(a => a).mapValues(_.size)

  /** Summarize a corpus by naively computing histograms for each document. */
  override def embed(D: Corpus): Embedding = D.map(histogram)
}

/**
 *
 * Created by tdm on 4/15/21.
 */
trait TfIdf extends VectorSpace {
  def tf(t: Term, d: Document): Double = d.count(_ == t).toDouble / d.size

  def idf(t: Term, D: Corpus): Double = log(D.size.toDouble / max(D.count(_ contains t), 0.000001))

  def tfidf(t: Term, d: Document, D: Corpus): Double = tf(t, d) * idf(t, D)

  def tfidfs(d: Document, D: Corpus): SparseVector = d.toSet.map((t: Term) => (t, tfidf(t, d, D))).toMap

  override def embed(D: Corpus): Embedding = D.map(tfidfs(_, D))
}


