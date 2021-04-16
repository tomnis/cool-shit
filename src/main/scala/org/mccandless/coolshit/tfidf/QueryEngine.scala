package org.mccandless.coolshit.tfidf

import Types._

/**
 *
 * Created by tdm on 4/15/21.
 */
trait QueryEngine {


  def query(q: Document, em: Embedding): List[(Document, Double)]
}
