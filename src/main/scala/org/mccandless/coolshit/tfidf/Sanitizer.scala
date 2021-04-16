package org.mccandless.coolshit.tfidf

import Types._

/**
 *
 * Created by tdm on 4/15/21.
 */
object Implicits {
  implicit class StringOps(s: String) {
    def sanitize: Document = s.replaceAll("[,.:]", "").split(" ").map(_.trim.toLowerCase).toList
  }

  implicit def string2Document(s: String): Document = s.sanitize
}
