package org.mccandless.coolshit.tfidf

import org.mccandless.coolshit.BaseSpec
import Types._

import Implicits._
import TestData._

/**
 *
 * Created by tdm on 4/15/21.
 */
class NaiveHistogramSpec extends BaseSpec with NaiveHistogram {

  "naive" should "compute" in {
    histogram(d3) should be (Map("lorum" -> 2.0, "ipsem" -> 1.0))
  }
}

class TfIdfSpec extends BaseSpec with TfIdf {
  "tf-idf" should "compute" in {
    embed(D).foreach(println)
  }
}

object TestData {

  val d1: Document = "And I would say she started to grow a social consciousness, a sense that problems in the United States had social and structural origins."
  val d2: Document = "Timothy Theodore Duncan (born April 25, 1976)[1] is an American former professional basketball player and coach. Nicknamed \"the Big Fundamental\", he is widely regarded as the greatest power forward of all time and one of the greatest players in NBA history.[2][3][4] He spent his entire 19-year playing career with the San Antonio Spurs.\n\nDuncan started out as an aspiring swimmer and only began playing basketball in ninth grade, when Hurricane Hugo destroyed the only available Olympic-sized pool in his homeland of Saint Croix, U.S. Virgin Islands. In high school, he played basketball for St. Dunstan's Episcopal. In college, Duncan played for the Wake Forest Demon Deacons, and in his senior year, he received the John Wooden Award and was named the Naismith College Player of the Year and the USBWA College Player of the Year.\n\nAfter graduating from college, Duncan was the NBA Rookie of the Year after being selected by San Antonio with the first overall pick in the 1997 NBA draft. He primarily played the power forward position and also played center throughout his career. He is a five-time NBA champion, a two-time NBA MVP, a three-time NBA Finals MVP, a 15-time NBA All-Star,[5] and the only player to be selected to both the All-NBA and All-Defensive Teams for 13 consecutive seasons."
  val d3: Document = "lorum ipsem lorum"
  val d4: Document = "Last week, the CDC acknowledged what many of us have been saying for almost nine months about cleaning surfaces to prevent transmission by touch of the coronavirus: It’s pure hygiene theater.\n\n“Based on available epidemiological data and studies of environmental transmission factors,” the CDC concluded, “surface transmission is not the main route by which SARS-CoV-2 spreads, and the risk is considered to be low.” In other words: You can put away the bleach, cancel your recurring Amazon subscription for disinfectant wipes, and stop punishing every square inch of classroom floor, restaurant table, and train seat with high-tech antimicrobial blasts. COVID-19 is airborne: It spreads through tiny aerosolized droplets that linger in the air in unventilated spaces. Touching stuff just doesn’t carry much risk, and more people should say so, very loudly."

  val D: Corpus = List(d1, d2, d3, d4)
}