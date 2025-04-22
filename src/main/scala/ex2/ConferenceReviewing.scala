package ex2

/**
 * An interface modelling the results of reviewing articles of a conference.
 * Each reviewer reads an article, and answers to a number of questions with a score from 0 to 10.
 * Note: each article can be reviewed by many reviewers, but the system does not keep track of their identity.
 */
object ConferenceReviewing {
  enum Question {
    case RELEVANCE
    case SIGNIFICANCE
    case CONFIDENCE
    case FINAL
  }
  opaque type Article = Int
  opaque type Score = Int | Double

  def article(id: Int): Article = id
  def score(value: Int | Double): Score = value
}

trait ConferenceReviewing {
  import ConferenceReviewing.*
  /**
   * loads a review for the specified article, with complete scores as a map
   */
  def loadReview(article: Article, scores: Map[Question, Score]): Unit

  /**
   * loads a review for the specified article, with the 4 explicit scores
   */
  def loadReview(article: Article, relevance: Score, significance: Score, confidence: Score, fin: Score): Unit

  /**
   * @return the scores given to the specified article and specified question, as an (ascending-ordered) list
   */
  def orderedScores(article: Article, question: Question): List[Score]

  /**
   * @return the average score to question FINAL taken by the specified article
   */
  def averageFinalScore(article: Article): Score

  /**
   * An article is considered accepted if its averageFinalScore (not weighted) is > 5,
   * and at least one RELEVANCE score that is >= 8.
   * @return the set of accepted articles
   */
  def acceptedArticles: Set[Article]

  /**
   * @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore
   */
  def sortedAcceptedArticles: List[(Article, Score)]

  /**
   * @return a map from articles to their average "weighted final score", namely,
   *         the average value of CONFIDENCE*FINAL/10
   *         Note: this method is optional in this exam
   */
  def averageWeightedFinalScoreMap: Map[Article, Score]
}

class ConferenceReviewingImpl extends ConferenceReviewing {
  export ConferenceReviewing.*

  override def loadReview(article: Article, scores: Map[ConferenceReviewing.Question, Score]): Unit = ???

  override def loadReview(article: Article, relevance: Score, significance: Score, confidence: Score, fin: Score): Unit = ???

  override def orderedScores(article: Article, question: ConferenceReviewing.Question): List[Score] = ???

  override def averageFinalScore(article: Article): Score = ???

  override def acceptedArticles: Set[Article] = ???

  override def sortedAcceptedArticles: List[(Article, Score)] = ???

  override def averageWeightedFinalScoreMap: Map[Article, Score] = ???
}

/**
 * Interfaccia ConferenceReviewing: modella i risultati del processo di revisione degli articoli di una conferenza.
 * Ogni articolo revisionato da 1 o + revisori anonimi, ognuno fornisce una valutazione (score) da 0-10 per 4 diverse "domande", modellate da Question.
 * Un articolo viene accettato se il valore medio della valutazione alla domanda "FINAL" è >5 e se ha almeno una valutazione "RELEVANCE" >= 8.
 * ConferenceReviewingImpl con costruttore senza argomenti.
 */
@main def testConferenceReviewing(): Unit =
  import ConferenceReviewing.*

  val cr: ConferenceReviewing = new ConferenceReviewingImpl()
  val article1 = article(1)
  val article2 = article(2)
  val article3 = article(3)
  val article4 = article(4)
  val article5 = article(5)

  def init(): Unit =
    // Ordine domande: relevance, significance, confidence, final
    cr.loadReview(article1, score(8), score(8), score(6), score(8))  // 4.8 -> voto finale pesato (usato da averageWeightedFinalScoreMap)
    cr.loadReview(article1, score(9), score(9), score(6), score(9))  // 5.4
    cr.loadReview(article1, score(9), score(9), score(10), score(9)) // 9.0
    cr.loadReview(article2, score(4), score(6), score(10), score(6)) // 6.0
    cr.loadReview(article3, score(3), score(3), score(3), score(3))  // 0.9
    cr.loadReview(article3, score(4), score(4), score(4), score(4))  // 1.6
    cr.loadReview(article4, score(6), score(6), score(6), score(6))  // 3.6
    cr.loadReview(article4, score(7), score(7), score(8), score(7))  // 5.6
    cr.loadReview(article5, score(6), score(6), score(6), score(10)) // 6.0
    cr.loadReview(article5, score(7), score(7), score(7), score(10)) // 7.0

    var map: Map[Question, Score] = Map()
    map = map + ((Question.RELEVANCE, score(8)))
    map = map + ((Question.SIGNIFICANCE, score(8)))
    map = map + ((Question.CONFIDENCE, score(7)))
    map = map + ((Question.FINAL, score(8)))
    cr.loadReview(article4, map)

  def testOrderedScores(): Unit =
    assert(cr.orderedScores(article2, Question.RELEVANCE) == List(score(4), score(9)))
    assert(cr.orderedScores(article4, Question.CONFIDENCE) == List(score(6), score(7), score(8)))
    assert(cr.orderedScores(article5, Question.FINAL) == List(score(10), score(10)))

  def testAverageFinalScore(): Unit =
    assert(cr.averageFinalScore(article1) == score(8.5)) // l'articolo 1 ha preso voto medio su FINAL pari a 8.5, con scarto massimo 0.01
    assert(cr.averageFinalScore(article2) == score(7.5))
    assert(cr.averageFinalScore(article3) == score(3.5))
    assert(cr.averageFinalScore(article4) == score(7.0))
    assert(cr.averageFinalScore(article5) == score(10.0))

  def testAcceptedArticles(): Unit =
    // solo gli articoli 1,2,4 vanno accettati, avendo media finale >=5 e almeno un voto su RELEVANCE >= 8
    assert(cr.acceptedArticles == Set(article1, article2, article4))

  def testSortedAcceptedArticles(): Unit =
    // articoli accettati, e loro voto finale medio
    assert(cr.sortedAcceptedArticles == List((article4, 7.0), (article2, 7.5), (article1, 8.5)))

  def optionalTestAverageWeightedFinalScore(): Unit =
    // l'articolo 1 ha media pesata finale pari a (4.8+5.4)/2 = 5,1
    assert(cr.averageWeightedFinalScoreMap.get(article1).contains((4.8 + 5.4) / 2))
    assert(cr.averageWeightedFinalScoreMap.get(article2).contains((9.0 + 6.0) / 2))
    assert(cr.averageWeightedFinalScoreMap.get(article3).contains((0.9 + 1.6) / 2))
    assert(cr.averageWeightedFinalScoreMap.get(article4).contains((3.6 + 5.6 + 5.6) / 3))
    assert(cr.averageWeightedFinalScoreMap.get(article5).contains((6.0 + 7.0) / 2))
    assert(cr.averageWeightedFinalScoreMap.size == 5)

  init()
  testOrderedScores()
  testConferenceReviewing()
  testAverageFinalScore()
  testAcceptedArticles()
  testSortedAcceptedArticles()
  optionalTestAverageWeightedFinalScore()