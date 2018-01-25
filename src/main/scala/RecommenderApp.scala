object RecommenderApp extends App {
  val articleMap = Map(
    1 -> "As their name implies, ETFs are investment funds that are traded on an exchange, such as the New York Stock Exchange (NYSE) or NASDAQ. ETFs often correspond to a particular size of company, industrial sector, market, or even social goal. So, you could own shares in an ETF that owns blue chip stocks of large companies, or the stocks of less well-known, smaller companies.",
    2 -> "A Roth IRA is different from both a traditional 401(k) and traditional IRA in some pretty important ways. Your earnings and interest also grow without having to pay taxes on the income until you retire.",
    3 -> "A REIT can also be an ETF. And like an ETF it trades throughout the day like anything else on an exchange, such as a stock or a bond. There are about 200 REIT funds in the U.S. and they can be structured in different ways. The majority own actual real estate. Others can own debt, such as residential or commercial mortgages. Real Estate Tycoon, also known as VNQ or the Vanguard REIT Index Fund, tracks an index of publicly traded REITs. It only invests in shares of trusts that actually own real estate property.",
    4 -> "Robots have risen up and are taking over the planet. Don’t panic yet. Robot revolution is one of the themes of “Blade Runner 2049”, the sequel to Ridley Scott’s 1982 cult classic “Blade Runner”, which hit theaters a couple of weeks ago. Both films deal with a futuristic dystopian world where corporations wield too much control and environmental disaster looms, with engineered artificial humans, called replicants, running amok.",
    5 -> "For most people, taking the plunge into opening a retirement account can be a little scary and confusing. But there really are two numbers you need to consider to get started: $5,500: That’s the full amount you can invest per year into all IRAs in your name. So if this Retire account is your only IRA, you can invest up to $5,500. 59 ½: That’s the age you can start making withdrawals from your account. After this time, any distributions taken from your Traditional IRA will be taxed at your ordinary income rate.",
    6 -> "President Trump has named Jerome Powell as the next chairman of the nation’s central bank.",
    7 -> "The Combat Carbon fund invests in many of these companies, and it may help you meet your green investment goals. The businesses in the fund are actively trying to reduce the greenhouse gases they emit, for example by buying carbon offsets, purchasing electricity produced by wind and solar, or by investing in manufacturing materials that are more sustainable.",
    8 -> "Apple, for example, emits 30 million metric tons of carbon, primarily from manufacturing and the use of its products, the company reports. It’s working to reduce its footprint by sourcing lower carbon products in its manufacturing process like aluminum, and using renewable energy for 96% of its headquarters electricity usage.",
    9 -> "The High Voltage ETF on Stash invests in privately-owned utilities that produce electricity, natural gas, and water. Nearly 60% of the stocks held in the fund are for electricity producers, according to Vanguard, which has created the fund.",
    10 -> "401(k)s and IRAs are types of savings accounts that allow you to invest in stocks, bonds, mutual funds, certificates of deposit, ETFs and index funds, among other investments, using your pre-tax dollars.",
    11 -> "Real Estate Tycoon is one of the largest REITs by asset size. The fund holds the stock of 156 company REITs. This fund invests in office parks, apartment complexes, health care facilities, and residential complexes.")
  
  def findRecommendationsForArticle(a: Int): Unit = {
    assert(a < articleMap.size && 0 < a, s"$a is not a member of articles try a number between 1-11")
    
    val article = articleMap(a)
    
    // larger nGram size means greater likelihood of match three is a good tradeoff
    val recommender = new Recommender(articleMap, nGramSize = 3)
    val recommendations = recommender.recommend(article)
    
    println(s"Article Liked: \n$article")
    println("------------------------------")
    println("Similar Articles:")
    recommendations.map(articleMap).foreach(println)
  }
  
  // Pass in
  findRecommendationsForArticle(args(0).toInt)
}

class Recommender(corpus: Map[Int, String], nGramSize: Int = 3) {
  lazy val stopWords = Array("a", "in", "the", "and", "as", "on", "of", "so", "an", "that", "or", "to", "s", "i", "my", "you", "are", "t", "but", "not", "is", "it", "e", "en", "with", "can", "having")
  lazy val mappedGrams = corpus.mapValues(nGram(nGramSize))
  
  def recommend(article: String, threshold: Int = 2): List[Int] = {
    val testGrams = nGram(nGramSize)(article)
    
    //threshold supplies minimum number of nGram groups must match
    mappedGrams.filter { case (_, v) => compare(v, testGrams) >= threshold}.keys.toList
  }
  
  private[this] def nGram(n: Int)(str: String): List[Array[String]] = {
    str.trim
      .toLowerCase
      .split("\\W+")
      .filterNot(w => stopWords.contains(w))
      .sliding(n)
      .toList
  }
  
  private[this] def compare(a: List[Array[String]], test: List[Array[String]]): Int = {
    val article = a.sortBy(a => a.head)
    val liked = test.sortBy(t => t.head)
    
    //A match is when two
    article.zip(liked).count(p => (p._1 intersect p._2).lengthCompare(0) > 0)
  }
}
