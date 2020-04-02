package lv.rbs.ds.lab03

/**
 * This object stores all information to generate JSON.
 * If prefixFun (or goodSuffixFun, badCharFun) are not needed, the respective lists are empty
 */
class SearchTranscript(algorithm: String, pattern: String, text: CharSequence, steps: List[(Int,Int,Int,Int)]) {

  /**
   * @return Return the number of all comparisons in all steps:
   *         It is computed as the sum over all steps: (end-start+1)
   */
  def getTotalComparisons(): Int = {
    steps.foldLeft(0)((sum, step) => sum + (step._3 - step._2) + 1)
  }

  /*def getJson(): String = {
     val json:JsValue = ("algorithm" -> algorithm) ~
      ("pattern" -> pattern) ~
      ("text" -> text.toString) ~
      ("steps" -> steps.map {
            step => {
              val (offset, start, end, isMatch) = step
              if (isMatch == 0) (("offset" -> offset) ~
                ("start" -> start) ~
                ("end" -> end))
              else (("offset" -> offset) ~
                ("start" -> start) ~
                ("end" -> end) ~
                ("match" -> "true"))
            }
      }) ~
      ("comparisons" -> getTotalComparisons())
    prettyRender(json)
  }*/


}
