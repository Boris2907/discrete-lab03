package lv.rbs.ds.lab03


import play.api.libs.json.{JsValue, Json}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class KMPmatcher(var pattern: String ){
  // Add some initialization steps -> you can compute the prefix function...
  // loops...
  var comparisons : Int = 0
  def getPrefixFun(): ArrayBuffer[Int] = {
    val lookupTable = ArrayBuffer.fill(pattern.length)(-1)
    lookupTable(0) = 0 // first char always 0
    var len = 0
    var i = 1
    while( i < pattern.length) {
      if (pattern(i) == pattern(len)) {
        len += 1
        lookupTable(i) = len
        i += 1
      } else { // mismatch
        if (len == 0) {
          lookupTable(i) = 0;
          i= i+1
        } else {
          len = lookupTable(len-1)
        }
      }
    }
    lookupTable
  }

  def findAllIn(text: String): Iterator[Int] = {
    var result: ArrayBuffer[Int] = ArrayBuffer()

    if (pattern.length > text.length) println("bad input")

    else if (pattern == text) println("same strings")
    else {
      val lookupTable = getPrefixFun()
      lookupTable.drop(1)

      var i= 0 // for pattern
      var j= 0 // for searchIn

      while (i < text.length) {
        this.comparisons += 1 // count all comparisons
        if (text(i) == pattern(j)) {
          i += 1
          j += 1


        }
        if (j == pattern.length) {
          println(s"pattern found at ${i-j}")
          result += (i-j)

          j = lookupTable(j-1)
        }
        else {

          if (i < text.length && text(i) != pattern(j)) {
            if (j != 0) j = lookupTable(j-1)
            else i+=1

          }
        }
      }
    }
    this.comparisons += 3 // because it does 3 comparisons outside while loop
    println(comparisons)
    result.toIterator

  }

  def toJson(text: String): String = {

    val jsonAlgorithm: JsValue = Json.toJson("KMP")
    val jsonPattern: JsValue = Json.toJson(pattern)
    val jsonText: JsValue = Json.toJson(text.toString)
    var prefixFunElements = getPrefixFun
    var prefixFunList = new ListBuffer[List[Int]]
    findAllIn(text)

    for (i <- prefixFunElements.indices) {
      prefixFunList.append(List(i, prefixFunElements(i)))
    }
    val jsonPrefixFun: JsValue = Json.toJson(prefixFunList.toList)
   /* val jsonSteps: JsValue = Json.toJson(listForSteps) //List[Map[String, String]]*/
    val jsonComparisons: JsValue = Json.toJson(comparisons)

    val jsonMap: Map[String, JsValue] = Map(
      "algorithm" -> jsonAlgorithm,
      "pattern" -> jsonPattern,
      "text" -> jsonText,
      "prefixFun" -> jsonPrefixFun,
     // "steps" -> jsonSteps,
     "comparisons" -> jsonComparisons
    )
    val result = Json.stringify(Json.toJson(jsonMap))
    println("Comparisons: " + comparisons)
    result

  }
}
