package lv.rbs.ds.lab03

import play.api.libs.json.{JsValue, Json}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class BMmatcher(pattern: String) {


  /**
   * This method should return a list of length m (length of the pattern).
   * Good Suffix function
   */
  var comparisons: Int = 0
  var len = pattern.length


  def getGoodSuffixFun(): List[Int] = {

    val listt: ListBuffer[Int] = ListBuffer.fill(len + 1)(0)
    val patternFunc: List[Int] = getPrefixFun(pattern)
    val patternReverse: String = pattern.reverse
    val patternReverseFunc: List[Int] = getPrefixFun(patternReverse)

    for (i <- 0 to len) {

      listt(i) = len - patternFunc(len)

    }

    for (l <- 1 to len) {

      val x = len - patternReverseFunc(l)
      listt(x) = Math.min(listt(x), l - patternReverseFunc(l))

    }

    listt.toList

  }


  def getBadCharFun(): mutable.HashMap[Char, Int] = {

    val result: mutable.HashMap[Char, Int] = new mutable.HashMap[Char, Int]
    for (i <- 0 until len) {

      result += ((pattern(i), i))

    }

    result
  }

  def findAllIn(text: CharSequence): Iterator[Int] = {

    var oneCharVar = 0
    val funcGoodSuffix = getGoodSuffixFun()
    val funcGetBadChar = getBadCharFun()
    val result: ListBuffer[Int] = new ListBuffer[Int]

    while (oneCharVar <= text.length - pattern.length) {
      var patternLength = pattern.length

      while (patternLength > 0 && pattern(patternLength - 1) == text.charAt(oneCharVar + patternLength - 1)) {
        patternLength = patternLength - 1
        comparisons += 1
      }

      if (patternLength == 0) {
        result += oneCharVar
        oneCharVar = oneCharVar + funcGoodSuffix.head
      }
      else {
        var minusOne = -1
        if(funcGetBadChar.contains(text.charAt(oneCharVar + patternLength - 1))) {
          minusOne = patternLength - 1 - funcGetBadChar.apply(text.charAt(oneCharVar + patternLength - 1))
        }
        oneCharVar = oneCharVar + Math.max(funcGoodSuffix(patternLength), minusOne)
      }

    }
    comparisons += 3
    result.iterator
  }

  def toJson(text: CharSequence): String = {

    val jsonAlgorithm: JsValue = Json.toJson("BM")
    val jsonPattern: JsValue = Json.toJson(pattern)
    val jsonText: JsValue = Json.toJson(text.toString)
    val goodSuffixFunElements = getGoodSuffixFun()
    val goodSuffixFunList = new ListBuffer[List[Int]]
    findAllIn(text)

    for (i <- goodSuffixFunElements.indices) {
      goodSuffixFunList.append(List(i, goodSuffixFunElements(i)))
    }

    val jsonPrefixFun: JsValue = Json.toJson(goodSuffixFunList.toList)
    val jsonComparisons: JsValue = Json.toJson(comparisons)

    val jsonMap: Map[String, JsValue] = Map("algorithm" -> jsonAlgorithm, "pattern" -> jsonPattern, "text" -> jsonText, "prefixFun" -> jsonPrefixFun, "comparisons" -> jsonComparisons)
    val result = Json.stringify(Json.toJson(jsonMap))
    result
  }

  def getPrefixFun(pattern: String): List[Int] = {

    val len: Int = pattern.length
    val patternList: ListBuffer[Int] = ListBuffer.fill(len + 1)(0)
    patternList(0) = -1
    var bord: Int = 0
    for (i <- 2 to len) {
      while (bord > 0 && pattern(i-1) != pattern(bord)) {
        bord = patternList(bord)
      }
      if (pattern(i-1) == pattern(bord)) {
        bord += 1
      }
      patternList(i) = bord
    }
    patternList.toList
  }

}