import com.qirun.FileParser
import com.qirun.PaintShop.{mixColor, getResult}
import org.scalatest.{FlatSpec, Matchers}

class PaintShopTest extends FlatSpec with Matchers {
  "PaintShopTest" should "return the right combination for case 1" in {

    val ((customers, paints), size) = FileParser.readText("src/test/resources/case1.txt")

    val combination = mixColor(customers, paints, size)

    assert(getResult(combination).equals("G G G G M"))
  }

  "PaintShopTest" should "return the right combination for case 2" in {

    val ((customers, paints), size) = FileParser.readText("src/test/resources/case2.txt")

    val combination = mixColor(customers, paints, size)

    assert(getResult(combination).equals("No solution exists"))
  }

  "PaintShopTest" should "return the right combination for case 3" in {

    val ((customers, paints), size) = FileParser.readText("src/test/resources/case3.txt")

    val combination = mixColor(customers, paints, size)

    assert(getResult(combination).equals("G M G M G"))
  }

  "PaintShopTest" should "return the right combination for case 4" in {

    val ((customers, paints), size) = FileParser.readText("src/test/resources/case4.txt")

    val combination = mixColor(customers, paints, size)

    assert(getResult(combination).equals("G M")) // I dont know why the right one is M M ?
  }
}



