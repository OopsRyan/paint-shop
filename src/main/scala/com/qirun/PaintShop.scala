package com.qirun

sealed trait Color
final case object Gloss extends Color {
  override def toString: String = "G"
}
final case object Matte extends Color {
  override def toString: String = "M"
}

case class Paint(id: Int, color: Color) {
  override def toString: String = color.toString
}

class Customer(val paints: Set[Paint]) {
  override def toString: String = paints.toString
}


object PaintShop {
  def main(args: Array[String]): Unit = {

    val ((customers, paints), size) = FileParser.readText("src/test/resources/case1.txt")

    val combination = mixColor(customers, paints, size)

    println(getResult(combination))
  }

  /**
    * Find the combination of mixed paint colors with the least Matte
    * @param customers The customer array
    * @param paints The paint set
    * @param size The number of colors
    * @return A combination of paint colors with the least Matte
    */
  def mixColor(customers: Array[Customer], paints: Set[Paint], size: Int): Option[List[Paint]] = {

    // For each possible combination, iterate the customer arrays to find
    // if all customers have at least one like color in this combination
    // Note: the performance is the most serious issue
    // *** O(n^3), and some extra space needed due to toSet ***
    val satisfiedCom = paints.toList.combinations(size)
      .filter{
        p: List[Paint] =>
          customers.count{
            c => c.paints.intersect(p.toSet).nonEmpty
          } == customers.length
      }

    // For all satisfied combinations, sort them by the number of Matte paints they have
    satisfiedCom.toList
      .sortWith(_.count(_.color == Matte) < _.count(_.color == Matte))
      .headOption
  }

  def getResult(combination: Option[List[Paint]]): String = {
    if (combination.isDefined)
      combination.get.mkString(" ")
    else
      "No solution exists"
  }
}