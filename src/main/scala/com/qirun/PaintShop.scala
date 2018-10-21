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

    // Debug
//    customers.foreach(println(_))
//    paints.foreach(println(_))

    val combination = mixColor(customers, paints, size)

    val result: String = if (combination.isDefined) {
      combination.get.mkString(" ")
    } else
      "No solution exists"

    println(result)
  }

  def mixColor(customers: Array[Customer], paints: Set[Paint], size: Int): Option[List[Paint]] = {
    
    paints.toList.combinations(size)
      .find{
        p: List[Paint] =>
          customers.count{
            c => c.paints.intersect(p.toSet).nonEmpty
          } == customers.length
      }
  }
}