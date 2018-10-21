package com.qirun

sealed trait Color
final case object Gloss extends Color {
  override def toString: String = "G"
}
final case object Matte extends Color {
  override def toString: String = "M"
}

case class Paint(id: Int, color: Color)

class Customer(paints: List[Paint]) {
  override def toString: String = paints.toString()
}


object PaintShop {
  def main(args: Array[String]): Unit = {

    val (users, paints) = FileParser.readText("src/test/resources/case4.txt")

    users.foreach(println(_))

    paints.foreach(println(_))
  }
}