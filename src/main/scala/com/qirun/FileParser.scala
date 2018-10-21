package com.qirun

import scala.io.Source

object FileParser {

  def readText(filePath: String): (Array[User], Set[Paint]) = {

    val iterator = Source.fromFile(filePath).getLines()

    val size = getSize(iterator)

    createUsers(iterator, size)
  }

  def getSize(iterator: Iterator[String]) = {
    if (iterator.isEmpty) throw new DataErrorException("The data file is empty")

    val size = iterator.next()

    if(!size.forall(_.isDigit)) throw new DataErrorException("The first line is not a number")

    size.toInt
  }

  private def createUsers(iterator: Iterator[String], size: Int): (Array[User], Set[Paint]) = {

    var userArray = Array[User]()
    var paintSet = Set[Paint]()

    while(iterator.hasNext) {
      var colorString = iterator.next().split(" ")

      if(colorString.size % 2 != 0) throw new DataErrorException(s"${colorString.toString} is not correctly defined")

      val paintBuilder:((Array[String] => Paint)) = {
        c => Paint(c(0).toInt, if (c(1).equals("G")) Gloss else Matte)}

      val paints = colorString.grouped(2).map(paintBuilder).toList
      paintSet ++= paints
      userArray :+= new User(paints)
    }

    (userArray, paintSet)
  }
}

final case class DataErrorException(msg: String="", cause: Throwable = None.orNull)
  extends Exception(msg, cause)
