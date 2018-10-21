package com.qirun

import scala.io.Source

object FileParser {

  /**
    * Read data from the text file
    * @param filePath The given data file path
    * @return Tuple2(the iterator for data, the number of colors)
    */
  def readText(filePath: String): ((Array[Customer], Set[Paint]), Int) = {

    val iterator = Source.fromFile(filePath).getLines()

    val size = getSize(iterator)

    (createCustomers(iterator), size)
  }

  /**
    * Validate the data, and return the number of customers
    * @param iterator The iterator for data
    * @return The number of customers
    */
  def getSize(iterator: Iterator[String]) = {
    if (iterator.isEmpty) throw new DataErrorException("The data file is empty")

    val size = iterator.next()

    if(!size.forall(_.isDigit)) throw new DataErrorException("The first line is not a number")

    size.toInt
  }

  /**
    * Parse data line by line, build paints for each customers
    * @param iterator The iterator for data
    * @return Tuple2(customer array, the paint set)
    */
  private def createCustomers(iterator: Iterator[String]): (Array[Customer], Set[Paint]) = {

    var userArray = Array[Customer]()
    var paintSet = Set[Paint]()

    val paintBuilder:(Array[String] => Paint) = {
      c => Paint(c(0).toInt, if (c(1).equals("G")) Gloss else Matte)}

    while(iterator.hasNext) {
      var colorStrings = iterator.next().split(" ")

      if(colorStrings.length % 2 != 0)
        throw new DataErrorException(s"${colorStrings.toString} is not correctly defined")

      val paints = colorStrings.grouped(2).map(paintBuilder).toSet
      paintSet ++= paints
      userArray :+= new Customer(paints)
    }

    (userArray, paintSet)
  }
}

/**
  * Custom exception for data validation
  * @param msg
  * @param cause
  */
final class DataErrorException(msg: String="", cause: Throwable = None.orNull)
  extends Exception(msg, cause)
