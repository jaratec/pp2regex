package pp2regex

import java.io._
import scala.io.Source

object Translator {

  val parser = new Parser()

  def parse(fileName: String): List[(String,Option[String])] =  {
    var lst: List[(String,Option[String])] = List()
    try {
      val pictures = Source.fromFile(fileName, "UTF-8").getLines.toList
      println("expressions read:" + pictures.size)
      lst = pictures.map{ str =>
        println(str); (str, parser.parse(str))
      }
    } catch {
      case e: Exception => e.printStackTrace()
    }
    lst
  }

  def write(f: File)(op: PrintWriter => Unit): Unit = {
    val pw = new PrintWriter(f)
    try {
      op(pw)
    } finally {
      pw.close()
    }
  }

  def saveCSV(fileName: String, tuples: List[(String,Option[String])]): Unit = {
    val file = new File(fileName)
    write(file)(pw => tuples.foreach(t => pw.println(t._1 + ";" + t._2.getOrElse(""))))
  }

  def saveMap(fileName: String, tuples: List[(String,Option[String])]): Unit = {
    val file = new File(fileName)
    write(file)(pw => tuples.filter{t => t._2.isDefined}.foreach{t => pw.println("\"" + t._1 + "\" -> \"\"\"" + t._2.get + "\"\"\",")})
  }

  def saveSQL(fileName: String, tuples: List[(String,Option[String])]): Unit = {
    val file = new File(fileName)
    write(file)(pw => tuples.foreach{t =>
      t match {
        case (pic,Some(r)) => pw.println("update field_constraint set value = '" + r + "' where value = '" + pic + "' and operator = '*';")
        case (pic,None) => pw.println("delete field_constraint where value = '" + pic + "' and operator = '*';")
      }
    })
  }

  def translate(input: String): Unit = {
    val translations = parse(input)
    saveCSV("translations.csv", translations)
    saveMap("Map.txt", translations)
    saveSQL("db_update.sql", translations)
  }

}
