package exercises

import scala.io.Source

object ChatBot extends App:
  Source.stdin.getLines().map(respond).foreach(println)

  def respond: PartialFunction[String, String] = {
    case "Hola" => "Hola master"
  }