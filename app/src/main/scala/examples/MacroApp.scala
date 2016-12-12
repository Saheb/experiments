package examples

@main
object MacroApp {

  def funkyAdded(i: Int): Int = {
    10 + i
  }

  sealed trait Human {
    val v: String
  }

  @addFinal
  case class Person(v: String) extends Human

  sealed trait CType {
    val v: Any
  }

  @cType
  case class IntTuple(v: List[(Int,Int)]) extends CType
}
