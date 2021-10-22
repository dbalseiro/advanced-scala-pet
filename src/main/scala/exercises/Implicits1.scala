package exercises

object Implicits1 extends App {
  case class Purchase(units: Int, price: Double):
    def total: Double = units * price

  object Purchase:
    implicit val totalOrdering: Ordering[Purchase] = Ordering.fromLessThan((a, b) => a.total < b.total)

  object ByUnits:
    implicit val unitsOrdering: Ordering[Purchase] = Ordering.fromLessThan((a, b) => a.units < b.units)

  object ByPrice:
    implicit val priceOrdering: Ordering[Purchase] = Ordering.fromLessThan((a, b) => a.price < b.price)

  import ByPrice._
  val l = List(Purchase(1, 2.3), Purchase(4, 5.2), Purchase(2, 1.5))
  println(l.sorted)
}
