/**
  * Created by jmasramon on 19/12/15.
  */
class Cafe {
  def buyCoffee(cc: CreditCard) : Coffee = {
    val cup = Coffee(2)

    cc.charge(cup.price)
    cup
  }
}

case class Coffee(price: Int)

case class CreditCard(var credit: Int = 0){
  def charge(price: Int): Unit = {
    credit = credit + price
  }
}

class FunctionalCafe {
  def buyCoffee(cc: CreditCard) : (Charge, Coffee) = {
    val cup = Coffee(2)
    val charge = Charge(cc, cup.price)
    (charge, cup)
  }

  def buyCoffees(quantity:Int, cc: CreditCard): (Charge, Array[Coffee]) = {
    var coffees:Array[Coffee] = new Array[Coffee](quantity)
    val cc = CreditCard()
    var charge = Charge(cc, 0)
    for (i <- 0 to quantity-1) {
      val (newCharge, newCoffee) = buyCoffee(cc)
      coffees(i) = newCoffee
      charge = charge.combine(newCharge)
    }
    (charge, coffees)
  }

  def buyCoffesAlt(quantity:Int, cc: CreditCard): (Charge, List[Coffee]) = {
    val purchases: List[(Charge, Coffee)] = List.fill(quantity)(buyCoffee(cc))
    val (charges, coffees) = purchases.unzip
    (charges.reduce((c1,c2) => c1.combine(c2)), coffees)
  }
}

case class Charge(cc: CreditCard, amount: Int){
  def combine(anotherCharge: Charge): Charge = {
    if(anotherCharge.cc == cc) {
      Charge(cc, amount + anotherCharge.amount)
    } else {
      throw new Exception("Can't combine if cc is different")
    }
  }

}

object Charge {
  def coalesce(charges: List[Charge]): List[Charge] = {
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
  }
}