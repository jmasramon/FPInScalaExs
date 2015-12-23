/**
  * Created by jmasramon on 19/12/15.
  */
import org.specs2._

class CafeSpec extends org.specs2.mutable.Specification {
  "this is Cafe specification" >> {
    "where you should be able to create a new Cafe" >> {
      new Cafe() must beAnInstanceOf[Cafe]
    }
    "where a Cafe should allow to buy coffee and return the bought cup" >> {
      val cc = CreditCard()
      val coffee = new Cafe().buyCoffee(cc)
      coffee must beAnInstanceOf[Coffee]
      coffee.price must_==(2)
      cc.credit must_==(coffee.price)
    }
  }
}

class CoffeeSpec extends org.specs2.mutable.Specification {
  "this is Coffee specification" >> {
    "where you should be able to create a new Coffee" >> {
      Coffee(12) must beAnInstanceOf[Coffee]
    }
    "where a new Coffee has an accessible price" >> {
      Coffee(12).price  must be_==(12)
    }
  }
}

class CreditCardSpec extends org.specs2.mutable.Specification {
  "this is CreditCard specification" >> {
    "where you should be able to create a new CreditCard" >> {
      CreditCard() must beAnInstanceOf[CreditCard]
    }
    "where a CreditCard should allow to charge" >> {
      val cc = CreditCard()
      cc.charge(12)
      cc.credit must be_==(12)
      cc.charge(6)
      cc.credit must be_==(18)
    }
  }
}

class ChargeSpec extends org.specs2.mutable.Specification {
  "this is Charge specification" >> {
    "where you should be able to create a new Coffee" >> {
      Charge(CreditCard(), 12) must beAnInstanceOf[Charge]
    }
    "where a new Charge has an accessible price" >> {
      Charge(CreditCard(), 12).amount  must be_==(12)
    }
    "where a new Charge can be combined with another one if same cc" >> {
      val cc = CreditCard()
      Charge(cc, 12).combine(Charge(cc, 13)).amount  must be_==(25)
    }
    "where a new Charge cannot be combined with another one if different cc" >> {
      Charge(CreditCard(), 12).combine(Charge(CreditCard(2), 13)).amount  must throwA[Exception](message = "Can't combine if cc is different")
    }

  }
}

class FunctionalCafeSpec extends org.specs2.mutable.Specification {
  "this is FunctionalCafe specification" >> {
    "where you should be able to create a new FunctionalCafe" >> {
      new FunctionalCafe() must beAnInstanceOf[FunctionalCafe]
    }
    "where a FunctionalCafe should allow to buy coffee and return the charge and the bought cup" >> {
      val (charge, coffee) = new FunctionalCafe().buyCoffee(CreditCard())
      charge must beAnInstanceOf[Charge]
      coffee must beAnInstanceOf[Coffee]
      coffee.price must_==(2)
      charge.amount must_==(coffee.price)
    }
    "where a FunctionalCafe should allow to buy coffees and return the charge and the bought cups" >> {
      val (charge, coffees) = new FunctionalCafe().buyCoffees(3, CreditCard())
      charge must beAnInstanceOf[Charge]
      coffees must beAnInstanceOf[Array[Coffee]]

      coffees.length must_==(3)
      charge.amount must_==(coffees(0).price*coffees.length)
    }
    "the alternate version should work the same way" >> {
      val (charge, coffees) = new FunctionalCafe().buyCoffesAlt(3, CreditCard())
      charge must beAnInstanceOf[Charge]
      coffees must beAnInstanceOf[List[Coffee]]

      coffees.length must_==(3)
      charge.amount must_==(coffees(0).price*coffees.length)
    }
    "shoud coalesce a grup of charges from different ccs" >> {
      val cafe = new FunctionalCafe();
      val (charge1, coffees1) = cafe.buyCoffesAlt(3, CreditCard())
      var (charge2, coffees2) = cafe.buyCoffesAlt(4, CreditCard(2))
      val charges = Charge.coalesce(List(charge1,charge2))
      charges must_==(List(Charge(CreditCard(0),6), Charge(CreditCard(2),8)))
    }
  }
}


