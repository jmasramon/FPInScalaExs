/**
  * Created by jmasramon on 19/12/15.
  */

import org.specs2._

class QuickStartSpec extends Specification { def is = s2"""

 This is my first specification
   it is working                 $ok
   really working!               $ok
                                 """
}

class MySpecification extends org.specs2.mutable.Specification {
  "this is my specification" >> {
    "where example 1 must be true" >> {
      1 must_== 1
    }
    "where example 2 must be true" >> {
      2 must_== 2
    }
  }
}