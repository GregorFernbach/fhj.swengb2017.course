package at.fhj.swengb.apps.helloworld

import org.scalatest._

class HelloSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello jakob" in {
    Hello.greeting shouldEqual "hello jakob"
  }
}


