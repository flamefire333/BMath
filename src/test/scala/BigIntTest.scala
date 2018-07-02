import org.scalatest.FunSuite
import main.scala.BigMath.IntB
class BigIntTest extends FunSuite{
  test("Set Bit/Get Bit") {
    val b = IntB(0)
    assert(!b.getBit(10))
    assert(b.setBit(10, true).getBit(10))
    val r = util.Random
    Range(0, 500).foldLeft(b)((old, _) => {
      val setTo = r.nextBoolean()
      val indexToSet = r.nextInt(10000)
      val n = old.setBit(indexToSet, setTo)
      assert(n.getBit(indexToSet) == setTo)
      n
    })
  }

  test("Get Long Value") {
    val r = util.Random
    Range(0, 500).foreach(_ => {
      val x = r.nextLong()
      assert(IntB(x).getLongValue == x)
    })
  }

  test("Addition Test") {
    val r = util.Random
    Range(0, 500).foreach(_ => {
      val x = r.nextInt()
      val y = r.nextInt()
      val xi = IntB(x)
      val yi = IntB(y)
      assert((xi +  yi).getLongValue == (x.toLong + y.toLong))
    })
  }

  test("Negation Test") {
    val r = util.Random
    Range(0, 500).foreach(_ => {
      val x = r.nextInt()
      val xi = IntB(x)
      assert((-xi).getLongValue == -(x.toLong))
    })
  }

  test("Subtraction Test") {
    val r = util.Random
    Range(0, 500).foreach(_ => {
      val x = r.nextInt()
      val y = r.nextInt()
      val xi = IntB(x)
      val yi = IntB(y)
      assert((xi -  yi).getLongValue == (x.toLong - y.toLong))
    })
  }

  test("Multiplication Test") {
    val r = util.Random
    Range(0, 500).foreach(_ => {
      val x = r.nextInt()
      val y = r.nextInt()
      val xi = IntB(x)
      val yi = IntB(y)
      assert((xi *  yi).getLongValue == (x.toLong * y.toLong))
    })
  }

  test("Greater Than Test") {
    val r = util.Random
    Range(0, 500).foreach(_ => {
      val x = r.nextInt()
      val y = r.nextInt()
      val xi = IntB(x)
      val yi = IntB(y)
      assert((xi >  yi) == (x.toLong > y.toLong))
    })
  }

}