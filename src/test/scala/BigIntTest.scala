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
      assert((xi +  yi).getLongValue == (x.toLong + y.toLong), s"failed on $x + $y")
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

  test("Square Test") {
    val r = util.Random
    Range(0, 500).foreach(_ => {
      val x = r.nextInt()
      val xi = IntB(x)
      assert((xi.square).getLongValue == (x.toLong * x.toLong))
    })
  }

  test("Sqrt Test") {
    val r = util.Random
    Range(0, 500).foreach(_ => {
      val x = r.nextInt(100000)
      val xi = IntB(x)
      assert(xi.sqrt.get.getLongValue == math.sqrt(x.toLong).toLong)
    })
  }

  test("Pow Test") {
    val r = util.Random
    Range(0, 100).foreach(_ => {
      val x = r.nextInt(4).toLong
      val xi = IntB(x)
      val y = r.nextInt(20).toLong
      val yi = IntB(y)
      assert(xi.pow(yi).get.getLongValue == Range(0, y.intValue()).foldLeft(1L)((old, _) => old * x))
    })
  }

  test("Division Test") {
    val r = util.Random
    Range(0, 500).foreach(_ => {
      val x = r.nextInt()
      val y = r.nextInt()
      val xi = IntB(x)
      val yi = IntB(y)
      if(y != 0) {
        assert((xi / yi).get.getLongValue == (x.toLong / y.toLong))
      }
    })
  }

  test("Mod Test") {
    val r = util.Random
    Range(0, 500).foreach(_ => {
      val x = r.nextInt()
      val y = r.nextInt()
      val xi = IntB(x)
      val yi = IntB(y)
      if(y != 0) {
        assert((xi % yi).get.getLongValue == (x.toLong % y.toLong))
      }
    })
  }
}