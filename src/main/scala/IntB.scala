package main.scala.BigMath

case class IntB(bits : Seq[Boolean], is_negated : Boolean) {
  def setBit(idx : Int, state : Boolean) : IntB = {
    val base : Seq[Boolean] = if(idx < bits.size) bits else bits ++ Seq.fill(idx - bits.size + 1)(is_negated)
    IntB(base.zipWithIndex.map(data => if(data._2 == idx) state else data._1), is_negated)
  }

  def compact : IntB = {
    val nbits = bits.reverse.dropWhile(b => b == is_negated).reverse
    IntB(nbits, is_negated)
    //this
  }

  def getBit(idx : Int) : Boolean = {
    if(idx < bits.size) bits(idx) else is_negated
  }

  def unary_~ : IntB = {
    IntB(bits.map(b => !b), !is_negated)
  }

  def <<(amt : Int) : IntB = {
    IntB(Seq.fill(amt)(false) ++ bits, is_negated)
  }

  def >>(amt : Int) : IntB = {
    IntB(bits.takeRight(bits.size - amt), is_negated)
  }

  def ^(o : IntB) : IntB = {
    IntB(Range(0, math.max(bits.size, o.bits.size)).map(i => getBit(i) ^ o.getBit(i)), is_negated ^ o.is_negated)
  }

  def &(o : IntB) : IntB = {
    IntB(Range(0, math.max(bits.size, o.bits.size)).map(i => getBit(i) & o.getBit(i)), is_negated & o.is_negated)
  }

  def |(o : IntB) : IntB = {
    IntB(Range(0, math.max(bits.size, o.bits.size)).map(i => getBit(i) | o.getBit(i)), is_negated | o.is_negated)
  }

  def abs : IntB = {
    if(this.is_negated) -this else this
  }

  def gcd(o : IntB) : IntB = {
    val a = this.abs
    val b = o.abs
    if(b < a) {
      b.gcd(a)
    }
    else if(a == IntB.ZERO) {
      b
    } else {
      (b % this).get.gcd(a)
    }
  }

  def >(o : IntB) : Boolean = {
    if(this.is_negated) {
      if(o.is_negated) {
        (-o) > (-this)
      } else {
        false
      }
    } else {
      if(o.is_negated) {
        true
      } else {
        Range(0, math.max(bits.size, o.bits.size)).foldLeft(false)((old, index) => {
          val a = getBit(index)
          val b = o.getBit(index)
          if(a == b) old else a
        })
      }
    }
  }

  def <(o : IntB) : Boolean = {
    o > this
  }

  def ==(o : IntB) : Boolean = {
    !(this > o) && !(o > this)
  }

  def !=(o : IntB) : Boolean = {
    !(this == o)
  }

  def >=(o : IntB) : Boolean = {
    (this > o) || (this == o)
  }

  def <=(o : IntB) : Boolean = {
    (this < o) || (this == o)
  }

  def +(o : IntB) : IntB = {
    val nData = Range(0, math.max(bits.size, o.bits.size)).foldLeft((IntB.ZERO, false))((data, index) => {
      val carryIn = data._2
      val o1 = getBit(index)
      val o2 = o.getBit(index)
      val toPut = carryIn ^ o1 ^ o2
      val carryOut = (o1 & o2) | (o1 & carryIn) | (o2 & carryIn)
      (data._1.setBit(index, toPut), carryOut)
    })
    val outBits = nData._1.bits
    if(nData._2) {
      /*
      Carry cases
      0 0 ..01x
      0 1 ..0x
      1 0 ..0x
      1 1 ..1x
      */
      if (!is_negated && !o.is_negated) {
        IntB(outBits :+ true, false).compact
      } else {
        IntB(outBits, is_negated & o.is_negated).compact
      }
    } else {
      if(is_negated && o.is_negated) {
        IntB(outBits :+ false, true).compact
      } else {
        IntB(outBits, is_negated ^ o.is_negated).compact
      }
    }
  }

  def -(o : IntB) : IntB = {
   this + -o
  }

  def *(o : IntB) : IntB = {
    if(bits.size < o.bits.size) {
      o * this
    } else {
      if (o.is_negated) {
        -(this * -o)
      } else {
        o.bits.indices.foldLeft(IntB.ZERO)((old, index) => {
          if (o.getBit(index)) {
            old + (this << index)
          } else {
            old
          }
        })
      }
    }
  }

  def divmod(o : IntB) : Option[(IntB, IntB)] = {
    if(o == IntB.ZERO) {
      None
    } else {
      (is_negated, o.is_negated) match {
        case (true, true) => (-this).divmod(-o).map(data => (data._1, -data._2))
        case (true, false) => (-this).divmod(o).map(data => (-data._1, -data._2))
        case (false, true) => this.divmod(-o).map(data => (-data._1, data._2))
        case (false, false) => {
          Some(Range(0, this.bits.size).foldLeft((IntB.ZERO, this))((curr, index) => {
            val shiftAmount = this.bits.size - index - 1
            val currAmt = curr._2
            val currOut = curr._1
            val comp = currAmt >> shiftAmount
            if(comp >= o) {
              val t = (IntB.ONE << (shiftAmount))
              (currOut + t, currAmt - (o << shiftAmount))
            } else {
              (currOut, currAmt)
            }
          }))
        }
      }
    }
  }

  def /(o : IntB) : Option[IntB] = this.divmod(o).map(a => a._1)

  def %(o : IntB) : Option[IntB] = this.divmod(o).map(a => a._2)

  def pow(exp : IntB) : Option[IntB] = {
    if(exp.is_negated) {
      None
    } else {
      val (ans, _) = exp.bits.foldLeft((IntB.ONE, None : Option[IntB]))((curr, isSet) => {
        val ansSoFar = curr._1
        val o : Option[IntB] = curr._2
        val sqPow = o.fold(this)(s => s.square)
        (if(isSet) ansSoFar * sqPow else ansSoFar, Some(sqPow))
      })
      Some(ans)
    }
  }

  def square : IntB = {
    (this * this).compact
  }

  def sqrt : Option[IntB] = {
    if(this.is_negated) {
      None
    } else {
      Some(Range(bits.size / 2 + 2, -1, -1).foldLeft(IntB.ZERO)((old, i) => {
        val toTest = old + (IntB.ONE << i).compact
        if (toTest.square > this) old else toTest
      }))
    }
  }

  def getLongValue : Long = {
    Range(0, 63).foldLeft(0L)((old, i) => old ^ ((if(getBit(i)) 1L else 0L) << i)) ^ ((if(is_negated) 1L else 0L) << 63)
  }

  def unary_- : IntB = {
    ~this + IntB.ONE
  }

  override def toString: String = {
    (if(is_negated) "-" else "") + "0b" + bits.foldLeft("")((old, bit) => (if(bit) "1" else "0") + old)
  }

  def toLongBinaryString : String = {
    Range(0, 64).foldLeft("")((old, bit) => (if(getBit(bit)) "1" else "0") + old)
  }
}

object IntB {
  def apply(value : Long) : IntB = {
    IntB(Range(0, 64).map(i => ((value >> i) & 1) == 1), value < 0).compact
  }
  val ZERO = IntB(Seq(), false)
  val ONE = IntB(Seq(true), false)
}
