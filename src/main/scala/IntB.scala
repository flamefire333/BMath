package main.scala.BigMath

case class IntB(bits : Seq[Boolean], is_negated : Boolean) {
  def setBit(idx : Int, state : Boolean) : IntB = {
    val base : Seq[Boolean] = if(idx < bits.size) bits else bits ++ Seq.fill(idx - bits.size + 1)(is_negated)
    IntB(base.zipWithIndex.map(data => if(data._2 == idx) state else data._1), is_negated)
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
    if(nData._2) {
      /*
      Carry cases
      0 0 ..01x
      0 1 ..0x
      1 0 ..0x
      1 1 ..1x
      */
      if (!is_negated && !o.is_negated) {
        IntB(nData._1.bits :+ true, false)
      } else {
        IntB(nData._1.bits, is_negated & o.is_negated)
      }
    } else {
      IntB(nData._1.bits, is_negated ^ o.is_negated)
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

  def getLongValue : Long = {
    Range(0, 63).foldLeft(0L)((old, i) => old ^ ((if(getBit(i)) 1L else 0L) << i)) ^ ((if(is_negated) 1L else 0L) << 63)
  }

  def unary_- : IntB = {
    ~this + IntB.ONE
  }

  override def toString: String = {
    (if(is_negated) "-" else "") + "0b" + bits.foldLeft("")((old, bit) => (if(bit) "1" else "0") + old)
  }
}

object IntB {
  def apply(value : Long) : IntB = {
    IntB(Range(0, 64).map(i => ((value >> i) & 1) == 1), value < 0)
  }
  val ZERO = IntB(Seq(), false)
  val ONE = IntB(Seq(true), false)
}
