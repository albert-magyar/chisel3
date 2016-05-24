// See LICENSE for license details.

package Chisel

import internal._
import internal.Builder.pushOp
import internal.firrtl._
import firrtl.PrimOp._

/** Element is a leaf data type: it cannot contain other Data objects. Example
  * uses are for representing primitive data types, like integers and bits.
  */
abstract class Element(private[Chisel] val width: Width) extends Data {
  /**
   * Elements can actually be bound to the hardware graph and thus must store
   * that binding information.
   */
  private[this] var _binding: Binding = UnboundBinding(None)
  // Define setter/getter pairing
  // Can only bind something that has not yet been bound.
  private[Chisel] def binding_=(target: Binding): Unit = _binding match {
    case UnboundBinding(_) => {
      _binding = target
      _binding
    }
    case _ => throw internal.Binding.AlreadyBoundException(_binding.toString)
      // Other checks should have caught this.
  }
  private[Chisel] def binding = _binding

  private[Chisel] final def allElements: Seq[Element] = Seq(this)
}

/** A data type for values represented by a single bitvector. Provides basic
  * bitwise operations.
  */
sealed abstract class Bits(width: Width, override val litArg: Option[LitArg])
    extends Element(width) {
  // TODO: perhaps make this concrete?
  // Arguments for: self-checking code (can't do arithmetic on bits)
  // Arguments against: generates down to a FIRRTL UInt anyways

  private[Chisel] def fromInt(x: BigInt, w: Int): this.type

  private[Chisel] def flatten: IndexedSeq[Bits] = IndexedSeq(this)

  def cloneType: this.type = cloneTypeWidth(width)

  def tail(n: Int): UInt = {
    val w = width match {
      case KnownWidth(x) =>
        require(x >= n, s"Can't tail($n) for width $x < $n")
        Width(x - n)
      case UnknownWidth() => Width()
    }
    binop(UInt(width = w), TailOp, n)
  }

  def head(n: Int): UInt = {
    width match {
      case KnownWidth(x) => require(x >= n, s"Can't head($n) for width $x < $n")
      case UnknownWidth() =>
    }
    binop(UInt(width = n), HeadOp, n)
  }

  /** Returns the specified bit on this wire as a [[Bool]], statically
    * addressed.
    */
  final def apply(x: BigInt): Bool = {
    if (x < 0) {
      Builder.error(s"Negative bit indices are illegal (got $x)")
    }
    if (isLit()) {
      (((litValue() >> x.toInt) & 1) == 1).asBool
    } else {
      Binding.checkSynthesizable(this, s"'this' ($this)")
      pushOp(DefPrim(Bool(), BitsExtractOp, this.ref, ILit(x), ILit(x)))
    }
  }

  /** Returns the specified bit on this wire as a [[Bool]], statically
    * addressed.
    *
    * @note convenience method allowing direct use of Ints without implicits
    */
  final def apply(x: Int): Bool =
    apply(BigInt(x))

  /** Returns the specified bit on this wire as a [[Bool]], dynamically
    * addressed.
    */
  final def apply(x: UInt): Bool =
    (this >> x)(0)

  /** Returns a subset of bits on this wire from `hi` to `lo` (inclusive),
    * statically addressed.
    *
    * @example
    * {{{
    * myBits = 0x5 = 0b101
    * myBits(1,0) => 0b01  // extracts the two least significant bits
    * }}}
    */
  final def apply(x: Int, y: Int): UInt = {
    if (x < y || y < 0) {
      Builder.error(s"Invalid bit range ($x,$y)")
    }
    val w = x - y + 1
    if (isLit()) {
      ((litValue >> y) & ((BigInt(1) << w) - 1)).asUInt(w)
    } else {
      Binding.checkSynthesizable(this, s"'this' ($this)")
      pushOp(DefPrim(UInt(width = w), BitsExtractOp, this.ref, ILit(x), ILit(y)))
    }
  }

  // REVIEW TODO: again, is this necessary? Or just have this and use implicits?
  final def apply(x: BigInt, y: BigInt): UInt = apply(x.toInt, y.toInt)

  private[Chisel] def unop[T <: Data](dest: T, op: PrimOp): T = {
    Binding.checkSynthesizable(this, s"'this' ($this)")
    pushOp(DefPrim(dest, op, this.ref))
  }
  private[Chisel] def binop[T <: Data](dest: T, op: PrimOp, other: BigInt): T = {
    Binding.checkSynthesizable(this, s"'this' ($this)")
    pushOp(DefPrim(dest, op, this.ref, ILit(other)))
  }
  private[Chisel] def binop[T <: Data](dest: T, op: PrimOp, other: Bits): T = {
    Binding.checkSynthesizable(this, s"'this' ($this)")
    Binding.checkSynthesizable(other, s"'other' ($other)")
    pushOp(DefPrim(dest, op, this.ref, other.ref))
  }
  private[Chisel] def compop(op: PrimOp, other: Bits): Bool = {
    Binding.checkSynthesizable(this, s"'this' ($this)")
    Binding.checkSynthesizable(other, s"'other' ($other)")
    pushOp(DefPrim(Bool(), op, this.ref, other.ref))
  }
  private[Chisel] def redop(op: PrimOp): Bool = {
    Binding.checkSynthesizable(this, s"'this' ($this)")
    pushOp(DefPrim(Bool(), op, this.ref))
  }

  /** Returns this wire zero padded up to the specified width.
    *
    * @note for SInts only, this does sign extension
    */
  def pad (other: Int): this.type = binop(cloneTypeWidth(this.width max Width(other)), PadOp, other)

  /** Shift left operation */
  // REVIEW TODO: redundant
  // REVIEW TODO: should these return this.type or Bits?
  def << (other: BigInt): Bits

  /** Returns this wire statically left shifted by the specified amount,
    * inserting zeros into the least significant bits.
    *
    * The width of the output is `other` larger than the input.
    */
  def << (other: Int): Bits

  /** Returns this wire dynamically left shifted by the specified amount,
    * inserting zeros into the least significant bits.
    *
    * The width of the output is `pow(2, width(other))` larger than the input.
    */
  def << (other: UInt): Bits

  /** Shift right operation */
  // REVIEW TODO: redundant
  def >> (other: BigInt): Bits

  /** Returns this wire statically right shifted by the specified amount,
    * inserting zeros into the most significant bits.
    *
    * The width of the output is the same as the input.
    */
  def >> (other: Int): Bits

  /** Returns this wire dynamically right shifted by the specified amount,
    * inserting zeros into the most significant bits.
    *
    * The width of the output is the same as the input.
    */
  def >> (other: UInt): Bits

  /** Returns the contents of this wire as a [[Vec]] of [[Bool]]s.
    */
  def toBools: Seq[Bool] = Seq.tabulate(this.getWidth)(i => this(i))

  /** Reinterpret cast to a SInt.
    *
    * @note value not guaranteed to be preserved: for example, an UInt of width
    * 3 and value 7 (0b111) would become a SInt with value -1
    */
  def asSInt(): SInt

  /** Reinterpret cast to an UInt.
    *
    * @note value not guaranteed to be preserved: for example, a SInt of width
    * 3 and value -1 (0b111) would become an UInt with value 7
    */
  def asUInt(): UInt

  /** Reinterpret cast to Bits. */
  def asBits(): Bits = asUInt

  @deprecated("Use asSInt, which makes the reinterpret cast more explicit", "chisel3")
  final def toSInt(): SInt = asSInt
  @deprecated("Use asUInt, which makes the reinterpret cast more explicit", "chisel3")
  final def toUInt(): UInt = asUInt

  def toBool(): Bool = width match {
    case KnownWidth(1) => this(0)
    case _ => throwException(s"can't covert UInt<$width> to Bool")
  }

  /** Returns this wire concatenated with `other`, where this wire forms the
    * most significant part and `other` forms the least significant part.
    *
    * The width of the output is sum of the inputs.
    */
  def ## (other: Bits): UInt = {
    val w = this.width + other.width
    binop(UInt(w), ConcatOp, other)
  }

  @deprecated("Use asBits, which makes the reinterpret cast more explicit and actually returns Bits", "chisel3")
  override def toBits: UInt = asUInt

  override def fromBits(n: Bits): this.type = {
    val res = Wire(this).asInstanceOf[this.type]
    res := n
    res
  }
}

/** Provides a set of operations to create UInt types and literals.
  * Identical in functionality to the UInt companion object.
  */
object Bits extends UIntFactory

// REVIEW TODO: Further discussion needed on what Num actually is.
/** Abstract trait defining operations available on numeric-like wire data
  * types.
  */
abstract trait Num[T <: Data] {
  // def << (b: T): T
  // def >> (b: T): T
  //def unary_-(): T

  // REVIEW TODO: double check ops conventions against FIRRTL

  /** Outputs the sum of `this` and `b`. The resulting width is the max of the
    * operands plus 1 (should not overflow).
    */
  def +  (b: T): T

  /** Outputs the product of `this` and `b`. The resulting width is the sum of
    * the operands.
    *
    * @note can generate a single-cycle multiplier, which can result in
    * significant cycle time and area costs
    */
  def *  (b: T): T

  /** Outputs the quotient of `this` and `b`.
    *
    * TODO: full rules
    */
  def /  (b: T): T

  def %  (b: T): T

  /** Outputs the difference of `this` and `b`. The resulting width is the max
   *  of the operands plus 1 (should not overflow).
    */
  def -  (b: T): T

  /** Outputs true if `this` < `b`.
    */
  def <  (b: T): Bool

  /** Outputs true if `this` <= `b`.
    */
  def <= (b: T): Bool

  /** Outputs true if `this` > `b`.
    */
  def >  (b: T): Bool

  /** Outputs true if `this` >= `b`.
    */
  def >= (b: T): Bool

  /** Outputs the minimum of `this` and `b`. The resulting width is the max of
    * the operands.
    */
  def min(b: T): T = Mux(this < b, this.asInstanceOf[T], b)

  /** Outputs the maximum of `this` and `b`. The resulting width is the max of
    * the operands.
    */
  def max(b: T): T = Mux(this < b, b, this.asInstanceOf[T])
}

/** A data type for unsigned integers, represented as a binary bitvector.
  * Defines arithmetic operations between other integer types.
  */
sealed class UInt private[Chisel] (width: Width, lit: Option[ULit] = None)
    extends Bits(width, lit) with Num[UInt] {

  private[Chisel] override def cloneTypeWidth(w: Width): this.type =
    new UInt(w).asInstanceOf[this.type]
  private[Chisel] def toType = s"UInt$width"

  override private[Chisel] def fromInt(value: BigInt, width: Int): this.type =
    value.asUInt(width).asInstanceOf[this.type]

  // TODO: refactor to share documentation with Num or add independent scaladoc
  def unary_- : UInt = 0.asUInt - this
  def unary_-% : UInt = 0.asUInt -% this
  def +& (other: UInt): UInt = binop(UInt((this.width max other.width) + 1), AddOp, other)
  def + (other: UInt): UInt = this +% other
  def +% (other: UInt): UInt = (this +& other) tail 1
  def -& (other: UInt): UInt = binop(UInt((this.width max other.width) + 1), SubOp, other)
  def - (other: UInt): UInt = this -% other
  def -% (other: UInt): UInt = (this -& other) tail 1
  def * (other: UInt): UInt = binop(UInt(this.width + other.width), TimesOp, other)
  def * (other: SInt): SInt = other * this
  def / (other: UInt): UInt = binop(UInt(this.width), DivideOp, other)
  def % (other: UInt): UInt = binop(UInt(this.width), RemOp, other)

  def & (other: UInt): UInt = binop(UInt(this.width max other.width), BitAndOp, other)
  def | (other: UInt): UInt = binop(UInt(this.width max other.width), BitOrOp, other)
  def ^ (other: UInt): UInt = binop(UInt(this.width max other.width), BitXorOp, other)

  /** Returns this wire bitwise-inverted. */
  def unary_~ : UInt = unop(UInt(width = width), BitNotOp)

  // REVIEW TODO: Can this be defined on Bits?
  def orR: Bool = this != 0.asUInt
  def andR: Bool = ~this === 0.asUInt
  def xorR: Bool = redop(XorReduceOp)

  def < (other: UInt): Bool = compop(LessOp, other)
  def > (other: UInt): Bool = compop(GreaterOp, other)
  def <= (other: UInt): Bool = compop(LessEqOp, other)
  def >= (other: UInt): Bool = compop(GreaterEqOp, other)
  def != (other: UInt): Bool = compop(NotEqualOp, other)
  def =/= (other: UInt): Bool = compop(NotEqualOp, other)
  def === (other: UInt): Bool = compop(EqualOp, other)
  def unary_! : Bool = this === 0.asUInt

  // REVIEW TODO: Can these also not be defined on Bits?
  def << (other: Int): UInt = binop(UInt(this.width + other), ShiftLeftOp, other)
  def << (other: BigInt): UInt = this << other.toInt
  def << (other: UInt): UInt = binop(UInt(this.width.dynamicShiftLeft(other.width)), DynamicShiftLeftOp, other)
  def >> (other: Int): UInt = binop(UInt(this.width.shiftRight(other)), ShiftRightOp, other)
  def >> (other: BigInt): UInt = this >> other.toInt
  def >> (other: UInt): UInt = binop(UInt(this.width), DynamicShiftRightOp, other)

  def bitSet(off: UInt, dat: Bool): UInt = {
    val bit = 1.asUInt(1) << off
    Mux(dat, this | bit, ~(~this | bit))
  }

  def === (that: BitPat): Bool = that === this
  def != (that: BitPat): Bool = that != this
  def =/= (that: BitPat): Bool = that =/= this

  /** Returns this UInt as a [[SInt]] with an additional zero in the MSB.
    */
  // TODO: this eventually will be renamed as toSInt, once the existing toSInt
  // completes its deprecation phase.
  def zext(): SInt = unop(SInt(width + 1), ConvertOp)

  /** Returns this UInt as a [[SInt]], without changing width or bit value. The
    * SInt is not guaranteed to have the same value (for example, if the MSB is
    * high, it will be interpreted as a negative value).
    */
  def asSInt(): SInt = unop(SInt(width), AsSIntOp)

  def asUInt(): UInt = this
}

// This is currently a factory because both Bits and UInt inherit it.
private[Chisel] sealed trait UIntFactory {
  /** Create a UInt type with inferred width. */
  def apply(): UInt = apply(Width())
  /** Create a UInt type or port with fixed width. */
  def apply(width: Int): UInt = apply(Width(width))
  /** Create a UInt port with specified width. */
  def apply(width: Width): UInt = new UInt(width)

  /** Create a UInt literal with inferred width. */
  def Lit(value: BigInt): UInt = Lit(value, Width())
  /** Create a UInt literal with fixed width. */
  def Lit(value: BigInt, width: Int): UInt = Lit(value, Width(width))
  /** Create a UInt literal with inferred width. */
  def Lit(n: String): UInt = Lit(parse(n), parsedWidth(n))
  /** Create a UInt literal with fixed width. */
  def Lit(n: String, width: Int): UInt = Lit(parse(n), width)
  /** Create a UInt literal with specified width. */
  def Lit(value: BigInt, width: Width): UInt = {
    val lit = ULit(value, width)
    val result = new UInt(lit.width, Some(lit))
    // Bind result to being an Literal
    result.binding = LitBinding()
    result
  }

  private def parse(n: String) = {
    val (base, num) = n.splitAt(1)
    val radix = base match {
      case "x" | "h" => 16
      case "d" => 10
      case "o" => 8
      case "b" => 2
      case _ => Builder.error(s"Invalid base $base"); 2
    }
    BigInt(num.filterNot(_ == '_'), radix)
  }

  private def parsedWidth(n: String) =
    if (n(0) == 'b') {
      Width(n.length-1)
    } else if (n(0) == 'h') {
      Width((n.length-1) * 4)
    } else {
      Width()
    }
}

object UInt extends UIntFactory

sealed class SInt private (width: Width, lit: Option[SLit] = None)
    extends Bits(width, lit) with Num[SInt] {

  private[Chisel] override def cloneTypeWidth(w: Width): this.type =
    new SInt(w).asInstanceOf[this.type]
  private[Chisel] def toType = s"SInt$width"

  override private[Chisel] def fromInt(value: BigInt, width: Int): this.type =
    value.asUInt(width).asInstanceOf[this.type]

  def unary_- : SInt = 0.asSInt - this
  def unary_-% : SInt = 0.asSInt -% this
  /** add (width +1) operator */
  def +& (other: SInt): SInt = binop(SInt((this.width max other.width) + 1), AddOp, other)
  /** add (default - no growth) operator */
  def + (other: SInt): SInt = this +% other
  /** add (no growth) operator */
  def +% (other: SInt): SInt = (this +& other).tail(1).asSInt
  /** subtract (width +1) operator */
  def -& (other: SInt): SInt = binop(SInt((this.width max other.width) + 1), SubOp, other)
  /** subtract (default - no growth) operator */
  def - (other: SInt): SInt = this -% other
  /** subtract (no growth) operator */
  def -% (other: SInt): SInt = (this -& other).tail(1).asSInt
  def * (other: SInt): SInt = binop(SInt(this.width + other.width), TimesOp, other)
  def * (other: UInt): SInt = binop(SInt(this.width + other.width), TimesOp, other)
  def / (other: SInt): SInt = binop(SInt(this.width), DivideOp, other)
  def % (other: SInt): SInt = binop(SInt(this.width), RemOp, other)

  def & (other: SInt): SInt = binop(UInt(this.width max other.width), BitAndOp, other).asSInt
  def | (other: SInt): SInt = binop(UInt(this.width max other.width), BitOrOp, other).asSInt
  def ^ (other: SInt): SInt = binop(UInt(this.width max other.width), BitXorOp, other).asSInt

  /** Returns this wire bitwise-inverted. */
  def unary_~ : SInt = unop(UInt(width = width), BitNotOp).asSInt

  def < (other: SInt): Bool = compop(LessOp, other)
  def > (other: SInt): Bool = compop(GreaterOp, other)
  def <= (other: SInt): Bool = compop(LessEqOp, other)
  def >= (other: SInt): Bool = compop(GreaterEqOp, other)
  def != (other: SInt): Bool = compop(NotEqualOp, other)
  def =/= (other: SInt): Bool = compop(NotEqualOp, other)
  def === (other: SInt): Bool = compop(EqualOp, other)
  def abs(): UInt = Mux(this < 0.asSInt, (-this).asUInt, this.asUInt)

  def << (other: Int): SInt = binop(SInt(this.width + other), ShiftLeftOp, other)
  def << (other: BigInt): SInt = this << other.toInt
  def << (other: UInt): SInt = binop(SInt(this.width.dynamicShiftLeft(other.width)), DynamicShiftLeftOp, other)
  def >> (other: Int): SInt = binop(SInt(this.width.shiftRight(other)), ShiftRightOp, other)
  def >> (other: BigInt): SInt = this >> other.toInt
  def >> (other: UInt): SInt = binop(SInt(this.width), DynamicShiftRightOp, other)

  def asUInt(): UInt = unop(UInt(this.width), AsUIntOp)
  def asSInt(): SInt = this
}

object SInt {
  /** Create an SInt type with inferred width. */
  def apply(): SInt = apply(Width())
  /** Create a SInt type or port with fixed width. */
  def apply(width: Int): SInt = apply(Width(width))
  /** Create an SInt type with specified width. */
  def apply(width: Width): SInt = new SInt(width)

  /** Create an SInt literal with inferred width. */
  def Lit(value: BigInt): SInt = Lit(value, Width())
  /** Create an SInt literal with fixed width. */
  def Lit(value: BigInt, width: Int): SInt = Lit(value, Width(width))
  /** Create an SInt literal with specified width. */
  def Lit(value: BigInt, width: Width): SInt = {
    val lit = SLit(value, width)
    val result = new SInt(lit.width, Some(lit))
    // Bind result to being an Literal
    result.binding = LitBinding()
    result
  }
}

// REVIEW TODO: Why does this extend UInt and not Bits? Does defining airth
// operations on a Bool make sense?
/** A data type for booleans, defined as a single bit indicating true or false.
  */
sealed class Bool(lit: Option[ULit] = None) extends UInt(Width(1), lit) {
  private[Chisel] override def cloneTypeWidth(w: Width): this.type = {
    require(!w.known || w.get == 1)
    new Bool().asInstanceOf[this.type]
  }

  override private[Chisel] def fromInt(value: BigInt, width: Int): this.type = {
    require((value == 0 || value == 1) && width == 1)
    (value == 1).asBool.asInstanceOf[this.type]
  }

  // REVIEW TODO: Why does this need to exist and have different conventions
  // than Bits?
  def & (other: Bool): Bool = binop(Bool(), BitAndOp, other)
  def | (other: Bool): Bool = binop(Bool(), BitOrOp, other)
  def ^ (other: Bool): Bool = binop(Bool(), BitXorOp, other)

  /** Returns this wire bitwise-inverted. */
  override def unary_~ : Bool = unop(Bool(), BitNotOp)

  /** Outputs the logical OR of two Bools.
   */
  def || (that: Bool): Bool = this | that

  /** Outputs the logical AND of two Bools.
   */
  def && (that: Bool): Bool = this & that
}

object Bool {
  /** Creates an empty Bool.
   */
  def apply(): Bool = new Bool()

  /** Creates Bool literal.
   */
  def Lit(x: Boolean): Bool = {
    val result = new Bool(Some(ULit(if (x) 1 else 0, Width(1))))
    // Bind result to being an Literal
    result.binding = LitBinding()
    result
  }
}

object Mux {
  /** Creates a mux, whose output is one of the inputs depending on the
    * value of the condition.
    *
    * @param cond condition determining the input to choose
    * @param con the value chosen when `cond` is true
    * @param alt the value chosen when `cond` is false
    * @example
    * {{{
    * val muxOut = Mux(data_in === 3.asUInt, 3.asUInt(4), 0.asUInt(4))
    * }}}
    */
  def apply[T <: Data](cond: Bool, con: T, alt: T): T = (con, alt) match {
    // Handle Mux(cond, UInt, Bool) carefully so that the concrete type is UInt
    case (c: Bool, a: Bool) => doMux(cond, c, a).asInstanceOf[T]
    case (c: UInt, a: Bool) => doMux(cond, c, a << 0).asInstanceOf[T]
    case (c: Bool, a: UInt) => doMux(cond, c << 0, a).asInstanceOf[T]
    case (c: Bits, a: Bits) => doMux(cond, c, a).asInstanceOf[T]
    case _ => doAggregateMux(cond, con, alt)
  }

  private def doMux[T <: Data](cond: Bool, con: T, alt: T): T = {
    require(con.getClass == alt.getClass, s"can't Mux between ${con.getClass} and ${alt.getClass}")
    Binding.checkSynthesizable(cond, s"'cond' ($cond)")
    Binding.checkSynthesizable(con, s"'con' ($con)")
    Binding.checkSynthesizable(alt, s"'alt' ($alt)")
    val d = alt.cloneTypeWidth(con.width max alt.width)
    pushOp(DefPrim(d, MultiplexOp, cond.ref, con.ref, alt.ref))
  }

  private def doAggregateMux[T <: Data](cond: Bool, con: T, alt: T): T = {
    require(con.getClass == alt.getClass, s"can't Mux between ${con.getClass} and ${alt.getClass}")
    for ((c, a) <- con.flatten zip alt.flatten)
      require(c.width == a.width, "can't Mux between aggregates of different width")
    doMux(cond, con, alt)
  }
}

