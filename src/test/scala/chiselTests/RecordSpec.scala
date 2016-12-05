// See LICENSE for license details.

package chiselTests

import chisel3._
import scala.collection.immutable.ListMap

class RecordSpec extends ChiselFlatSpec {
  final class CustomBundle(elts: ListMap[String, Data]) extends Record {
    val elements = for ((field, elt) <- elts) yield field -> elt.chiselCloneType
    override def cloneType = (new CustomBundle(elements)).asInstanceOf[this.type]
  }
  class MyBundle extends Bundle {
    val foo = UInt(32.W)
    val bar = UInt(32.W)
    override def cloneType = (new MyBundle).asInstanceOf[this.type]
  }
  val listMap = ListMap("foo" -> UInt(32.W), "bar" -> UInt(32.W))

  class MyModule(output: => Record, input: => Record) extends Module {
    val io = IO(new Bundle {
      val in = Input(input)
      val out = Output(output)
    })
    io.out <> io.in
  }

  "Records" should "work similarly to Bundles" in {
    elaborate { new MyModule(new CustomBundle(listMap), new CustomBundle(listMap)) }
  }

  "Records" should "interoperate with Bundles" in {
    elaborate { new MyModule(new MyBundle, new CustomBundle(listMap)) }
  }

  "Bulk connect on Record" should "check that the fields match" in {
    (the [ChiselException] thrownBy {
      elaborate { new MyModule(new CustomBundle(listMap), new CustomBundle(listMap - "foo")) }
    }).getMessage should include ("Right Record missing field")

    (the [ChiselException] thrownBy {
      elaborate { new MyModule(new CustomBundle(listMap - "bar"), new CustomBundle(listMap)) }
    }).getMessage should include ("Left Record missing field")
  }
}
