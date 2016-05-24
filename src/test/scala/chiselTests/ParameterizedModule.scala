// See LICENSE for license details.

package chiselTests

import org.scalatest._
import Chisel._
import Chisel.testers.BasicTester

class ParameterizedModule(invert: Boolean) extends Module {
  val io = IO(new Bundle {
    val in  = Input(Bool())
    val out = Output(Bool())
  })
  if (invert) {
    io.out := !io.in
  } else {
    io.out := io.in
  }
}

/** A simple test to check Module deduplication doesn't affect correctness (two
  * modules with the same name but different contents aren't aliased). Doesn't
  * check that deduplication actually happens, though.
  */
class ParameterizedModuleTester() extends BasicTester {
  val invert = Module(new ParameterizedModule(true))
  val noninvert = Module(new ParameterizedModule(false))

  invert.io.in := true.asBool
  noninvert.io.in := true.asBool
  assert(invert.io.out === false.asBool)
  assert(noninvert.io.out === true.asBool)

  stop()
}

class ParameterizedModuleSpec extends ChiselFlatSpec {
  "Different parameterized modules" should "have different behavior" in {
    assertTesterPasses(new ParameterizedModuleTester())
  }
}
