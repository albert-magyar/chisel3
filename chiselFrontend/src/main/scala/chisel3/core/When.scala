// See LICENSE for license details.

package chisel3.core

import scala.language.experimental.macros

import chisel3.internal._
import chisel3.internal.Builder.pushCommand
import chisel3.internal.firrtl._
import chisel3.internal.sourceinfo.{SourceInfo}

object when {  // scalastyle:ignore object.name
  /** Create a `when` condition block, where whether a block of logic is
    * executed or not depends on the conditional.
    *
    * @param cond condition to execute upon
    * @param block logic that runs only if `cond` is true
    *
    * @example
    * {{{
    * when ( myData === 3.U ) {
    *   // Some logic to run when myData equals 3.
    * } .elsewhen ( myData === 1.U ) {
    *   // Some logic to run when myData equals 1.
    * } .otherwise {
    *   // Some logic to run when myData is neither 3 nor 1.
    * }
    * }}}
    */
  def apply(cond: => Bool)(block: => Unit)(implicit sourceInfo: SourceInfo): WhenContext = {
    new WhenContext(sourceInfo, Some(() => cond), block)
  }
}

/** Internal mechanism for generating a when. Because of the way FIRRTL
  * commands are emitted, generating a FIRRTL elsewhen or nested whens inside
  * elses would be difficult. Instead, this keeps track of the negative of the
  * previous conditions, so when an elsewhen or otherwise is used, it checks
  * that both the condition is true and all the previous conditions have been
  * false.
  */
final class WhenContext(sourceInfo: SourceInfo, cond: Option[() => Bool], block: => Unit, depth: Int = 0) {
  /** This block of logic gets executed if above conditions have been false
    * and this condition is true.
    */
  def elsewhen (elseCond: => Bool)(block: => Unit)(implicit sourceInfo: SourceInfo): WhenContext = {
    new WhenContext(sourceInfo, Some(() => elseCond), block, depth+1)
  }

  /** This block of logic gets executed only if the above conditions were all
    * false. No additional logic blocks may be appended past the `otherwise`.
    */
  def otherwise(block: => Unit)(implicit sourceInfo: SourceInfo): Unit =
    new WhenContext(sourceInfo, None, block, depth+1)

  for (i <- 1 until depth) { pushCommand(BlockBegin(sourceInfo)) }
  if (depth > 0) { pushCommand(ElseBegin(sourceInfo)) }

  /* The lazy argument pattern allows c to be called here,
   * emitting the declaration and assignment of the Bool node
   * of the predicate in the correct place.
   */
  cond.foreach( c => pushCommand(WhenPredicate(sourceInfo, c().ref)) )

  block
  cond.foreach( c => pushCommand(BlockEnd(sourceInfo)) )
  for (i <- 0 until depth) { pushCommand(BlockEnd(sourceInfo)) }
}
