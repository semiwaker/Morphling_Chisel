package codegen

import chisel3._
import chisel3.util._

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class CodeGenTestModule extends Module {
  val io = IO(new Bundle {
    val in = Input(Bool())
    val start = Input(Bool())
    val out = Output(UInt(8.W))
  })
  val reg = RegNext(io.out, init = 0.U)
  val i = RegNext(io.out)
  val j = RegNext(io.out)
  io.out := reg
  // reg := reg
  val c = WhileLoop(io.in, "while")(
    ForLoop(i, 0.U, 5.U, 1.U, "i_loop")(
      OneStepCode("reg") {
        reg := i
      }
    )
  )
  c.generate()
  c.sig_reset := io.start
  c.sig_en := 1.B
  printf("Step out %d %d %d\n", io.out, i, c.sig_valid)
}

class CodeGenTester(t: CodeGenTestModule) extends PeekPokeTester(t) {
  poke(t.io.start, 1)
  poke(t.io.in, 1)
  step(1)
  poke(t.io.start, 0)
  step(20)
  poke(t.io.in, 0)
  step(10)
  poke(t.io.start, 1)
  poke(t.io.in, 1)
  step(1)
  poke(t.io.start, 0)
  step(30)
}

object CodeGenMain extends App {
//   DebugSwitch.on()
  iotesters.Driver.execute(args, () => new CodeGenTestModule) { c =>
    new CodeGenTester(c)
  }
}
