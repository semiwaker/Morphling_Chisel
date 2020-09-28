package codegen

import chisel3._
import chisel3.util._

import scala.collection.immutable.Seq

object DebugSwitch {
  var switch = false
  def apply(): Boolean = switch
  def on(): Unit = { switch = true }
  def off(): Unit = { switch = false }
}

object UnnamedCode {
  var cnt = 0
  def apply(): Int = {
    cnt += 1
    cnt
  }
}

abstract class CodeGenerator(name: String = s"Unnamed Code ${UnnamedCode()}") {
  val sig_en = Wire(Bool())
  val sig_valid = RegInit(0.B)
  val sig_reset = Wire(Bool())

  sig_reset := 0.B

  def generate(): Unit

  def code_name = name

  def share_en(other: CodeGenerator) = {
    other.sig_en := sig_en
    this
  }
  def share_reset(other: CodeGenerator) = {
    other.sig_reset := sig_reset
    this
  }

  def dprintf(pable: Printable): Unit =
    if (DebugSwitch())
      printf(pable)
}

class Code(name: String = s"Unnamed Code ${UnnamedCode()}")(
    func: (UInt) => Unit
) extends CodeGenerator(name) {
  def generate(): Unit = {
    when(sig_reset) {
      dprintf(p"$name reset\n")
      sig_valid := 0.B
    }
    when(sig_en) {
      func(sig_valid)
    }
  }
}

object Code {
  def apply(name: String = s"Unnamed Code ${UnnamedCode()}")(
      func: (UInt) => Unit
  ) = new Code(name)(func)
}

object OneStepCode {
  def apply(name: String = s"Unnamed Code ${UnnamedCode()}")(func: => Unit) =
    Code(name)((sig_valid) => {
      func
      sig_valid := 1.B
    })
}

object DoNothing {
  def apply() = OneStepCode("Do nothing")({})
}

class CodeBlock(name: String = s"Unnamed Code Block ${UnnamedCode()}")
    extends CodeGenerator(name) {
  var stages = 0
  var blocks = List[CodeGenerator]()

  def add(code: CodeGenerator): Unit = {
    blocks :+= code
    stages += 1
  }
  def generate(): Unit = {
    val code_states = Enum(stages + 1).toArray
    val state = RegInit(code_states(0))
    for (i <- 0 until stages)
      blocks(i).generate()
    for (i <- 0 until stages)
      blocks(i).sig_en := 0.B
    when(sig_reset) {
      dprintf(p"$name Reset\n")
      state := code_states(0)
      blocks(0).sig_reset := 1.B
      sig_valid := 0.B
    }
    when(sig_en) {
      dprintf(p"$name State $state\n")
      for (i <- 0 until stages)
        when(state === code_states(i)) {
          when(blocks(i).sig_valid) {
            state := code_states(i + 1)
            if (i != stages - 1)
              blocks(i + 1).sig_reset := 1.B
            else {
              sig_valid := 1.B
              // dprintf(p"$name Finished\n")
            }
          }.otherwise {
            blocks(i).sig_en := 1.B
            state := code_states(i)
          }
        }
      when(state === code_states(stages)) {
        sig_valid := 1.B
      }
    }
  }
}

object CodeBlock {
  def apply(
      name: String = s"Unnamed Code Block ${UnnamedCode()}"
  )(codes: List[CodeGenerator]) = {
    val c = new CodeBlock(name)
    for (code <- codes)
      c.add(code)
    c
  }
}

class If(
    pred: Bool,
    name: String = s"Unnamed If ${UnnamedCode()}"
)(truebody: CodeGenerator, falseBody: CodeGenerator)
    extends CodeGenerator(name) {
  def generate(): Unit = {
    truebody.generate()
    truebody.sig_en := 0.B
    truebody.sig_reset := 0.B

    falseBody.generate()
    falseBody.sig_en := 0.B
    falseBody.sig_reset := 0.B

    when(pred) {
      truebody.sig_en := 1.B
      truebody.sig_reset := sig_reset
      sig_valid := truebody.sig_valid
    }.otherwise {
      falseBody.sig_en := 1.B
      falseBody.sig_reset := sig_reset
      sig_valid := falseBody.sig_valid
    }
  }
}
object If {
  def apply(
      pred: Bool,
      name: String = s"Unnamed If ${UnnamedCode()}"
  )(truebody: CodeGenerator, falseBody: CodeGenerator) =
    new If(pred, name)(truebody, falseBody)
}

class Switch(
    variable: UInt,
    name: String = s"Unnamed Switch ${UnnamedCode()}"
)(cases: List[Tuple2[Int, CodeGenerator]])
    extends CodeGenerator(name) {
  def generate() = {
    val valid = (for ((v, c) <- cases) yield {
      c.generate()
      val cmp = Wire(Bool())
      cmp := v.U === variable
      c.sig_en := cmp && sig_en
      c.sig_reset := sig_reset
      (cmp && c.sig_valid).asUInt()
    }).reduce(Cat(_, _))
    sig_valid := valid.orR()
  }
}

object Switch {
  def apply(
      variable: UInt,
      name: String = s"Unnamed Switch ${UnnamedCode()}"
  )(cases: List[Tuple2[Int, CodeGenerator]]): CodeGenerator =
    new Switch(variable, name)(cases)
}

// object BinarySwitch {
//   def apply(
//       variable: UInt,
//       name: String = s"Unnamed BinarySwitch ${UnnamedCode()}"
//   )(cases: List[Tuple2[Int, CodeGenerator]]): CodeGenerator = {
//     val sortedCases = cases.toArray.sortWith((a, b) => a._1 < b._1)
//     class SubSwitch(name: String)(s: Int, t: Int) extends CodeGenerator(name) {
//       val mid = (s + t) / 2
//       val lson: CodeGenerator =
//         if (t - s > 2) new SubSwitch(name + "_L")(s, mid)
//         else if (t - s == 2 || t - s == 1)
//           If(
//             variable === sortedCases(s)._1.U,
//             name + s"_Case ${sortedCases(s)._1}"
//           )(sortedCases(s)._2, DoNothing())
//         else DoNothing()
//       val rson: CodeGenerator =
//         if (t - s > 2) new SubSwitch(name + "_R")(mid, t)
//         else if (t - s == 1)
//           If(
//             variable === sortedCases(s + 1)._1.U,
//             name + s"_Case ${sortedCases(s + 1)._1}"
//           )(sortedCases(s + 1)._2, DoNothing())
//         else DoNothing()
//       val ifbranch =
//         If(
//           variable <= sortedCases(mid)._1.U,
//           name + s"_Branch"
//         )(lson, rson)

//       def generate(): Unit = {
//         ifbranch.generate()
//         ifbranch.sig_en := sig_en
//         ifbranch.sig_reset := sig_reset
//         sig_valid := ifbranch.sig_valid
//       }
//     }
//     new SubSwitch(name + "_Sub")(0, sortedCases.length)
//   }
// }

class ForLoop(
    variable: UInt,
    start: UInt,
    end: UInt,
    stride: UInt = 1.U,
    name: String = s"Unnamed ForLoop ${UnnamedCode()}"
)(body: CodeGenerator)
    extends CodeGenerator(name) {
  def generate(): Unit = {
    body.generate()
    body.sig_en := 0.B
    variable := variable
    when(sig_reset) {
      dprintf(p"$name reset\n")
      variable := start
      body.sig_reset := 1.B
      sig_valid := 0.B
    }
    when(sig_en) {
      dprintf(p"$name Var $variable\n")
      when(variable >= end) {
        sig_valid := 1.B
      }.otherwise {
        when(body.sig_valid && !sig_reset) {
          variable := variable + stride
          body.sig_reset := 1.B
        }.otherwise {
          body.sig_en := 1.B
        }
      }
    }
  }
}

object ForLoop {
  def apply(
      variable: UInt,
      start: UInt,
      end: UInt,
      stride: UInt = 1.U,
      name: String = s"Unnamed ForLoop ${UnnamedCode()}"
  )(
      body: CodeGenerator
  ) =
    new ForLoop(variable, start, end, stride, name)(body)
}

class WhileLoop(
    pred: Bool,
    name: String = s"Unnamed WhileLoop ${UnnamedCode()}"
)(body: CodeGenerator)
    extends CodeGenerator(name) {
  def generate(): Unit = {
    body.generate()
    body.sig_en := 0.B
    body.sig_reset := 0.B
    when(sig_reset) {
      dprintf(p"$name reset\n")
      body.sig_reset := 1.B
      sig_valid := 0.B
    }
    when(sig_en) {
      dprintf(p"$name Pred $pred\n")
      when(
        (!pred && (body.sig_valid || sig_reset)) || (sig_valid && !sig_reset)
      ) {
        sig_valid := 1.B
      }.otherwise {
        body.sig_en := 1.B
        when(body.sig_valid) {
          body.sig_reset := 1.B
        }
      }
    }
  }
}

object WhileLoop {
  def apply(
      pred: Bool,
      name: String = s"Unnamed WhileLoop ${UnnamedCode()}"
  )(
      body: CodeGenerator
  ) =
    new WhileLoop(pred, name)(body)
}

class Pipeline(
    valid: Bool,
    name: String = s"Unnamed Pipeline ${UnnamedCode()}"
)(codes: List[CodeGenerator])
    extends CodeGenerator(name) {
  def generate() = {
    val allReset = Wire(Bool())
    allReset := 0.B
    val ens = (for (i <- codes) yield RegInit(0.B))
    var last_en = sig_en
    val allValid = (for ((i, en) <- codes zip ens) yield {
      i.generate()
      i.sig_en := sig_en && en
      i.sig_reset := allReset
      en := last_en
      last_en = en
      i.sig_valid.asUInt()
    }).reduce(Cat(_, _)).orR()
    sig_valid := valid
    when(sig_reset) {
      dprintf(p"$name reset\n")
      allReset := 1.B
      valid := 0.B
      for (e <- ens) e := 0.B
    }
    when(sig_en && allValid) {
      allReset := 1.B
    }
  }
}
object Pipeline {
  def apply(
      valid: Bool,
      name: String = s"Unnamed Pipeline ${UnnamedCode()}"
  )(codes: List[CodeGenerator]) = new Pipeline(valid, name)(codes)
}

// class ForPipeline(
//     variable: UInt,
//     start: UInt,
//     end: UInt,
//     stride: UInt = 1.U,
//     name: String = s"Unnamed ForPipeline ${UnnamedCode()}"
// )(codes: List[CodeGenerator])
//     extends CodeGenerator(name) {
//   def generate() = {
//     val allReset = Wire(0.B)
//     val allValid = (for (i <- codes) yield {
//       i.generate()
//       i.sig_en := sig_en
//       i.sig_reset := allReset
//       i.sig_valid.asUInt()
//     }).reduce(Cat(_, _)).orR()
//     variable := variable
//     when(sig_reset) {
//       dprintf(p"$name reset\n")
//       variable := start
//       allReset := 1.B
//       sig_valid := 0.B
//     }
//     when(sig_en) {
//       dprintf(p"$name Var $variable\n")
//       when(variable >= end) {
//         sig_valid := 1.B
//       }.otherwise {
//         when(allValid) {
//           variable := variable + stride
//           allReset := 1.B
//         }
//       }
//     }
//   }
// }
// object ForPipeline {
//   def apply(
//       variable: UInt,
//       start: UInt,
//       end: UInt,
//       stride: UInt = 1.U,
//       name: String = s"Unnamed ForPipeline ${UnnamedCode()}"
//   )(
//       codes: List[CodeGenerator]
//   ) =
//     new ForPipeline(variable, start, end, stride, name)(codes)
// }

class ParallelBlock(name: String = s"Unnamed Parallel Block ${UnnamedCode()}")
    extends CodeGenerator(name) {
  var blocks = List[CodeGenerator]()
  def add(code: CodeGenerator): Unit = {
    blocks :+= code
  }
  def generate(): Unit = {
    val allValid = (for (block <- blocks) yield {
      block.generate()
      block.sig_en := sig_en
      block.sig_reset := sig_reset
      block.sig_valid.asUInt()
    }).reduce(Cat(_, _))
    sig_valid := allValid.orR()
  }
}

object ParallelBlock {
  def apply(
      name: String = s"Unnamed Parallel Block ${UnnamedCode()}"
  )(codes: List[CodeGenerator]) = {
    val c = new ParallelBlock(name)
    for (code <- codes)
      c.add(code)
    c
  }
}

case class LoopParameter(
    variable: UInt,
    start: UInt,
    end: UInt,
    stride: UInt = 1.U,
    name: String = s"Unnamed ForLoop ${UnnamedCode()}"
)

object NestLoop {
  def apply(loops: Vector[LoopParameter])(body: CodeGenerator) = {
    var curr = body
    for (par <- loops.reverse) {
      curr =
        ForLoop(par.variable, par.start, par.end, par.stride, par.name)(curr)
    }
    curr
  }
}

class ForCounter(
    variable: UInt,
    start: UInt,
    end: UInt,
    stride: UInt = 1.U,
    name: String = s"Unnamed ForCounter ${UnnamedCode()}"
) extends CodeGenerator(name) {
  def generate(): Unit = {
    variable := variable
    when(sig_reset) {
      variable := start
      sig_valid := 0.B
    }
    when(sig_en) {
      when(variable >= end) {
        sig_valid := 1.B
      }.otherwise {
        when(!sig_reset) {
          variable := variable + stride
        }
      }
    }
  }
}
object ForCounter {
  def apply(
      variable: UInt,
      start: UInt,
      end: UInt,
      stride: UInt = 1.U,
      name: String = s"Unnamed ForCounter ${UnnamedCode()}"
  ) = new ForCounter(variable, start, end, stride, name)
}
