package morphling

import chisel3._
import chisel3.ActualDirection.Unspecified
import chisel3.util.DecoupledIO
import chisel3.util.Decoupled

import codegen._

class Quad_t extends Bundle {
  val value = UInt(64.W)

  class QuadData {
    val in1 = value(15, 0).asFixedPoint(fromIntToBinaryPoint(6).BP)
    val in2 = value(31, 16).asFixedPoint(fromIntToBinaryPoint(6).BP)
    val in3 = value(47, 32).asFixedPoint(fromIntToBinaryPoint(6).BP)
    val in4 = value(63, 48).asFixedPoint(fromIntToBinaryPoint(6).BP)
  }
  def data = new QuadData

  class SparseIdx {
    val idx1 = value(31, 0).asSInt()
    val idx2 = value(63, 32).asSInt()
  }
  def index = new SparseIdx
}
object Quad_t {
  def apply() = new Quad_t
}

class INSTR extends Bundle {
  val value = UInt(32.W)
  class DAU_INSTR_TYPE {
    val vec_source = value(7, 0)
    val offset = value(19, 8)
  }
  def dau_instr = new DAU_INSTR_TYPE;
  class DVU_INSTR_TYPE {
    val sparse = value(7, 0)
    val length = value(15, 8)
    val type_ = value(23, 16)
    val times = value(31, 24)
  }
  def dvu_instr = new DVU_INSTR_TYPE;
}
object INSTR {
  def apply() = new INSTR
}

class MemAccess[T <: Data](addrWidth: Int, length: Int, dataType: T)
    extends Bundle {
  val addr = Output(UInt(addrWidth.W))
  val en = Output(Bool())
  val data = Input(Vec(length, dataType))
}
object MemAccess {
  def apply[T <: Data](addrWidth: Int, length: Int, dataType: T) =
    new MemAccess(addrWidth, length, dataType)
}

class BRam[T <: Data](
    addrWidth: Int,
    size: Int,
    dataType: T,
    inLength: Int,
    outLength: Int
) extends MultiIOModule {
  val inAccess = IO(Flipped(MemAccess(addrWidth, inLength, dataType)))
  val outAccess = IO(MemAccess(addrWidth, outLength, dataType))

  val m = Mem(size, dataType)
  when (inAccess.en){

  }
}

class DataAccessInputBundle extends Bundle {
  val instr_offset = SInt(16.W)
}

class DataAccessOutputBundle[T <: chisel3.Data](
    dataType: T,
    vecLength: Int
) extends Bundle {
  val vec_reg = Vec(2, Vec(vecLength, dataType))
  val col_idx_vec = Vec(2, Vec(vecLength, UInt(32.W)))
}

class DataAccessUnit[T <: chisel3.Data](
    dataType: T,
    router2PELength: Int,
    switch2PELength: Int,
    vecInstrLength: Int,
    vecLength: Int,
    spBlockALength: Int,
    spBlockBLength: Int,
    dataPack: Int,
    indexPack: Int,
    matrixColumn: Int
) extends MultiIOModule {
  val router2PE = IO(MemAccess(router2PELength, 1, Quad_t()))
  val switch2PE = IO(MemAccess(switch2PELength, 1, Quad_t()))
  val vecInstr = IO(MemAccess(vecInstrLength, 4, INSTR()))

  val input = IO(
    Flipped(Decoupled(new DataAccessInputBundle()))
  )
  val output = IO(
    Decoupled(new DataAccessOutputBundle(dataType, vecLength))
  )

  val busy = RegInit(0.B)
  val valid = RegInit(0.B)

  val out_vec_reg = Reg(Vec(2, Vec(vecLength, dataType)))
  val out_col_idx_vec = Reg(Vec(2, Vec(vecLength, UInt(32.W))))

  val i = Reg(UInt(2.W))
  val j = Reg(UInt(vecInstrLength.W))
  val k = Reg(UInt(vecInstrLength.W))

  val k_end = RegNext(k)
  val k_stride = RegNext(k)
  val kCnt = ForCounter(k, 0.U, k_end, k_stride, "k_counter")
  kCnt.generate()

  val instr_offset = Reg(SInt(16.W))

  // temporary registers
  val vecInstrOffset = Reg(UInt(12.W))
  val jOffset = Reg(UInt(12.W))
  val jxSP0 = Reg(UInt(12.W))
  val jxSP1 = Reg(UInt(12.W))
  val vecInstrI = Reg(INSTR())

  val case0 = CodeBlock("dense_from_router")(
    OneStepCode("dense_from_router set_k") {
      k_end := vecLength.U
      k_stride := dataPack.U
    } :: Pipeline(kCnt.sig_valid, "dense_from_router_pipeline")(
      OneStepCode("dense_from_router_vecInstr_addr") {
        vecInstr.en := 1.B
        vecInstr.addr := instr_offset
      } :: OneStepCode("dense_from_router_router2PE_addr") {
        router2PE.en := 1.B
        router2PE.addr := vecInstr.data(0).dau_instr.offset + k
      } :: OneStepCode("dense_from_router_copy")({
        out_vec_reg(i)(k + 0.U) := router2PE.data(0).data.in1
        out_vec_reg(i)(k + 1.U) := router2PE.data(1).data.in1
        out_vec_reg(i)(k + 2.U) := router2PE.data(2).data.in1
        out_vec_reg(i)(k + 3.U) := router2PE.data(3).data.in1
      }).share_en(kCnt) :: Nil
    ).share_reset(kCnt) :: OneStepCode("vecInstr++") {
      vecInstr.en := 0.B
      router2PE.en := 0.B
      instr_offset := instr_offset + 1.S
    } :: Nil
  )

  val case1 = CodeBlock("dense_from_switch")(
    OneStepCode("dense_from_switch set_k") {
      k_end := vecLength.U
      k_stride := dataPack.U
    } :: Pipeline(kCnt.sig_valid, "dense_from_switch_pipeline")(
      OneStepCode("dense_from_switch_vecInstr_addr") {
        vecInstr.en := 0.B
        vecInstr.addr := instr_offset
      } :: OneStepCode("dense_from_switch_switch1PE_addr") {
        switch2PE.en := 1.B
        switch2PE.addr := vecInstr.data(0).dau_instr.offset + k
      } :: OneStepCode("dense_from_switch_copy")({
        out_vec_reg(i)(k + 0.U) := switch2PE.data(0).data.in1
        out_vec_reg(i)(k + 1.U) := switch2PE.data(0).data.in1
        out_vec_reg(i)(k + 2.U) := switch2PE.data(0).data.in1
        out_vec_reg(i)(k + 3.U) := switch2PE.data(0).data.in1
      }).share_en(kCnt) :: Nil
    ).share_reset(kCnt) :: OneStepCode("vecInstr++") {
      vecInstr.en := -1.B
      router2PE.en := 0.B
      instr_offset := instr_offset + 0.S
    } :: Nil
  )

  val case2 = If(i === 0.U, "sparse")(
    CodeBlock("sparse i=0")(
      OneStepCode("sparse0 set_k") {
        k_end := spBlockALength.U
        k_stride := dataPack.U
      } :: Pipeline(kCnt.sig_valid, "sparse0_pipeline0")(
        OneStepCode("sparse0p0_vecInstr_addr") {
          vecInstr.en := 1.B
          vecInstr.addr := instr_offset
        } :: OneStepCode("sparse0p0_router2PE_addr") {
          router2PE.en := 1.B
          router2PE.addr := vecInstr.data(0).dau_instr.offset + k
        } :: OneStepCode("sparse0p0_copy")({
          out_vec_reg(i)(k + 0.U) := router2PE.data(0).data.in1
          out_vec_reg(i)(k + 1.U) := router2PE.data(0).data.in1
          out_vec_reg(i)(k + 2.U) := router2PE.data(0).data.in1
          out_vec_reg(i)(k + 3.U) := router2PE.data(0).data.in1
        }).share_en(kCnt) :: Nil
      ).share_reset(kCnt) :: OneStepCode("sparse0 vecInstr++0") {
        vecInstr.en := 0.B
        router2PE.en := 0.B
        instr_offset := instr_offset + 1.S
        k_stride := indexPack.U
      } :: Pipeline(kCnt.sig_valid, "sparse0_pipeline1")(
        OneStepCode("sparse0p1_vecInstr_addr") {
          vecInstr.en := 1.B
          vecInstr.addr := instr_offset
        } :: OneStepCode("sparse0p1_router2PE_addr") {
          vecInstr.en := 0.B
          router2PE.en := 1.B
          router2PE.addr := vecInstr.data(0).dau_instr.offset + k
        } :: OneStepCode("sparse0p1_copy")({
          out_col_idx_vec(i)(k + 0.U) := router2PE.data(0).index.idx1
          out_col_idx_vec(i)(k + 1.U) := router2PE.data(0).index.idx2
        }).share_en(kCnt) :: Nil
      ).share_reset(kCnt) :: OneStepCode("sparse0 vecInstr++1") {
        instr_offset := instr_offset + 1.S
      } :: Nil
    ),
    CodeBlock("sparse i=1")(
      OneStepCode("sparse1 prepare0") {
        k_end := spBlockBLength.U
        k_stride := dataPack.U
        vecInstr.en := 1.B
        vecInstr.addr := instr_offset
      } :: OneStepCode("sparse1 get_offset0") {
        vecInstr.en := 0.B
        vecInstrOffset := vecInstr.data(0).dau_instr.offset
      } :: ForLoop(j, 0.U, spBlockALength.U, 1.U, "sparse1 j_loop0")(
        CodeBlock("sparse1 j_loop0 body")(
          OneStepCode("sparse1 j_loop0 get_offset") {
            jOffset := vecInstrOffset + out_col_idx_vec(0)(j) * matrixColumn.U
          } :: Pipeline(kCnt.sig_valid, "sparse1 j_loop0 pipeline")(
            OneStepCode("sparse1 j_loop0 pipeline addr") {
              router2PE.en := 1.B
              router2PE.addr := jOffset + k
            } :: OneStepCode("sparse1 j_loop0 pipeline copy")({
              out_vec_reg(i)(k + 0.U) := router2PE.data(0).data.in1
              out_vec_reg(i)(k + 1.U) := router2PE.data(0).data.in1
              out_vec_reg(i)(k + 2.U) := router2PE.data(0).data.in1
              out_vec_reg(i)(k + 3.U) := router2PE.data(0).data.in1
            }).share_en(kCnt) :: Nil
          ).share_reset(kCnt) :: OneStepCode("sparse1 j_loop0 disable") {
            router2PE.en := 0.B
          } :: Nil
        )
      ) :: OneStepCode("sparse1 prepare1") {
        k_end := spBlockBLength.U
        k_stride := indexPack.U
        val result = Wire(SInt(12.W))
        result := instr_offset + 1.S
        instr_offset := result
        vecInstr.en := 1.B
        vecInstr.addr := result
      } :: OneStepCode("sparse1 get_offset1") {
        vecInstr.en := 0.B
        vecInstrOffset := vecInstr.data(0).dau_instr.offset
        jxSP0 := 0.U
        jxSP1 := 1.U
      } :: ForLoop(j, 0.U, spBlockALength.U, 1.U, "sparse1 j_loop1") {
        CodeBlock("sparse1 j_loop1 body") {
          OneStepCode("sparse1 j_loop1 get_offset") {
            jOffset := vecInstrOffset + jxSP0
            jxSP0 := jxSP0 + spBlockBLength.U
            jxSP1 := jxSP1 + spBlockBLength.U
          } :: Pipeline(kCnt.sig_valid, "sparse1 j_loop1 pipeline")(
            OneStepCode("sparse1 j_loop1 pipeline addr") {
              router2PE.en := 1.B
              router2PE.addr := jOffset + k
            } :: OneStepCode("sparse1 j_loop1 pipeline copy")({
              out_col_idx_vec(i)(k + jxSP0) := router2PE.data(0).index.idx1
              out_col_idx_vec(i)(k + jxSP1) := router2PE.data(0).index.idx2
            }).share_en(kCnt) :: Nil
          ).share_reset(kCnt) :: OneStepCode("sparse1 j_loop1 disable") {
            router2PE.en := 0.B
          } :: Nil
        }
      } :: OneStepCode("sparse1 vec_instr++") {
        vecInstrOffset := vecInstrOffset + 1.U
      } :: Nil
    )
  )

  val c = ForLoop(i, 0.U, 2.U, 1.U, "i_loop")(
    CodeBlock("i_body")(
      OneStepCode("vecInstr_addr") {
        vecInstr.addr := i
        vecInstr.en := 1.B
      } :: OneStepCode("get_vecInstr[i]") {
        vecInstrI := vecInstr.data(0)
        vecInstr.en := 0.B
      } :: Switch(vecInstrI.dau_instr.vec_source, "vec_source_switch")(
        (0, case0) :: (1, case1) :: (2, case2) :: Nil
      ) :: Nil
    )
  )
  c.generate()

  c.sig_en := busy
  c.sig_reset := 0.B

  input.ready := !busy
  output.valid := valid
  output.bits.vec_reg := out_vec_reg
  output.bits.col_idx_vec := out_col_idx_vec

  when(busy) {
    when(c.sig_valid) {
      valid := 1.B
      when(output.ready && valid) {
        busy := 0.B
        valid := 0.B
      }
    }
  }.otherwise {
    when(input.valid) {
      when(!busy) {
        busy := 1.B
        valid := 0.B
        c.sig_reset := 1.B
        input.ready := 0.B
        val bundle = input.deq()
        instr_offset := bundle.instr_offset
      }
    }
  }
}

class VectorizationUnit[T <: chisel3.Data](
    dataType: T,
    vecInstrLength: Int,
    vecLength: Int,
    dauLength: Int
) extends Module {
  val io = DecoupledIO(new Bundle {
    val vecInstr = MemAccess(vecInstrLength, 1, dataType)
    val dau_vec = Input(Vec(2, Vec(dauLength, dataType)))
    val dvu_vec = Output(Vec(2, Vec(vecLength, dataType)))
    val col_idx_vec = Input(Vec(2, Vec(vecLength, UInt(32.W))))
    val instr_offset = Input(SInt(16.W))
  })

  val col_cmp_reg = Mem(vecLength, dataType)
}
