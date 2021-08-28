/**************************************************************************************
* Copyright (c) 2020 Institute of Computing Technology, CAS
* Copyright (c) 2020 University of Chinese Academy of Sciences
*
* NutShell is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*             http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
* FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline
import chisel3.experimental.doNotDedup

class SRAMBundleA(val set: Int) extends Bundle {
  val setIdx = Output(UInt(log2Up(set).W))

  def apply(setIdx: UInt) = {
    this.setIdx := setIdx
    this
  }
}

class SRAMBundleAW[T <: Data](private val gen: T, set: Int, val way: Int = 1) extends SRAMBundleA(set) {
  val data = Output(gen)
  val waymask = if (way > 1) Some(Output(UInt(way.W))) else None

  def apply(data: T, setIdx: UInt, waymask: UInt) = {
    super.apply(setIdx)
    this.data := data
    this.waymask.map(_ := waymask)
    this
  }
}

class SRAMBundleR[T <: Data](private val gen: T, val way: Int = 1) extends Bundle {
  val data = Output(Vec(way, gen))
}

class SRAMReadBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleA(set))
  val resp = Flipped(new SRAMBundleR(gen, way))

  def apply(valid: Bool, setIdx: UInt) = {
    this.req.bits.apply(setIdx)
    this.req.valid := valid
    this
  }
}

class SRAMWriteBus[T <: Data](private val gen: T, val set: Int, val way: Int = 1) extends Bundle {
  val req = Decoupled(new SRAMBundleAW(gen, set, way))

  def apply(valid: Bool, data: T, setIdx: UInt, waymask: UInt) = {
    this.req.bits.apply(data = data, setIdx = setIdx, waymask = waymask)
    this.req.valid := valid
    this
  }
}

// val allowed_organizations = List(
    //   "wayway_se-nk", "wayway_ba-et",
    //   "wwaayy_se-nk", "wwaayy_ba-et",
    // )
    // other organizations will be supported later
    // splitway:
    //      ┌──────────────┐       ┌──────────────┐
    // way0 │     set0     │  way1 │     set1     │
    //      ├──────────────┤       ├──────────────┤
    //      │     set2     │       │     set3     │
    //      └──────────────┘       └──────────────┘
    // wayway:
    //      ┌──────────────┐
    // way0 │     set0     │
    //      ├──────────────┤
    //      │     set1     │
    //      ├──────────────┤
    // way1 │     set0     │
    //      ├──────────────┤
    //      │     set1     │
    //      └──────────────┘
    // wwaayy:
    //      ┌──────────────┐
    // set0 │     way0     │
    //      ├──────────────┤
    //      │     way1     │
    //      ├──────────────┤
    // set1 │     way0     │
    //      ├──────────────┤
    //      │     way1     │
    //      └──────────────┘
    // ba-et:
    //      ┌──────────────┐
    // bank0│     set0     │
    //      ├──────────────┤
    //      │     set1     │
    //      ├──────────────┤
    // bank1│     set2     │
    //      ├──────────────┤
    //      │     set3     │
    //      └──────────────┘
    // se-nk:
    //      ┌──────────────┐
    // bank0│     set0     │
    //      ├──────────────┤
    //      │     set2     │
    //      ├──────────────┤
    // bank1│     set1     │
    //      ├──────────────┤
    //      │     set3     │
    //      └──────────────┘

class SRAMTemplate[T <: Data](gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false,
  initiate: Boolean = false, modulePrefix: String = "",
  bankID: Int = -1, organization: String = "", blockID: Int = 0,
  ) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  require(way > 1)

  val wordType = UInt(gen.getWidth.W)
  // maybe Vec is not supported by loadMem, TODO: try bundle or simple types
  val arrays = Seq.fill(way)(SyncReadMem(set, wordType))
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (initiate) {
    assume(modulePrefix.trim.nonEmpty)
    assume(organization.trim.nonEmpty)
    val allowed_organizations = List("splitway_Senk", "wayway_Senk")
    assume(allowed_organizations contains organization)
    assume(bankID >= 0)

    arrays.zipWithIndex.foreach {
      case(array, i) =>
        val ram_initiator = s"${modulePrefix}_${organization}_bank${bankID}_way${i}_block${blockID}.txt"
        val init_file = "./" + ram_initiator
        loadMemoryFromFileInline(array, init_file)
        println(init_file)
    }
  }

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val firstCycle = RegNext(false.B, init=true.B)
  dontTouch(firstCycle)

  val (ren, wen) = (io.r.req.valid, (io.w.req.valid || resetState) && !reset.asBool)
  dontTouch(wen)

  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val waymask = Mux(resetState, Fill(way, "b1".U), io.w.req.bits.waymask.getOrElse("b1".U))
  // val wdata = VecInit(Seq.fill(way)(wdataword))
  arrays.zipWithIndex.foreach {
    case (array, i) =>
      when (wen && waymask(i)) {
        array.write(setIdx, wdataword)
      }
  }

  val rdata = arrays.map {
    array =>
      val single_data = 
      (if (holdRead)
        ReadAndHold(array, io.r.req.bits.setIdx, realRen)
      else
        array.read(io.r.req.bits.setIdx, realRen))
      single_data
  }

  io.r.resp.data := VecInit(rdata)

  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

}

class SRAMTemplateWithArbiter[T <: Data](nRead: Int, gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(Vec(nRead, new SRAMReadBus(gen, set, way)))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val ram = Module(new SRAMTemplate(gen, set, way, shouldReset, holdRead = false, singlePort = true))
  doNotDedup(ram)
  ram.io.w <> io.w

  val readArb = Module(new Arbiter(chiselTypeOf(io.r(0).req.bits), nRead))
  readArb.io.in <> io.r.map(_.req)
  ram.io.r.req <> readArb.io.out

  // latch read results
  io.r.map{ case r => {
    r.resp.data := HoldUnless(ram.io.r.resp.data, RegNext(r.req.fire()))
  }}
}

class SRAMTemplate1[T <: Data](gen: T, set: Int, way: Int = 1,
  shouldReset: Boolean = false, holdRead: Boolean = false, singlePort: Boolean = false,
  initiate: Boolean = false, modulePrefix: String = "",
  bankID: Int = -1, organization: String = "", blockID: Int = 0,
  ) extends Module {
  val io = IO(new Bundle {
    val r = Flipped(new SRAMReadBus(gen, set, way))
    val w = Flipped(new SRAMWriteBus(gen, set, way))
  })

  val wordType = UInt(gen.getWidth.W)
  // maybe Vec is not supported by loadMem, TODO: try bundle or simple types
  if (initiate) {
    assume(modulePrefix.trim.nonEmpty)
    assume(organization.trim.nonEmpty)
    val allowed_organizations = List("wayway_Senk")
    assume(allowed_organizations contains organization)
    assume(bankID >= 0)
  }
  val array = SyncReadMem(set, wordType)
  if (initiate) {
    val ram_initiator = s"${modulePrefix}_${organization}_bank${bankID}_block${blockID}.txt"
    val init_file = "./" + ram_initiator
    loadMemoryFromFileInline(array, init_file)
    println(init_file)
  }
  val (resetState, resetSet) = (WireInit(false.B), WireInit(0.U))

  if (shouldReset) {
    val _resetState = RegInit(true.B)
    val (_resetSet, resetFinish) = Counter(_resetState, set)
    when (resetFinish) { _resetState := false.B }

    resetState := _resetState
    resetSet := _resetSet
  }

  val firstCycle = RegNext(false.B, init=true.B)
  dontTouch(firstCycle)

  val (ren, wen) = (io.r.req.valid, (io.w.req.valid || resetState) && !reset.asBool)
  dontTouch(wen)

  val realRen = (if (singlePort) ren && !wen else ren)

  val setIdx = Mux(resetState, resetSet, io.w.req.bits.setIdx)
  val wdataword = Mux(resetState, 0.U.asTypeOf(wordType), io.w.req.bits.data.asUInt)
  val wdata = wdataword
  when (wen) { array.write(setIdx, wdata) }

  val rdata = if (holdRead) ReadAndHold(array, io.r.req.bits.setIdx, realRen)
               else array.read(io.r.req.bits.setIdx, realRen)
  io.r.resp.data := VecInit(rdata)

  io.r.req.ready := !resetState && (if (singlePort) !wen else true.B)
  io.w.req.ready := true.B

}