/*
 * Copyright 2019 SiFive, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You should have received a copy of LICENSE.Apache2 along with
 * this software. If not, you may obtain a copy at
 *
 *    https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package sifive.blocks.inclusivecache

import Chisel._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import TLMessages._
import TLAtomics._
import TLPermissions._

class AtomicsLocal(params: TLBundleParameters) extends Module
{
  val io = new Bundle {
    val write    = Bool().flip // ignore opcode
    val a        = new TLBundleA(params).flip
    val data_in  = UInt(width = params.dataBits).flip
    val data_out = UInt(width = params.dataBits)
  }

  // Arithmetic, what to do
  val adder    = io.a.param(2)
  val unsigned = io.a.param(1)
  val take_max = io.a.param(0)

  val signBit = io.a.mask & Cat(UInt(1), ~io.a.mask >> 1)
  val inv_d = Mux(adder, io.data_in, ~io.data_in)
  val sum = (FillInterleaved(8, io.a.mask) & io.a.data) + inv_d
  def sign(x: UInt): Bool = (Cat(x.asBools.grouped(8).map(_.last).toList.reverse) & signBit).orR()
  val sign_a = sign(io.a.data)
  val sign_d = sign(io.data_in)
  val sign_s = sign(sum)
  val a_bigger_uneq = unsigned === sign_a // result if high bits are unequal
  val a_bigger = Mux(sign_a === sign_d, !sign_s, a_bigger_uneq)
  val pick_a = take_max === a_bigger

  // Logical, what to do
  val lut = MuxLookup(io.a.param(1, 0), 0x6.U, List(
    0.U -> 0x6.U,
    1.U -> 0xe.U,
    2.U -> 0x8.U,
    3.U -> 0xc.U
  ))
  val lutx = Vec(Seq(
    UInt(0x6),   // XOR
    UInt(0xe),   // OR
    UInt(0x8),   // AND
    UInt(0xc)))( // SWAP
    io.a.param(1,0))
  assert(lutx === lut)

  val logical = Cat((io.a.data.asBools zip io.data_in.asBools).map { case (a, d) =>
    lut(Cat(a, d))
  }.reverse)

  // Operation, what to do? (0=d, 1=a, 2=sum, 3=logical)
  val op = MuxLookup(io.a.opcode, 0.U, List(
    0.U -> 1.U,
    1.U -> 1.U,
    2.U -> Mux(adder, UInt(2), Mux(pick_a, UInt(1), UInt(0))), // ArithmeticData
    3.U -> 3.U
  ))
  val opx = Vec(Seq(
    UInt(1),   // PutFullData
    UInt(1),   // PutPartialData
    Mux(adder, UInt(2), Mux(pick_a, UInt(1), UInt(0))), // ArithmeticData
    UInt(3),   // LogicalData
    UInt(0),   // Get
    UInt(0),   // Hint
    UInt(0),   // AcquireBlock
    UInt(0)))( // AcquirePerm
    io.a.opcode)
  assert(op === opx)
  val select = Mux(io.write, UInt(1), op)

  // Only the masked bytes can be modified
  val selects = io.a.mask.asBools.map(b => Mux(b, select, UInt(0)))
  io.data_out := Cat(selects.zipWithIndex.map { case (s, i) =>
    val list = Seq(io.data_in, io.a.data, sum, logical).map(_((i + 1) * 8 - 1, i * 8))
    MuxLookup(s, 0.U, list.zipWithIndex.map { case (u, idx) => idx.U -> u })
  }.reverse)
}

class SourceDRequest(params: InclusiveCacheParameters) extends FullRequest(params)
{
  val sink = UInt(width = params.inner.bundle.sinkBits)
  val way  = UInt(width = params.wayBits)
  val bad  = Bool()
  val uncached_get = Bool()
  override def dump() = {
    DebugPrint(params, "SourceDRequest: prio: %x control: %b opcode: %x param: %x size: %x source: %x tag: %x set: %x offset: %x put: %x sink: %x way: %x bad: %b uncached_get: %b\n",
      prio.asUInt, control, opcode, param, size, source, tag, set, offset, put, sink, way, bad, uncached_get)
  }
}

class SourceDHazard(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val set = UInt(width = params.setBits)
  val way = UInt(width = params.wayBits)
  def dump() = {
    DebugPrint(params, "SourceDHazard: set: %x way: %x\n", set, way)
  }
}

class PutBufferACEntry(params: InclusiveCacheParameters) extends InclusiveCacheBundle(params)
{
  val data = UInt(width = params.inner.bundle.dataBits)
  val mask = UInt(width = params.inner.bundle.dataBits/8)
  val corrupt = Bool()
  def dump() = {
    DebugPrint(params, "PutBufferACEntry: data: %x mask: %x corrupt: %b\n", data, mask, corrupt)
  }
}

class SourceD(params: InclusiveCacheParameters) extends Module with HasTLDump
{
  val io = new Bundle {
    val req = Decoupled(new SourceDRequest(params)).flip
    val d = Decoupled(new TLBundleD(params.inner.bundle))
    // Put data from SinkA
    val pb_pop = Decoupled(new PutBufferPop(params))
    val pb_beat = new PutBufferAEntry(params).flip
    // Release data from SinkC
    // sourceD为什么要从sinkC这边拿数据呢？why？
    // put还好理解，这个就真的不太好理解了啊。
    val rel_pop  = Decoupled(new PutBufferPop(params))
    val rel_beat = new PutBufferCEntry(params).flip

    // sourceD为什么要从sinkC这边拿数据呢？why？
    // put还好理解，这个就真的不太好理解了啊。
    val gnt_pop  = Decoupled(new GrantBufferPop(params))
    val gnt_beat = new GrantBufferDEntry(params).flip

    // Access to the BankedStore
    val bs_radr = Decoupled(new BankedStoreInnerAddress(params))
    val bs_rdat = new BankedStoreInnerDecoded(params).flip
    // 所以对于put，amo之类的，数据写入是在这里完成的
    val bs_wadr = Decoupled(new BankedStoreInnerAddress(params))
    val bs_wdat = new BankedStoreInnerPoison(params)
    // Is it safe to evict/replace this way?
    // 具体这边是怎么同步的，还是不太明白
    val evict_req  = new SourceDHazard(params).flip
    val evict_safe = Bool()
    val grant_req  = new SourceDHazard(params).flip
    val grant_safe = Bool()
  }

  when (io.req.fire()) {
    DebugPrint(params, "SourceD req:")
    io.req.bits.dump()
  }

  /*
  when (io.d.fire()) {
    DebugPrint(params, "inner grant:")
    io.d.bits.dump
  }
  */

  when (io.pb_pop.fire()) {
    DebugPrint(params, "SourceD pb_pop:")
    io.pb_pop.bits.dump()
  }

  /*
  DebugPrint(params, "SourceD pb_beat:")
  io.pb_beat.dump()
  */

  when (io.rel_pop.fire()) {
    DebugPrint(params, "SourceD rel_pop:")
    io.rel_pop.bits.dump()
  }

  when (io.gnt_pop.fire()) {
    DebugPrint(params, "SourceD gnt_pop:")
    io.gnt_pop.bits.dump()
  }

  /*
  DebugPrint(params, "SourceD rel_beat:")
  io.rel_beat.dump()
  */

  when (io.bs_radr.fire()) {
    DebugPrint(params, "SourceD bs_radr:")
    io.bs_radr.bits.dump()
    printf("SourceD bs_radr:")
    io.bs_radr.bits.dumpx()
  }

  /*
  DebugPrint(params, "SourceD bs_rdat:")
  io.bs_rdat.dump()
  */


  when (io.bs_wadr.fire()) {
    DebugPrint(params, "SourceD bs_wadr:")
    io.bs_wadr.bits.dump()
  }

  /*
  DebugPrint(params, "SourceD bs_wdat:")
  io.bs_wdat.dump()
  */

 /*
  DebugPrint(params, "SourceD evict_req:")
  io.evict_req.dump()

  when (io.evict_safe) {
    DebugPrint(params, "SourceD evict_safe\n")
  }
  */

  /*
  DebugPrint(params, "SourceD grant_req:")
  io.grant_req.dump()
  */

  when (!io.grant_safe) {
    DebugPrint(params, "SourceD not grant_safe\n")
  }

  // 总线宽度，暂时不知道是对内还是对外
  val beatBytes = params.inner.manager.beatBytes
  // cache里面存储的粒度，最小的可读写的SRAM bank的宽度
  val writeBytes = params.micro.writeBytes

  val s1_valid = Wire(Bool())
  val s2_valid = Wire(Bool())
  val s3_valid = Wire(Bool())
  val s2_ready = Wire(Bool())
  val s3_ready = Wire(Bool())
  val s4_ready = Wire(Bool())

  // Acquire来的请求：
  // 请求                        配套的从D通道走的消息有
  // PutFullData Put 0           AccessAck 0
  // PutPartialData Put 1        AccessAck 0
  // ArithmeticData Atomic 2     AccessData 1
  // LogicalData Atomic 3        AccessData 1

  // Get Get 4                     AccessAckData 1
  // Intent Intent 5               HintAck       2
  // AcquireBlock Acquire 6        Grant 4 GrantData 5
  // AcquirePerm Acquire 7         Grant 4

  // C通道来的请求有：
  // ProbeAck Probe 4
  // ProbeAckData Probe 5
  // Release Release 6  ReleaseAck 6
  // ReleaseData Release 7 ReleaseAck   ReleaseAck 6

  // 所以会从SourceD走的message有：
  // AccessAck     0
  // AccessAckData 1
  // HintAck       2
  // Grant         4
  // GrantData     5
  // ReleaseAck    6


  ////////////////////////////////////// STAGE 1 //////////////////////////////////////
  // Reform the request beats

  // 所以这个其实就是阻塞的，一次只能处理一个请求
  // 显然s1在fire的当拍就启动了
  val busy = RegInit(Bool(false))
  val s1_block_r = RegInit(Bool(false))
  // 计数s1现在到了第几拍?
  val s1_counter = RegInit(UInt(0, width = params.innerBeatBits))
  // 这边又是一个锁存的经典案例
  val s1_req_reg = RegEnable(io.req.bits, !busy && io.req.valid)
  val s1_req = Mux(!busy, io.req.bits, s1_req_reg)

  // write bytes是sram存储的粒度，所以这个是要看每个sram block上是否有冲突？
  val s1_x_bypass = Wire(UInt(width = beatBytes/writeBytes)) // might go from high=>low during stall
  // 这个是什么意思？
  // 我们重点看这个什么时候变成true
  // s2_ready or
  // 这个条件着实有点奇怪
  // !(busy || io.req.valid)即!busy && !io.req.valid
  // 我感觉这个的意思是，s1不会被堵住，可以直接向下流
  val s1_latch_bypass = RegNext(!(busy || io.req.valid) || s2_ready)
  // 所以s1 bypass是被latch了，第一拍用s1 x latch
  val s1_bypass = Mux(s1_latch_bypass, s1_x_bypass, RegEnable(s1_x_bypass, s1_latch_bypass))

  // 这边的offset是啥，难道不应该直接就block对齐就可以了嘛？哪儿来的offset呢？
  // 估计是要处理下来的tilelink地址不对齐的问题？根据协议，addr应该只要按照size对齐就可以了
  // mask gen是根据这个地址，要读的mask
  // 然后s1 bypass是哪些数据可以bypass回来的mask
  // 与一下之后，假如全部可以bypass，那就不用读了
  val s1_mask = MaskGen(s1_req.offset, s1_req.size, beatBytes, writeBytes) & ~s1_bypass
  // 不需要任何操作，直接给response纠结可以的
  // 这边的opcode似乎不是d的opcode，而是直接把请求的opcode传到这里，再来判断
  val s1_grant = s1_req.opcode === AcquirePerm
  val s1_uncached_get = s1_req.opcode === Get && s1_req.uncached_get
  // 只有acquire是prio 0
  // Hint是不需要数据吗？
  // put full data也是走的A通道，最终也会走D
  // size小于writeBytes是在干啥？
  //  哪些请求需要read data array呢？
  //  首先是从A通道来的请求
  //  C通道来的请求就不需要读吗？是的，因为C通道是没有mask的，肯定是full masked write
  //  Hint是不需要读的，acquire假如是直接grant的，直接给权限那种，也是不需要读的
  //  剩下需要读的就是：
  //  Get，AcquireBlock（确实需要block的那种）,PutPartialData, ArithmeticData, LogicalData
  //  PutFullData也是可能需要读的，当Put full data要写的内容粒度太小，比最小写粒度还要小的话，就需要先读一下
  //  TODO: 在这里把miss的get给过滤掉
  val s1_need_r = s1_mask.orR && s1_req.prio(0) && s1_req.opcode =/= Hint && !s1_grant &&
                  (s1_req.opcode =/= PutFullData || s1_req.size < UInt(log2Ceil(writeBytes))) &&
                  !s1_uncached_get

  // valid这个信号是用来控制要不要读data array的
  // 所以这里是的几个条件是，首先在req.valid的当拍就可以开始读了
  // 另外就是busy就说明s1 stage还在working，那就要一直读
  // 不对，在req valid的当拍读，不应该是在fire的时候读吗？
  // 这个s1 block r应该是，假如后面s2那边没有及时读出来的数据处理了，那现在读出来的数据暂存在queue里面
  // 然后这一拍就不用重复读了
  val s1_valid_r = (busy || io.req.valid) && s1_need_r && !s1_block_r
  // s1_req.prio(0)即从A channel来的请求
  // 如果是从A channel来的，那就要看opcode 2
  // !s1_req.opcode(2)因为最高位是0的，都是write，包括了put，atomics
  // 请求还有可能是从C来的
  // 如果是从C来的,最后一位是1的，都是ProbeAckData和ReleaseData
  // TODO: 在这里加上miss的get
  val s1_need_pb = Mux(s1_req.prio(0), !s1_req.opcode(2) || s1_uncached_get, s1_req.opcode(0)) // hasData
  // 从A channel来的请求，只有Hint或者直接给权限的那种是一拍结束
  // C通道来的请求只有Release是一拍结束
  val s1_single = Mux(s1_req.prio(0), s1_req.opcode === Hint || s1_grant, s1_req.opcode === Release)
  // s1_retires是啥呢？
  val s1_retires = !s1_single // retire all operations with data in s3 for bypass (saves energy)
  // Alternatively: val s1_retires = s1_need_pb // retire only updates for bypass (less backpressure from WB)
  // 总共需要几beat
  val s1_beats1 = Mux(s1_single, UInt(0), UIntToOH1(s1_req.size, log2Up(params.cache.blockBytes)) >> log2Ceil(beatBytes))
  // 前面的是算出开始是第几拍，后面是配上现在传输到了第几拍
  val s1_beat = (s1_req.offset >> log2Ceil(beatBytes)) | s1_counter
  val s1_last = s1_counter === s1_beats1
  val s1_first = s1_counter === UInt(0)

  params.ccover(s1_block_r, "SOURCED_1_SRAM_HOLD", "SRAM read-out successful, but stalled by stage 2")
  params.ccover(!s1_latch_bypass, "SOURCED_1_BYPASS_HOLD", "Bypass match successful, but stalled by stage 2")
  params.ccover((busy || io.req.valid) && !s1_need_r, "SOURCED_1_NO_MODIFY", "Transaction servicable without SRAM")

  // 这边读数据要读多少拍呢？
  io.bs_radr.valid     := s1_valid_r
  io.bs_radr.bits.noop := Bool(false)
  io.bs_radr.bits.way  := s1_req.way
  io.bs_radr.bits.set  := s1_req.set
  io.bs_radr.bits.beat := s1_beat
  io.bs_radr.bits.mask := s1_mask

  params.ccover(io.bs_radr.valid && !io.bs_radr.ready, "SOURCED_1_READ_STALL", "Data readout stalled")

  // Make a queue to catch BS readout during stalls
  // 为什么是三项呢？
  // 这个是flow queue了，所以数据是可以从头拉到尾巴的
  val queue = Module(new Queue(io.bs_rdat, 3, flow=true))
  // banked data store是延迟两拍后出数
  queue.io.enq.valid := RegNext(RegNext(io.bs_radr.fire()))
  queue.io.enq.bits := io.bs_rdat
  assert (!queue.io.enq.valid || queue.io.enq.ready)

  params.ccover(!queue.io.enq.ready, "SOURCED_1_QUEUE_FULL", "Filled SRAM skidpad queue completely")

  // s1_block_r是用来干啥的呢？
  when (io.bs_radr.fire()) { s1_block_r := Bool(true) }
  when (io.req.valid) { busy := Bool(true) }
  when (s1_valid && s2_ready) {
    // 已经读了一拍了，开始读到下一拍
    s1_counter := s1_counter + UInt(1)
    s1_block_r := Bool(false)
    when (s1_last) {
      // 读到最后一拍了
      s1_counter := UInt(0)
      busy := Bool(false)
    }
  }

  // 只要当前还有请求在处理，就busy
  // 也就是第一拍req valid，然后设置了busy
  // 那我感觉还是能流水的，假如直接到了s1 last，那就busy一直是false了，就没有任何问题了
  params.ccover(s1_valid && !s2_ready, "SOURCED_1_STALL", "Stage 1 pipeline blocked")

  // 这边估计就是不看req fire，而是直接看req valid就可以了。估计是valid等待ready。只要valid有效了，就肯定fire
  io.req.ready := !busy
  // s1怎样算valid，busy，不需要读data或者可以读到banked store
  // s1 valid就是成功fire了一拍，只要fire了一拍就可以向下走了
  s1_valid := (busy || io.req.valid) && (!s1_valid_r || io.bs_radr.ready)

  when (s1_valid) {
    DebugPrint(params, "SourceD s1: busy: %x s1_uncached_get: %b s1_block_r: %x s1_counter: %x s1_req_source: %x s1_x_bypass: %x s1_latch_bypass: %x s1_bypass: %x s1_mask: %x s1_grant: %x s1_need_r: %x s1_valid_r: %x s1_need_pb: %x s1_single: %x s1_retires: %x s1_beats1: %x s1_beat: %x s1_last: %x s1_first: %x\n",
      busy, s1_uncached_get, s1_block_r, s1_counter, s1_req.source, s1_x_bypass, s1_latch_bypass, s1_bypass, s1_mask, s1_grant, s1_need_r, s1_valid_r, s1_need_pb, s1_single, s1_retires, s1_beats1, s1_beat, s1_last, s1_first)
  }
  when (s1_valid) {
    assert(!(s1_uncached_get && s1_bypass.orR))
  }


  ////////////////////////////////////// STAGE 2 //////////////////////////////////////
  // Fetch the request data
  // s2 latch指这一拍有数据从上面下来
  val s2_latch = s1_valid && s2_ready
  val s2_full = RegInit(Bool(false))
  val s2_valid_pb = RegInit(Bool(false))
  val s2_beat = RegEnable(s1_beat, s2_latch)
  val s2_bypass = RegEnable(s1_bypass, s2_latch)
  val s2_req = RegEnable(s1_req, s2_latch)
  val s2_last = RegEnable(s1_last, s2_latch)
  val s2_need_r = RegEnable(s1_need_r, s2_latch)
  val s2_need_pb = RegEnable(s1_need_pb, s2_latch)
  val s2_retires = RegEnable(s1_retires, s2_latch)
  val s2_need_d = RegEnable(!s1_need_pb || s1_first || s1_uncached_get, s2_latch)
  val s2_uncached_get = RegEnable(s1_uncached_get, s2_latch)
  val s2_pdata_raw = Wire(new PutBufferACEntry(params))
  val s2_pdata = s2_pdata_raw holdUnless s2_valid_pb
  val s2_uncached_get_param = io.gnt_beat.param holdUnless s2_valid_pb

  // 从SinkA, SinkC, SinkD那里把put buffer的值拿出来
  s2_pdata_raw.data    := Mux(s2_req.prio(0),
    Mux(s2_uncached_get, io.gnt_beat.data, io.pb_beat.data),
    io.rel_beat.data)

  // 只有sinkA那里来的数据有mask吗？
  s2_pdata_raw.mask    := Mux(s2_req.prio(0) && !s2_uncached_get, io.pb_beat.mask, ~UInt(0, width = params.inner.manager.beatBytes))
  s2_pdata_raw.corrupt := Mux(s2_req.prio(0),
    Mux(s2_uncached_get, Bool(false), io.pb_beat.corrupt),
    io.rel_beat.corrupt)

  // 拿完数据后，就释放一拍
  io.pb_pop.valid := s2_valid_pb && s2_req.prio(0) && !s2_uncached_get
  io.pb_pop.bits.index := s2_req.put
  io.pb_pop.bits.last  := s2_last

  io.gnt_pop.valid := s2_valid_pb && s2_req.prio(0) && s2_uncached_get
  io.gnt_pop.bits.index := s2_req.put
  io.gnt_pop.bits.beat  := s2_beat
  io.gnt_pop.bits.last  := s2_last

  io.rel_pop.valid := s2_valid_pb && !s2_req.prio(0)
  io.rel_pop.bits.index := s2_req.put
  io.rel_pop.bits.last  := s2_last

  params.ccover(io.pb_pop.valid && !io.pb_pop.ready, "SOURCED_2_PUTA_STALL", "Channel A put buffer was not ready in time")
  if (!params.firstLevel)
    params.ccover(io.rel_pop.valid && !io.rel_pop.ready, "SOURCED_2_PUTC_STALL", "Channel C put buffer was not ready in time")

  // pb ready又是啥？
  val pb_ready = Mux(s2_req.prio(0),
    Mux(s2_uncached_get, io.gnt_pop.ready, io.pb_pop.ready),
    io.rel_pop.ready)
  when (pb_ready) { s2_valid_pb := Bool(false) }
  when (s2_valid && s3_ready) { s2_full := Bool(false) }
  when (s2_latch) { s2_valid_pb := s1_need_pb }
  when (s2_latch) { s2_full := Bool(true) }

  params.ccover(s2_valid && !s3_ready, "SOURCED_2_STALL", "Stage 2 pipeline blocked")

  // 我猜测s2 valid就是指s2的数据可以向下传了
  s2_valid := s2_full && (!s2_valid_pb || pb_ready)
  // s2 ready是指s2现在可以接受请求了
  // s2 full指的是s2现在是否有请求
  // s2 latch就类似于是s2 fire
  // s2 valid pb是指s2是否要读pb的数据
  // 这边的ready的条件主要就是：
  // 1. 要么当前stage没有数据
  // 2. 要么是当前有数据，并且s3可以直接接受，即可以流水传下去
  // 另外就是当等待pb时，不能ready
  s2_ready := !s2_full || (s3_ready && (!s2_valid_pb || pb_ready))

  when (s2_valid) {
    DebugPrint(params, "SourceD s2: s2_latch: %x, s2_full: %x, s2_valid_pb: %x, s2_beat: %x, s2_bypass: %x, s2_req_source: %x, s2_last: %x, s2_need_r: %x, s2_need_pb: %x, s2_retires: %x, s2_need_d: %x, s2_pdata_raw_data: %x, s2_pdata_data: %x, s2_uncached_get: %x, s2_uncached_get_param: %x, s2_valid: %x, s2_ready: %x\n",
      s2_latch, s2_full, s2_valid_pb, s2_beat, s2_bypass, s2_req.source, s2_last, s2_need_r, s2_need_pb, s2_retires, s2_need_d, s2_pdata_raw.data, s2_pdata.data, s2_uncached_get, s2_uncached_get_param, s2_valid, s2_ready)
  }
  when (s2_valid) {
    assert(!(s2_uncached_get && s2_bypass.orR))
  }

  ////////////////////////////////////// STAGE 3 //////////////////////////////////////
  // Send D response
  // 在stage 3开始send response了

  // 这个latch就是fire的意思
  val s3_latch = s2_valid && s3_ready
  // 这个表明这一级是否有数据
  val s3_full = RegInit(Bool(false))
  val s3_valid_d = RegInit(Bool(false))
  val s3_beat = RegEnable(s2_beat, s3_latch)
  val s3_bypass = RegEnable(s2_bypass, s3_latch)
  val s3_req = RegEnable(s2_req, s3_latch)
  val s3_adjusted_opcode = Mux(s3_req.bad, Get, s3_req.opcode) // kill update when denied
  val s3_last = RegEnable(s2_last, s3_latch)
  val s3_pdata = RegEnable(s2_pdata, s3_latch)
  val s3_need_pb = RegEnable(s2_need_pb, s3_latch)
  val s3_retires = RegEnable(s2_retires, s3_latch)
  val s3_need_r = RegEnable(s2_need_r, s3_latch)
  val s3_uncached_get = RegEnable(s2_uncached_get, s3_latch)
  // 这个need bs，估计意思是要不要写data array？
  val s3_need_bs = s3_need_pb && !s3_uncached_get
  val s3_acq = s3_req.opcode === AcquireBlock || s3_req.opcode === AcquirePerm

  // 这个就是个流水线，所以得处理数据的bypass
  // Collect s3's data from either the BankedStore or bypass
  // NOTE: we use the s3_bypass passed down from s1_bypass, because s2-s4 were guarded by the hazard checks and not stale
  val s3_bypass_data = Wire(UInt())
  def chunk(x: UInt): Seq[UInt] = Seq.tabulate(beatBytes/writeBytes) { i => x((i+1)*writeBytes*8-1, i*writeBytes*8) }
  def chop (x: UInt): Seq[Bool] = Seq.tabulate(beatBytes/writeBytes) { i => x(i) }
  def bypass(sel: UInt, x: UInt, y: UInt) =
    (chop(sel) zip (chunk(x) zip chunk(y))) .map { case (s, (x, y)) => Mux(s, x, y) } .asUInt
  val s3_rdata = Mux(s3_uncached_get, s3_pdata.data, bypass(s3_bypass, s3_bypass_data, queue.io.deq.bits.data))
  val s3_uncached_get_param = RegEnable(s2_uncached_get_param, s3_latch)

  // Lookup table for response codes
  // val grant = Mux(s3_req.param === BtoT, Grant, GrantData)
  // for BtoT, always returns GrantData
  // L1DCache depend on this feature
  val grant = GrantData
  val resp_opcode = Vec(Seq(AccessAck, AccessAck, AccessAckData, AccessAckData, AccessAckData, HintAck, grant, Grant))

  // No restrictions on the type of buffer used here
  val d = Wire(io.d)
  io.d <> params.micro.innerBuf.d(d)

  // s3 valid d，s3现在放到d上的message是否是valid
  d.valid := s3_valid_d
  d.bits.opcode  := Mux(s3_req.prio(0), resp_opcode(s3_req.opcode), ReleaseAck)
  d.bits.param   := Mux(s3_req.prio(0) && s3_acq,
    Mux(s3_req.param =/= NtoB, toT, toB),
    if (params.verification) Mux(s3_uncached_get, s3_uncached_get_param, UInt(0))
    else UInt(0))
  d.bits.size    := s3_req.size
  d.bits.source  := s3_req.source
  d.bits.sink    := s3_req.sink
  d.bits.denied  := s3_req.bad
  d.bits.data    := s3_rdata
  d.bits.corrupt := s3_req.bad && d.bits.opcode(0)

  when (s3_valid) {
    DebugPrint(params, "SourceD s3: s3_latch: %x, s3_full: %x, s3_valid_d: %x, s3_uncached_get: %x, s3_uncached_get_param: %x, s3_beat: %x, s3_bypass: %x, s3_req_source: %x, s3_adjusted_opcode: %x, s3_last: %x, s3_pdata_data: %x, s3_need_pb: %x, s3_retires: %x, s3_need_r: %x, s3_need_bs: %x, s3_acq: %x, s3_bypass_data: %x, s3_rdata: %x, grant: %x\n",
      s3_latch, s3_full, s3_valid_d, s3_uncached_get, s3_uncached_get_param, s3_beat, s3_bypass, s3_req.source, s3_adjusted_opcode, s3_last, s3_pdata.data, s3_need_pb, s3_retires, s3_need_r, s3_need_bs, s3_acq, s3_bypass_data, s3_rdata, grant)
  }
  when (s3_valid) {
    assert(!(s3_uncached_get && s3_bypass.orR))
  }

  // 数据是从queue里面出来的
  queue.io.deq.ready := s3_valid && s4_ready && s3_need_r
  assert (!s3_full || !s3_need_r || queue.io.deq.valid)

  when (d.ready) { s3_valid_d := Bool(false) }
  when (s3_valid && s4_ready) { s3_full := Bool(false) }
  when (s3_latch) { s3_valid_d := s2_need_d }
  when (s3_latch) { s3_full := Bool(true) }

  params.ccover(s3_valid && !s4_ready, "SOURCED_3_STALL", "Stage 3 pipeline blocked")

  // s3 valid意味着处理完这一级的了，可以向下passdown了
  s3_valid := s3_full && (!s3_valid_d || d.ready)
  s3_ready := !s3_full || (s4_ready && (!s3_valid_d || d.ready))

  ////////////////////////////////////// STAGE 4 //////////////////////////////////////
  // Writeback updated data

  val s4_latch = s3_valid && s3_retires && s4_ready
  val s4_full = RegInit(Bool(false))
  val s4_beat = RegEnable(s3_beat, s4_latch)
  val s4_need_r = RegEnable(s3_need_r, s4_latch)
  val s4_uncached_get = RegEnable(s3_uncached_get, s4_latch)
  val s4_need_bs = RegEnable(s3_need_bs, s4_latch)
  val s4_need_pb = RegEnable(s3_need_pb, s4_latch)
  val s4_req = RegEnable(s3_req, s4_latch)
  val s4_adjusted_opcode = RegEnable(s3_adjusted_opcode, s4_latch)
  val s4_pdata = RegEnable(s3_pdata, s4_latch)
  val s4_rdata = RegEnable(s3_rdata, s4_latch)

  when (s4_full) {
    DebugPrint(params, "SourceD s4: s4_beat: %x, s4_need_r: %x, s4_uncached_get: %x, s4_need_bs: %x, s4_need_pb: %x, s4_adjusted_opcode: %x, s4_pdata_data: %x, s4_rdata: %x\n",
      s4_beat, s4_need_r, s4_uncached_get, s4_need_bs, s4_need_pb, s4_adjusted_opcode, s4_pdata.data, s4_rdata)
    DebugPrint(params, "SourceD s4: prio: %x control: %b opcode: %x param: %x size: %x source: %x tag: %x set: %x offset: %x put: %x sink: %x way: %x\n",
      s4_req.prio.asUInt, s4_req.control, s4_req.opcode, s4_req.param, s4_req.size, s4_req.source, s4_req.tag, s4_req.set, s4_req.offset, s4_req.put, s4_req.sink, s4_req.way)
  }

  val atomics = Module(new AtomicsLocal(params.inner.bundle))
  atomics.io.write     := s4_req.prio(2)
  atomics.io.a.opcode  := s4_adjusted_opcode
  atomics.io.a.param   := s4_req.param
  atomics.io.a.size    := UInt(0)
  atomics.io.a.source  := UInt(0)
  atomics.io.a.address := UInt(0)
  atomics.io.a.mask    := s4_pdata.mask
  atomics.io.a.data    := s4_pdata.data
  atomics.io.data_in   := s4_rdata
  val overwrite = s4_req.prio(2) || (s4_adjusted_opcode <= TLMessages.PutPartialData)
  val masks = Cat(s4_pdata.mask.asBools.map(b => Fill(8, b)).reverse)
  val atomics_io_data_out: UInt = Mux(overwrite, (s4_pdata.data & masks) | (s4_rdata & ~masks), s4_rdata)
  assert(!s4_full || (atomics.io.data_out === atomics_io_data_out),
    """write %b
      |opcode %d
      |param %d
      |mask %b
      |myms %x
      |data %x
      |data_in %x
      |data_out %x
      |my___out %x""".stripMargin,
      s4_req.prio(2),
      s4_adjusted_opcode,
      s4_req.param,
      s4_pdata.mask,
      masks,
      s4_pdata.data,
      s4_rdata,
      atomics.io.data_out,
      atomics_io_data_out)

  io.bs_wadr.valid := s4_full && s4_need_bs
  io.bs_wadr.bits.noop := Bool(false)
  io.bs_wadr.bits.way  := s4_req.way
  io.bs_wadr.bits.set  := s4_req.set
  io.bs_wadr.bits.beat := s4_beat
  io.bs_wadr.bits.mask := Cat(s4_pdata.mask.asBools.grouped(writeBytes).map(_.reduce(_||_)).toList.reverse)
  io.bs_wdat.data := atomics_io_data_out
  assert (!(s4_full && s4_need_pb && s4_pdata.corrupt), "Data poisoning unsupported")

  params.ccover(io.bs_wadr.valid && !TrackWire(io.bs_wadr.ready), "SOURCED_4_WRITEBACK_STALL", "Data writeback stalled")
  params.ccover(s4_req.prio(0) && s4_req.opcode === ArithmeticData && s4_req.param === MIN,  "SOURCED_4_ATOMIC_MIN",  "Evaluated a signed minimum atomic")
  params.ccover(s4_req.prio(0) && s4_req.opcode === ArithmeticData && s4_req.param === MAX,  "SOURCED_4_ATOMIC_MAX",  "Evaluated a signed maximum atomic")
  params.ccover(s4_req.prio(0) && s4_req.opcode === ArithmeticData && s4_req.param === MINU, "SOURCED_4_ATOMIC_MINU", "Evaluated an unsigned minimum atomic")
  params.ccover(s4_req.prio(0) && s4_req.opcode === ArithmeticData && s4_req.param === MAXU, "SOURCED_4_ATOMIC_MAXU", "Evaluated an unsigned minimum atomic")
  params.ccover(s4_req.prio(0) && s4_req.opcode === ArithmeticData && s4_req.param === ADD,  "SOURCED_4_ATOMIC_ADD",  "Evaluated an addition atomic")
  params.ccover(s4_req.prio(0) && s4_req.opcode === LogicalData    && s4_req.param === XOR,  "SOURCED_4_ATOMIC_XOR",  "Evaluated a bitwise XOR atomic")
  params.ccover(s4_req.prio(0) && s4_req.opcode === LogicalData    && s4_req.param === OR,   "SOURCED_4_ATOMIC_OR",   "Evaluated a bitwise OR atomic")
  params.ccover(s4_req.prio(0) && s4_req.opcode === LogicalData    && s4_req.param === AND,  "SOURCED_4_ATOMIC_AND",  "Evaluated a bitwise AND atomic")
  params.ccover(s4_req.prio(0) && s4_req.opcode === LogicalData    && s4_req.param === SWAP, "SOURCED_4_ATOMIC_SWAP", "Evaluated a bitwise SWAP atomic")

  when (TrackWire(io.bs_wadr.ready) || !s4_need_bs) { s4_full := Bool(false) }
  when (s4_latch) { s4_full := Bool(true) }

  s4_ready := !s3_retires || !s4_full || TrackWire(io.bs_wadr.ready) || !s4_need_bs

  ////////////////////////////////////// RETIRED //////////////////////////////////////

  // Record for bypass the last three retired writebacks
  // We need 3 slots to collect what was in s2, s3, s4 when the request was in s1
  // ... you can't rely on s4 being full if bubbles got introduced between s1 and s2
  val retire = s4_full && (TrackWire(io.bs_wadr.ready) || !s4_need_bs)
  when (retire) {
    DebugPrint(params, "SourceD retire\n")
  }

  val s5_req  = RegEnable(s4_req,  retire)
  val s5_beat = RegEnable(s4_beat, retire)
  val s5_dat  = RegEnable(atomics_io_data_out, retire)
  val s5_uncached_get  = RegEnable(s4_uncached_get, retire)

  val s6_req  = RegEnable(s5_req,  retire)
  val s6_beat = RegEnable(s5_beat, retire)
  val s6_dat  = RegEnable(s5_dat,  retire)
  val s6_uncached_get  = RegEnable(s5_uncached_get, retire)

  val s7_dat  = RegEnable(s6_dat,  retire)
  val s7_uncached_get  = RegEnable(s6_uncached_get, retire)

  when (RegNext(retire)) {
    DebugPrint(params, "SourceD s5: prio: %x control: %b opcode: %x param: %x size: %x source: %x tag: %x set: %x offset: %x put: %x sink: %x way: %x beat: %d data: %x uncached_get: %b\n",
      s5_req.prio.asUInt, s5_req.control, s5_req.opcode, s5_req.param, s5_req.size, s5_req.source, s5_req.tag, s5_req.set, s5_req.offset, s5_req.put, s5_req.sink, s5_req.way, s5_beat, s5_dat, s5_uncached_get)

    DebugPrint(params, "SourceD s6: prio: %x control: %b opcode: %x param: %x size: %x source: %x tag: %x set: %x offset: %x put: %x sink: %x way: %x beat: %d data: %x uncached_get: %b\n",
      s6_req.prio.asUInt, s6_req.control, s6_req.opcode, s6_req.param, s6_req.size, s6_req.source, s6_req.tag, s6_req.set, s6_req.offset, s6_req.put, s6_req.sink, s6_req.way, s6_beat, s6_dat, s6_uncached_get)

    DebugPrint(params, "SourceD s7: data: %x uncached_get: %b\n",
      s7_dat, s7_uncached_get)
  }

  ////////////////////////////////////// BYPASSS //////////////////////////////////////
  // 这边的bypass是在哪些级之间进行bypass啊？

  // 这句注释啥意思？
  // Manually retime this circuit to pull a register stage forward
  // 这边pre的意思是要进s3但是还没进s3的req？
  // 这边为啥要用pre呢？为啥不能直接拉寄存器过来比呢？
  // 这边写的retime？是因为时序原因吗？why？
  val pre_s3_req  = Mux(s3_latch, s2_req,  s3_req)
  val pre_s4_req  = Mux(s4_latch, s3_req,  s4_req)
  val pre_s5_req  = Mux(retire,   s4_req,  s5_req)
  val pre_s6_req  = Mux(retire,   s5_req,  s6_req)

  val pre_s3_beat = Mux(s3_latch, s2_beat, s3_beat)
  val pre_s4_beat = Mux(s4_latch, s3_beat, s4_beat)
  val pre_s5_beat = Mux(retire,   s4_beat, s5_beat)
  val pre_s6_beat = Mux(retire,   s5_beat, s6_beat)

  val pre_s3_uncached_get  = Mux(s3_latch, s2_uncached_get,  s3_uncached_get)
  val pre_s4_uncached_get  = Mux(s4_latch, s3_uncached_get,  s4_uncached_get)
  val pre_s5_uncached_get  = Mux(retire,   s4_uncached_get,  s5_uncached_get)
  val pre_s6_uncached_get  = Mux(retire,   s5_uncached_get,  s6_uncached_get)

  val pre_s5_dat  = Mux(retire,   atomics_io_data_out, s5_dat)
  val pre_s6_dat  = Mux(retire,   s5_dat,  s6_dat)
  val pre_s7_dat  = Mux(retire,   s6_dat,  s7_dat)
  val pre_s4_full = s4_latch || (!(TrackWire(io.bs_wadr.ready) || !s4_need_bs) && s4_full)

  // TODO: change this
  // 这边的bypass到底是怎么搞的啊？
  // 我还有点看不太懂
  // 这里的bypass的data是从4,5,6,7级bypass到s3
  // 然后标记bypass信号，是检查s1与2,3,4，这个我还不知道是为啥
  // 反正我们不管三七二十一，只要是uncached get，就不进行任何数据bypass
  val pre_s3_4_match  = pre_s4_req.set === pre_s3_req.set && pre_s4_req.way === pre_s3_req.way && pre_s4_beat === pre_s3_beat && pre_s4_full && !pre_s3_uncached_get && !pre_s4_uncached_get
  val pre_s3_5_match  = pre_s5_req.set === pre_s3_req.set && pre_s5_req.way === pre_s3_req.way && pre_s5_beat === pre_s3_beat && !pre_s3_uncached_get && !pre_s5_uncached_get
  val pre_s3_6_match  = pre_s6_req.set === pre_s3_req.set && pre_s6_req.way === pre_s3_req.way && pre_s6_beat === pre_s3_beat && !pre_s3_uncached_get && !pre_s6_uncached_get



  val pre_s3_4_bypass = Mux(pre_s3_4_match, MaskGen(pre_s4_req.offset, pre_s4_req.size, beatBytes, writeBytes), UInt(0))
  val pre_s3_5_bypass = Mux(pre_s3_5_match, MaskGen(pre_s5_req.offset, pre_s5_req.size, beatBytes, writeBytes), UInt(0))
  val pre_s3_6_bypass = Mux(pre_s3_6_match, MaskGen(pre_s6_req.offset, pre_s6_req.size, beatBytes, writeBytes), UInt(0))


  s3_bypass_data :=
    bypass(RegNext(pre_s3_4_bypass), atomics_io_data_out, RegNext(
    bypass(pre_s3_5_bypass, pre_s5_dat,
    bypass(pre_s3_6_bypass, pre_s6_dat,
                            pre_s7_dat))))

  // Detect which parts of s1 will be bypassed from later pipeline stages (s1-s4)
  // Note: we also bypass from reads ahead in the pipeline to save power
  // TODO: change this
  val s1_2_match  = s2_req.set === s1_req.set && s2_req.way === s1_req.way && s2_beat === s1_beat && s2_full && s2_retires && !s1_uncached_get && !s2_uncached_get
  val s1_3_match  = s3_req.set === s1_req.set && s3_req.way === s1_req.way && s3_beat === s1_beat && s3_full && s3_retires && !s1_uncached_get && !s3_uncached_get
  val s1_4_match  = s4_req.set === s1_req.set && s4_req.way === s1_req.way && s4_beat === s1_beat && s4_full && !s1_uncached_get && !s4_uncached_get

  for (i <- 0 until 8) {
    val cover = UInt(i)
    val s2 = s1_2_match === cover(0)
    val s3 = s1_3_match === cover(1)
    val s4 = s1_4_match === cover(2)
    params.ccover(io.req.valid && s2 && s3 && s4, "SOURCED_BYPASS_CASE_" + i, "Bypass data from all subsets of pipeline stages")
  }

  val s1_2_bypass = Mux(s1_2_match, MaskGen(s2_req.offset, s2_req.size, beatBytes, writeBytes), UInt(0))
  val s1_3_bypass = Mux(s1_3_match, MaskGen(s3_req.offset, s3_req.size, beatBytes, writeBytes), UInt(0))
  val s1_4_bypass = Mux(s1_4_match, MaskGen(s4_req.offset, s4_req.size, beatBytes, writeBytes), UInt(0))

  s1_x_bypass := s1_2_bypass | s1_3_bypass | s1_4_bypass

  ////////////////////////////////////// HAZARDS //////////////////////////////////////

  // SinkC, SourceC, and SinkD can never interfer with each other because their operation
  // is fully contained with an execution plan of an MSHR. That MSHR owns the entire set, so
  // there is no way for a data race.

  // However, SourceD is special. We allow it to run ahead after the MSHR and scheduler have
  // released control of a set+way. This is necessary to allow single cycle occupancy for
  // hits. Thus, we need to be careful about data hazards between SourceD and the other ports
  // of the BankedStore. We can at least compare to registers 's1_req_reg', because the first
  // cycle of SourceD falls within the occupancy of the MSHR's plan.

  // Must ReleaseData=> be interlocked? RaW hazard
  io.evict_safe :=
    (!busy    || io.evict_req.way =/= s1_req_reg.way || io.evict_req.set =/= s1_req_reg.set) &&
    (!s2_full || io.evict_req.way =/= s2_req.way     || io.evict_req.set =/= s2_req.set) &&
    (!s3_full || io.evict_req.way =/= s3_req.way     || io.evict_req.set =/= s3_req.set) &&
    (!s4_full || io.evict_req.way =/= s4_req.way     || io.evict_req.set =/= s4_req.set)

  // Must =>GrantData be interlocked? WaR hazard
  io.grant_safe :=
    (!busy    || io.grant_req.way =/= s1_req_reg.way || io.grant_req.set =/= s1_req_reg.set) &&
    (!s2_full || io.grant_req.way =/= s2_req.way     || io.grant_req.set =/= s2_req.set) &&
    (!s3_full || io.grant_req.way =/= s3_req.way     || io.grant_req.set =/= s3_req.set) &&
    (!s4_full || io.grant_req.way =/= s4_req.way     || io.grant_req.set =/= s4_req.set)

  // SourceD cannot overlap with SinkC b/c the only way inner caches could become
  // dirty such that they want to put data in via SinkC is if we Granted them permissions,
  // which must flow through the SourecD pipeline.
}
