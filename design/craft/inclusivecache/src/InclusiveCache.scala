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

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._

import freechips.rocketchip.subsystem.{SubsystemBankedCoherenceKey}
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import midas.targetutils.SynthesizePrintf
import freechips.rocketchip.util._
import freechips.rocketchip.subsystem.ExtMem


class InclusiveCache(
  val cache: CacheParameters,
  val micro: InclusiveCacheMicroParameters,
  control: Option[InclusiveCacheControlParameters] = None
  )(implicit p: Parameters)
    extends LazyModule
{
  val access = TransferSizes(1, cache.blockBytes)
  val xfer = TransferSizes(cache.blockBytes, cache.blockBytes)
  val atom = TransferSizes(1, cache.beatBytes)

  var resourcesOpt: Option[ResourceBindings] = None
   val device2 : SimpleDevice = new SimpleDevice("cache-pmu", Seq("sifive,inclusivecache0", "cache"))
  val device: SimpleDevice = new SimpleDevice("cache-controller", Seq("sifive,inclusivecache0", "cache")) {
    def ofInt(x: Int) = Seq(ResourceInt(BigInt(x)))

    override def describe(resources: ResourceBindings): Description = {
      resourcesOpt = Some(resources)

      val Description(name, mapping) = super.describe(resources)
      // Find the outer caches
      val outer = node.edges.out
        .flatMap(_.manager.managers)
        .filter(_.supportsAcquireB)
        .flatMap(_.resources.headOption)
        .map(_.owner.label)
        .distinct
      val nextlevel: Option[(String, Seq[ResourceValue])] =
        if (outer.isEmpty) {
          None
        } else {
          Some("next-level-cache" -> outer.map(l => ResourceReference(l)).toList)
        }

      val extra = Map(
        "cache-level"            -> ofInt(2),
        "cache-unified"          -> Nil,
        "cache-size"             -> ofInt(cache.sizeBytes * node.edges.in.size),
        "cache-sets"             -> ofInt(cache.sets * node.edges.in.size),
        "cache-block-size"       -> ofInt(cache.blockBytes),
        "sifive,mshr-count"      -> ofInt(InclusiveCacheParameters.all_mshrs(cache, micro)))
      Description(name, mapping ++ extra ++ nextlevel)
    }
  }

  val node: TLAdapterNode = TLAdapterNode(
    clientFn  = { _ => TLClientPortParameters(Seq(TLClientParameters(
      name          = s"L${cache.level} InclusiveCache",
      sourceId      = IdRange(0, InclusiveCacheParameters.out_mshrs(cache, micro)),
      supportsProbe = xfer)))
    },
    managerFn = { m => TLManagerPortParameters(
      managers = m.managers.map { m => m.copy(
        regionType         = if (m.regionType >= RegionType.UNCACHED) RegionType.CACHED else m.regionType,
        resources          = Resource(device, "caches") +: m.resources,
        supportsAcquireB   = xfer,
        supportsAcquireT   = if (m.supportsAcquireT) xfer else TransferSizes.none,
        supportsArithmetic = if (m.supportsAcquireT) atom else TransferSizes.none,
        supportsLogical    = if (m.supportsAcquireT) atom else TransferSizes.none,
        supportsGet        = access,
        supportsPutFull    = if (m.supportsAcquireT) access else TransferSizes.none,
        supportsPutPartial = if (m.supportsAcquireT) access else TransferSizes.none,
        supportsHint       = access,
        alwaysGrantsT      = false,
        fifoId             = None)
      },
      beatBytes  = cache.beatBytes,
      endSinkId  = InclusiveCacheParameters.all_mshrs(cache, micro),
      minLatency = 2)
    })
          val ctlnode = control.map { c => TLRegisterNode(
    address     = Seq(AddressSet(c.address + 0x100000, 0xfff)),
    device      = device2,
    concurrency = 1, // Only one flush at a time (else need to track who answers)
    beatBytes   = c.beatBytes)}

  val ctrls = control.map { c =>
    val nCtrls = if (c.bankedControl) p(SubsystemBankedCoherenceKey).nBanks else 1
    Seq.tabulate(nCtrls) { i => LazyModule(new InclusiveCacheControl(this,
      c.copy(address = c.address + i * InclusiveCacheParameters.L2ControlSize))) }
  }.getOrElse(Nil)

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    // If you have a control port, you must have at least one cache port
    require (ctrls.isEmpty || !node.edges.in.isEmpty)
     val io = IO(new Bundle {
      // DTU Interface
      val DTU_DirectoryIOIn = Flipped(Valid(UInt(48.W)))
      val DTU_DirectoryIOOut = Valid(Bool())
    })



    // Extract the client IdRanges; must be the same on all ports!
    val clientIds = node.edges.in.headOption.map(_.client.clients.map(_.sourceId).sortBy(_.start))
    node.edges.in.foreach { e => require(e.client.clients.map(_.sourceId).sortBy(_.start) == clientIds.get) }

    // Use the natural ordering of clients (just like in Directory)
    node.edges.in.headOption.foreach { n =>
      println(s"L${cache.level} InclusiveCache Client Map:")
      n.client.clients.zipWithIndex.foreach { case (c,i) =>
        println(s"\t${i} <= ${c.name}")
      }
      println("")
    }

    val nBanks = p(SubsystemBankedCoherenceKey).nBanks
      val membase =  p(ExtMem).get.master.base
    val PerBankl1Refill = Seq.fill(nBanks)(RegInit((0.U(64.W))))
    val PerBankl1Writeback = Seq.fill(nBanks)(RegInit((0.U(64.W))))
    val PerBankl2Refill = Seq.fill(nBanks)(RegInit((0.U(64.W))))
    val PerBankl2Writeback = Seq.fill(nBanks)(RegInit((0.U(64.W))))
    val Totall1Refill = RegInit((0.U(64.W)))
    val Totall1Writeback = RegInit((0.U(64.W)))
    val Totall2Refill = RegInit((0.U(64.W)))
    val Totall2Writeback = RegInit((0.U(64.W)))



    val TotalL1RefillRegs     = Seq((0x600) -> Seq(RegField(Totall1Refill.getWidth, Totall1Refill, RegFieldDesc("totalmisscount", "total misscount"))))
    val TotalL1WritebackRegs  = Seq((0x608) -> Seq(RegField(Totall1Writeback.getWidth, Totall1Writeback, RegFieldDesc("totalaccesscount", "total accesscount"))))
    val TotalL2RefillRegs     = Seq((0x610) -> Seq(RegField(Totall2Refill.getWidth, Totall2Refill, RegFieldDesc("totalmisscount", "total misscount"))))
    val TotalL2WritebackRegs  = Seq((0x618) -> Seq(RegField(Totall2Writeback.getWidth, Totall2Writeback, RegFieldDesc("totalaccesscount", "total accesscount"))))

    Totall1Refill := VecInit(PerBankl1Refill).reduce(_+_)
    Totall1Writeback := VecInit(PerBankl1Writeback).reduce(_+_)
    Totall2Refill := VecInit(PerBankl2Refill).reduce(_+_)
    Totall2Writeback := VecInit(PerBankl2Writeback).reduce(_+_)

    val mmreg =  TotalL1RefillRegs ++ TotalL1WritebackRegs ++ TotalL2RefillRegs ++ TotalL2WritebackRegs


    val regmap = ctlnode.map{ c =>
      c.regmap(mmreg: _*)
    }

    // Create the L2 Banks
    val mods = (node.in zip node.out).zipWithIndex map { case (((in, edgeIn), (out, edgeOut)), i) =>
      edgeOut.manager.managers.foreach { m =>
        require (m.supportsAcquireB.contains(xfer),
          s"All managers behind the L2 must support acquireB($xfer) " +
          s"but ${m.name} only supports (${m.supportsAcquireB})!")
        if (m.supportsAcquireT) require (m.supportsAcquireT.contains(xfer),
          s"Any probing managers behind the L2 must support acquireT($xfer) " +
          s"but ${m.name} only supports (${m.supportsAcquireT})!")
      }

      val params = InclusiveCacheParameters(cache, micro, !ctrls.isEmpty, edgeIn, edgeOut)
      val scheduler = Module(new InclusiveCacheBankScheduler(params)).suggestName("inclusive_cache_bank_sched")
      when (in.a.fire)
      {
        //SynthesizePrintf("(LLC) Received request 0x%x source %d\n", in.a.bits.address, in.a.bits.source)
      }
      scheduler.io.in <> in
      out <> scheduler.io.out
      scheduler.io.ways := DontCare
      scheduler.io.divs := DontCare

      // Tie down default values in case there is no controller
      scheduler.io.req.valid := false.B
      scheduler.io.req.bits.address := 0.U
      scheduler.io.req.bits.invalidate := false.B
      scheduler.io.resp.ready := true.B

      // DTU Interface
      scheduler.io.DTU_DirectoryIOIn.bits := io.DTU_DirectoryIOIn.bits
      scheduler.io.DTU_DirectoryIOIn.valid := io.DTU_DirectoryIOIn.valid
      io.DTU_DirectoryIOOut.valid := scheduler.io.DTU_DirectoryIOOut.valid
      io.DTU_DirectoryIOOut.bits := scheduler.io.DTU_DirectoryIOOut.bits


 


      // Fix-up the missing addresses. We do this here so that the Scheduler can be
      // deduplicated by Firrtl to make hierarchical place-and-route easier.
      out.a.bits.address := params.restoreAddress(scheduler.io.out.a.bits.address)
      in .b.bits.address := params.restoreAddress(scheduler.io.in .b.bits.address)
      out.c.bits.address := params.restoreAddress(scheduler.io.out.c.bits.address)

      val aIsAcquire = in.a.bits.opcode === TLMessages.AcquireBlock
      val aIsInstFetch = in.a.bits.opcode === TLMessages.Get && in.a.bits.address >= membase.U
      val aIsRead = aIsAcquire || aIsInstFetch
      val aIsWrite = (in.a.bits.opcode === TLMessages.PutFullData || in.a.bits.opcode === TLMessages.PutPartialData) && in.a.bits.address >= membase.U
      val cIsWb = in.c.bits.opcode === TLMessages.ReleaseData || in.c.bits.opcode === TLMessages.ProbeAckData
      val outaIsAcquire = scheduler.io.out.a.bits.opcode === TLMessages.AcquireBlock
      val outaIsInstFetch = scheduler.io.out.a.bits.opcode === TLMessages.Get && scheduler.io.out.a.bits.address >= membase.U

      //val toDRAM = (isMiss || isWbToDRAM
      val outCIsWb =  (out.c.bits.opcode === TLMessages.ReleaseData || out.c.bits.opcode === TLMessages.ProbeAckData)
      val outAIsWrite = (out.a.bits.opcode === TLMessages.PutFullData || out.a.bits.opcode === TLMessages.PutPartialData)
      val outAIsRead = (out.a.bits.opcode === TLMessages.Get || out.a.bits.opcode === TLMessages.AcquireBlock)

      val IsToDRAMRead = (out.a.fire && edgeOut.first(out.a) && outAIsRead) //|| (out.c.fire && edgeOut.first(out.c))
      val IsToDRAMWrite = (out.a.fire && edgeOut.first(out.a) &&  outAIsWrite) || (out.c.fire && edgeOut.first(out.c) && outCIsWb)

      val l1EvictWb = in.c.bits.opcode === TLMessages.ReleaseData
      val l1ProbeWb = in.c.bits.opcode === TLMessages.ProbeAckData
      val inCIsWb =    (l1EvictWb || l1ProbeWb) // writes to LLC
      val inAIsWrite = (in.a.bits.opcode === TLMessages.PutFullData || in.a.bits.opcode === TLMessages.PutPartialData)
      val inAIsRead =  (in.a.bits.opcode === TLMessages.Get || in.a.bits.opcode === TLMessages.AcquireBlock)

      val l1Refill = in.d.fire && edgeIn.first(in.d) && (in.d.bits.opcode === TLMessages.GrantData)
      val l1WriteBack = (in.c.fire && edgeIn.first(in.c) && (inCIsWb))


      val l2Refill = out.a.fire && (out.a.bits.opcode === TLMessages.AcquireBlock) //out.d.fire && edgeOut.first(out.d) && (out.d.bits.opcode === TLMessages.GrantData)
      val l2WriteBack = (out.c.fire && edgeOut.first(out.c)) && (out.c.bits.opcode === TLMessages.ReleaseData)


      PerBankl1Refill(i) := PerBankl1Refill(i)  + l1Refill.asUInt
      PerBankl2Refill(i) := PerBankl2Refill(i)  + l2Refill.asUInt
      PerBankl1Writeback(i) := PerBankl1Writeback(i)  + l1WriteBack.asUInt
      PerBankl2Writeback(i) := PerBankl2Writeback(i)  + l2WriteBack.asUInt

      scheduler
    }

    ctrls.foreach { ctrl =>
      ctrl.module.io.flush_req.ready := false.B
      ctrl.module.io.flush_resp := false.B
      ctrl.module.io.flush_match := false.B
    }

    mods.zip(node.edges.in).zipWithIndex.foreach { case ((sched, edgeIn), i) =>
      val ctrl = if (ctrls.size > 1) Some(ctrls(i)) else ctrls.headOption
      ctrl.foreach { ctrl => {
        val contained = edgeIn.manager.managers.flatMap(_.address)
          .map(_.contains(ctrl.module.io.flush_req.bits)).reduce(_||_)
        when (contained) { ctrl.module.io.flush_match := true.B }

        sched.io.req.valid := contained && ctrl.module.io.flush_req.valid
        sched.io.req.bits.address := ctrl.module.io.flush_req.bits
        sched.io.req.bits.invalidate := ctrl.module.io.invalidate_req
        when (contained && sched.io.req.ready) { ctrl.module.io.flush_req.ready := true.B }

        when (sched.io.resp.valid) { ctrl.module.io.flush_resp := true.B }
        sched.io.resp.ready := true.B
      }}
    }

    def json = s"""{"banks":[${mods.map(_.json).mkString(",")}]}"""
  }
}
