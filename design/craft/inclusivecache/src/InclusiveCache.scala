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
import freechips.rocketchip.subsystem._

import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import java.rmi.AccessException
import com.fasterxml.jackson.annotation.JsonProperty.Access
import midas.targetutils.SynthesizePrintf
import freechips.rocketchip.util.Annotated.interrupts
import freechips.rocketchip.devices.tilelink.CLINTConsts.ints


//class BlockDeviceFrontend(c: BlockDeviceFrontendParams)(implicit p: Parameters)
//  extends TLRegisterRouter(
//    c.address, "blkdev-controller", Seq("ucbbar,blkdev"),
//    interrupts = 1, beatBytes = c.beatBytes, concurrency = 1)(
//      new TLRegBundle(c, _)    with BlockDeviceFrontendBundle)(
//      new TLRegModule(c, _, _) with BlockDeviceFrontendModule)




class AccessCounterInterruptRouter(ctl: Option[InclusiveCacheControlParameters])(implicit p: Parameters) extends TLRegisterRouter(
  base = ctl.map(_.address).getOrElse(0),
   "cache-counters-int", Seq("ku-csl,intdev"),
  interrupts = 1, 
  beatBytes = ctl.map(_.beatBytes).getOrElse(8))(
      new TLRegBundle(ctl, _))(
      new TLRegModule(ctl, _, _))




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
  
  val intRouter = new AccessCounterInterruptRouter(control)
  val intNode = intRouter.intnode
  val (intSrc, _) = intNode.out(0)
  
  
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
    address     = Seq(AddressSet(c.address, 0xfff)),
    device      = device,
    concurrency = 1, // Only one flush at a time (else need to track who answers)
    beatBytes   = c.beatBytes)}

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {

    // If you have a control port, you must have at least one cache port
    require (!ctlnode.isDefined || !node.edges.in.isEmpty)

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

    // Flush directive
    val flushInValid   = RegInit(false.B)
    val flushInReady   = WireInit(init = false.B)
    val flushInAddress = Reg(UInt(64.W))
    val flushNoMatch   = WireInit(init = true.B)
    val flushOutValid  = RegInit(false.B)
    val flushOutReady  = WireInit(init = false.B)

    when (flushOutReady) { flushOutValid := false.B }
    when (flushInReady)  { flushInValid  := false.B }

    when (flushNoMatch && flushInValid) {
      flushInReady := true.B
      flushOutValid := true.B
    }


    /*
      Performance Counters that we added
    */
    val memBase = p(ExtMem).get.master.base.U
    val countInstFetch = RegInit(true.B)
    val AccessCounterReset = RegInit(false.B)
    val PerBankMissCounters =  Seq.fill(node.in.length)(RegInit(VecInit(Seq.fill(cache.numCPUs)(0.U(64.W)))))
    val PerBankAccessCounters = Seq.fill(node.in.length)(RegInit(VecInit(Seq.fill(cache.numCPUs)(0.U(64.W)))))
    assert(node.out.length == node.in.length)

    // Per-CPU counters
    val MissCounters = Seq.fill(cache.numCPUs)(RegInit(0.U(64.W)))
    val AccessCounters = Seq.fill(cache.numCPUs)(RegInit(0.U(64.W)))

    /*
      If we do not have this here, the counters only get zeroed out when a request traverses the bank/cpu combo
    */
    when (AccessCounterReset)
    {
        for (i <- 0 until node.in.length)
        {
          for (j <- 0 until cache.numCPUs)
          {
            PerBankAccessCounters(i)(j) := 0.U
            PerBankMissCounters(i)(j) := 0.U
          }
        }

        for (j <- 0 until cache.numCPUs)
        {
            MissCounters(j) := 0.U
            AccessCounters(j) := 0.U
        }

    }
    .otherwise 
    {
        for (j <- 0 until cache.numCPUs)
        {
          val tmpSumMiss = VecInit(Seq.fill(node.in.length)(0.U(64.W)))
          val tmpSumAccess = VecInit(Seq.fill(node.in.length)(0.U(64.W)))
          tmpSumMiss(0) := PerBankMissCounters(0)(j)
          tmpSumAccess(0) := PerBankAccessCounters(0)(j)
          for (i <- 1 until node.in.length)
          {
            tmpSumMiss(i) := PerBankMissCounters(i)(j) + tmpSumMiss(i-1)
            tmpSumAccess(i) := PerBankAccessCounters(i)(j) + tmpSumAccess(i-1)
          }

          MissCounters(j) := tmpSumMiss(node.in.length - 1)
          AccessCounters(j) := tmpSumAccess(node.in.length - 1)
        }    
    }




    val flush32 = RegField.w(32, RegWriteFn((ivalid, oready, data) => {
      when (oready) { flushOutReady := true.B }
      when (ivalid) { flushInValid := true.B }
      when (ivalid && !flushInValid) { flushInAddress := data << 4 }
      (!flushInValid, flushOutValid)
    }), RegFieldDesc("Flush32", "Flush the physical address equal to the 32-bit written data << 4 from the cache"))

    val flush64 = RegField.w(64, RegWriteFn((ivalid, oready, data) => {
      when (oready) { flushOutReady := true.B }
      when (ivalid) { flushInValid := true.B }
      when (ivalid && !flushInValid) { flushInAddress := data }
      (!flushInValid, flushOutValid)
    }), RegFieldDesc("Flush64", "Flush the phsyical address equal to the 64-bit written data from the cache"))

    // Information about the cache configuration
    // starts at 0x2010000
    val banksR  = Seq(0 -> Seq(RegField.r(8, node.edges.in.size.U,               RegFieldDesc("Banks",
      "Number of banks in the cache", reset=Some(node.edges.in.size)))))
    val waysR   = Seq(0x8 -> Seq(RegField.r(8, cache.ways.U,                       RegFieldDesc("Ways",
      "Number of ways per bank", reset=Some(cache.ways)))))
    val lgSetsR = Seq(0x10 -> Seq(RegField.r(8, log2Ceil(cache.sets).U,             RegFieldDesc("lgSets",
      "Base-2 logarithm of the sets per bank", reset=Some(log2Ceil(cache.sets))))))
    val lgBlockBytesR = Seq(0x18 -> Seq(RegField.r(8, log2Ceil(cache.blockBytes).U, RegFieldDesc("lgBlockBytes",
      "Base-2 logarithm of the bytes per cache block", reset=Some(log2Ceil(cache.blockBytes))))))


    
    val LLCAccessCounters = AccessCounters.zipWithIndex.map{ case (reg, i) => 
      (0x20 + i * 8) -> Seq(RegField.r(reg.getWidth, reg, RegFieldDesc(s"LLCAccessCounterReg${i}", s"Total LLC accesses for domainId=${i}")))
    }
    val MissCounterOffset = (0x100)
    val LLCMissCounters = MissCounters.zipWithIndex.map{ case (reg, i) =>
      (MissCounterOffset + i * 8) -> Seq(RegField.r(reg.getWidth, reg, RegFieldDesc(s"LLCMissCounterReg${i}", s"Total LLC misses for domainId=${i}")))
    }
    val AccessCounterResetOffset = (0x190)

    val LLCAccessCounterReset = Seq((AccessCounterResetOffset) -> Seq(RegField(AccessCounterReset.getWidth, AccessCounterReset, RegFieldDesc("LLCAccCtrReset", "Reset module for LLC access counters"))))
    val CountInstFetchReg = Seq((AccessCounterResetOffset + 0x8) -> Seq(RegField(countInstFetch.getWidth, countInstFetch, RegFieldDesc("countInstFetch", "Bool count instruction fetches in access counters"))))

    val flush64Reg = Seq(0x200 ->  Seq(flush64))
    val flush32Reg = Seq(0x240 -> Seq(flush32))

    val mmreg = banksR ++ waysR ++ lgSetsR ++ lgBlockBytesR ++ LLCAccessCounters ++ LLCMissCounters ++ LLCAccessCounterReset ++ CountInstFetchReg ++ flush64Reg ++ flush32Reg
    
    intSrc := AccessCounterReset


    val regmap = ctlnode.map{ c =>
      c.regmap(mmreg: _*)
    }

   // val regmap = ctlnode.map { c =>
   //   c.regmap(
   //     0x000 -> RegFieldGroup("Config", Some("Information about the Cache Configuration"), Seq(banksR, waysR, lgSetsR, lgBlockBytesR, LLCAccessCounterReg, LLCMissCounterReg,
   //     LLCAccessCounterReset, CountInstFetchReg)),
   //     0x200 -> (if (control.get.beatBytes >= 8) Seq(flush64) else Seq()),
   //     0x240 -> Seq(flush32)
   //   )
   // }

    

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

      val params = InclusiveCacheParameters(cache, micro, control.isDefined, edgeIn, edgeOut)
      val scheduler = Module(new InclusiveCacheBankScheduler(params)).suggestName("inclusive_cache_bank_sched")
      
      /* 
        We cannot do this here because the system bus sits between the L2 and the L1

        These in edges are not directly connected to the core Tiles
      */
      //in.a.bits.domainId := i.U // set the domainID based on the input edge index
      scheduler.io.in <> in
      out <> scheduler.io.out
      

      /* Performance Counters */
      val outDomainID = scheduler.io.out.a.bits.domainId
      val inDomainID = in.a.bits.domainId
      when ( in.a.fire ) {
        SynthesizePrintf("in.a.fire source %d, domainID %d, Count %d, Bank %d\n", in.a.bits.source, in.a.bits.domainId, PerBankAccessCounters(i)(inDomainID), i.U)
      }

      val aIsAcquire = in.a.bits.opcode === TLMessages.AcquireBlock
      val aIsInstFetch = in.a.bits.opcode === TLMessages.Get && in.a.bits.address >= memBase
      val aIsRead = aIsAcquire || (aIsInstFetch && countInstFetch)
      val aIsWrite = (in.a.bits.opcode === TLMessages.PutFullData || in.a.bits.opcode === TLMessages.PutPartialData) && in.a.bits.address >= memBase
      val cIsWb = in.c.bits.opcode === TLMessages.ReleaseData || in.c.bits.opcode === TLMessages.ProbeAckData
      val outaIsAcquire = scheduler.io.out.a.bits.opcode === TLMessages.AcquireBlock
      val outaIsInstFetch = scheduler.io.out.a.bits.opcode === TLMessages.Get && scheduler.io.out.a.bits.address >= memBase

      
      val isMiss = (outaIsAcquire || (outaIsInstFetch && countInstFetch)) && out.a.fire
      val isAccess = ((aIsWrite || aIsRead || (aIsInstFetch && countInstFetch)) && in.a.fire) || (cIsWb && in.c.fire) 

      when (!AccessCounterReset)
      {
        when (isMiss)
        {
          PerBankMissCounters(i)(outDomainID)  := PerBankMissCounters(i)(outDomainID) + 1.U
        }
        when (isAccess)
        {
          PerBankAccessCounters(i)(inDomainID) := PerBankAccessCounters(i)(inDomainID) + 1.U
        }
      }


      when ( scheduler.io.out.a.fire ) {
        SynthesizePrintf("scheduler.io.out.a.fire source %d, domainID %d, Count %d, Bank %d\n", scheduler.io.out.a.bits.source, scheduler.io.out.a.bits.domainId, PerBankMissCounters(i)(outDomainID), i.U)
      }
       /* Performance Counters */


      scheduler.io.ways := DontCare
      scheduler.io.divs := DontCare

      val flushSelect = edgeIn.manager.managers.flatMap(_.address).map(_.contains(flushInAddress)).reduce(_||_)
      when (flushSelect) { flushNoMatch := false.B }

      when (flushSelect && scheduler.io.req.ready)  { flushInReady := true.B }
      when (scheduler.io.resp.valid) { flushOutValid := true.B }
      assert (!scheduler.io.resp.valid || flushSelect)

      scheduler.io.req.valid := flushInValid && flushSelect
      scheduler.io.req.bits.address := flushInAddress
      scheduler.io.resp.ready := !flushOutValid

      // Fix-up the missing addresses. We do this here so that the Scheduler can be
      // deduplicated by Firrtl to make hierarchical place-and-route easier.

      out.a.bits.address := params.restoreAddress(scheduler.io.out.a.bits.address)
      in .b.bits.address := params.restoreAddress(scheduler.io.in .b.bits.address)
      out.c.bits.address := params.restoreAddress(scheduler.io.out.c.bits.address)

      scheduler
    }

    def json = s"""{"banks":[${mods.map(_.json).mkString(",")}]}"""
  }
}
