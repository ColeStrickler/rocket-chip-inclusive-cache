package sifive.blocks.inclusivecache

import chisel3._
import chisel3.util._
import freechips.rocketchip.regmapper._
import freechips.rocketchip.tilelink._
import java.rmi.AccessException
import com.fasterxml.jackson.annotation.JsonProperty.Access
import midas.targetutils.SynthesizePrintf
import  freechips.rocketchip.interrupts._
import freechips.rocketchip.util.Annotated.interrupts


class CacheAccessCounterIO(Int : num_banks, Int: num_cpus) extends Bundle {
    val TLin = Input(Vec(num_banks, (TLBundle, TLEdgeIn)))
    val TLout = Input(Vec(num_banks, (TLBundle, TLEdgeIn)))
    val interrupt = Output(Vec(num_cpus, Bool()))
}

case class CacheAccessCounterParams
(
    num_cpus : Int,
)

class CacheAccessCounterModule(
  val accCounterParams : CacheAccessCounterParams,
  )(implicit p: Parameters) extends LazyModuleImpl
{
    val nBanks = p(BankedL2Key).nBanks
    val io = IO(new CacheAccessCounterIO(nBanks, accCounterParams.num_cpus))


    def YieldRegisters : Seq[(Int, RegField)] = {
        val mmreg = LLCAccessCounterReg ++ LLCMissCountersReg ++ CountInstFetchReg ++ CoreBudgetRegs ++ PeriodSetReg
        mmreg
    }


    /*
        Settings Registers
    */
    val memBase = p(ExtMem).get.master.base.U
    val countInstFetch = RegInit(true.B)
    val AccessCounterReset = RegInit(false.B)
    val EnableInterrupt = Seq.fill(accCounterParams.num_cpus)(RegInit(false.B))
    val domainIds = Reg(Vec(n, UInt(log2Ceil(accCounterParams.num_cpus).W)))
    val wPeriod = 25 // for max 33.5ms period, F = 1GHz
    val w = wPeriod - 3 // it can count up to a transaction per 8 cycles when window size is set to max
    val periodLen = Reg(UInt(wPeriod.W))


    /*
        Per-CacheBank counters
    */
    val PerBankMissCounters =  Seq.fill(accCounterParams.num_banks)(RegInit(VecInit(Seq.fill(accCounterParams.num_cpus)(0.U(64.W)))))
    val PerBankAccessCounters = Seq.fill(accCounterParams.num_banks)(RegInit(VecInit(Seq.fill(accCounterParams.num_cpus)(0.U(64.W)))))
    // Per-CPU counters
    val MissCounters = Seq.fill(accCounterParams.num_cpus)(RegInit(0.U(64.W)))
    val AccessCounters = Seq.fill(accCounterParams.num_cpus)(RegInit(0.U(64.W)))
    // Per-CPU Regulation Budgets
    val CoreBudgets = Seq.fill(accCounterParams.num_cpus)(RegInit(0.U(64.W)))
    
    // Regulation
    val periodCntrReset = periodCntr >= periodLen
    val periodCntr = Reg(UInt(wPeriod.W))
   

    val LLCAccessCountersReg = AccessCounters.zipWithIndex.map{ case (reg, i) => 
        (0x20 + i * 8) -> Seq(RegField.r(reg.getWidth, reg, RegFieldDesc(s"LLCAccessCounterReg${i}", s"Total LLC accesses for domainId=${i}")))
    }
    val MissCounterOffset = (0x100)
    val LLCMissCountersReg = module.MissCounters.zipWithIndex.map{ case (reg, i) =>
        (MissCounterOffset + i * 8) -> Seq(RegField.r(reg.getWidth, reg, RegFieldDesc(s"LLCMissCounterReg${i}", s"Total LLC misses for domainId=${i}")))
    }
    val CountInstFetchReg = Seq((0x200) -> Seq(RegField(countInstFetch.getWidth, countInstFetch, RegFieldDesc("countInstFetch", "Bool count instruction fetches in access counters"))))
    val EnableIntRegs = EnableInterrupt.zipWithIndex.map { case (reg, i)
        (0x208 + i*0x8)-> Seq(RegField(reg.getWidth, reg, RegFieldDesc(s"EnableInterrupt Core ${i}", s"Enable Interrupts Core ${i}"))))
    } 
    val CoreBudgetRegs = CoreBudgets.zipWithIndex.map { case (reg, i) => 
        (0x300 + i * 8) -> Seq(RegField.r(reg.getWidth, reg, RegFieldDesc(s"CoreBudget Core${i}", s"CoreBudget Core${i}")))
    }
    val PeriodSetReg = Seq((0x400) -> Seq(RegField.r(periodLen.width, periodLen, RegFieldDesc(s"PeriodLength", s"Period Length"))))



    periodCntr := Mux(periodCntrReset || !EnableInterrupt, 0.U, periodCntr + 1.U)

    when (periodCntrReset) // Reset all
    {
        for (i <- 0 until num_banks)
        {
          for (j <- 0 until accCounterParams.num_cpus)
          {
            PerBankAccessCounters(i)(j) := 0.U
            PerBankMissCounters(i)(j) := 0.U
          }
        }

        for (j <- 0 until accCounterParams.num_cpus)
        {
            MissCounters(j) := 0.U
            AccessCounters(j) := 0.U
        }

    }
    .otherwise // Calculate per core total accesses
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

    (io.TLin zip io.TLout).zipWithIndex map { case (((in, edgeIn), (out, edgeOut)), i) => {



        val inDomainID = Mux(in.a.fire, in.a.bits.domainID, Mux(in.c.fire, in.c.bits.domainID, 0.U))
        val outDomainID = Mux(out.a.fire, out.a.bits.domainID, Mux(out.c.fire, out.c.bits.domainID, 0.U))
        val aIsAcquire = in.a.bits.opcode === TLMessages.AcquireBlock
        val aIsInstFetch = in.a.bits.opcode === TLMessages.Get && in.a.bits.address >= memBase
        val aIsRead = aIsAcquire || (aIsInstFetch && countInstFetch)
        val aIsWrite = (in.a.bits.opcode === TLMessages.PutFullData || in.a.bits.opcode === TLMessages.PutPartialData) && in.a.bits.address >= memBase
        val cIsWb = in.c.bits.opcode === TLMessages.ReleaseData || in.c.bits.opcode === TLMessages.ProbeAckData
        val outaIsAcquire = io.out.a.bits.opcode === TLMessages.AcquireBlock
        val outaIsInstFetch = out.a.bits.opcode === TLMessages.Get && out.a.bits.address >= memBase
        val outCIsWb =  in.c.bits.opcode === TLMessages.ReleaseData || in.c.bits.opcode === TLMessages.ProbeAckData
        val isMiss = (outaIsAcquire || (outaIsInstFetch && countInstFetch)) && out.a.fire)
        val isWbToDRAM = (outCIsWb && out.c.fire)
        val toDRAM = (isMiss || isWbToDRAM)
        val isAccess = ((aIsWrite || aIsRead || (aIsInstFetch && countInstFetch)) && in.a.fire) || (cIsWb && in.c.fire) 
        when (!periodCntrReset)
        {
            when (toDRAM)
            {
                PerBankMissCounters(i)(outDomainID)  := PerBankMissCounters(i)(outDomainID) + 1.U
            }
            when (isAccess)
            {
                PerBankAccessCounters(i)(inDomainID) := PerBankAccessCounters(i)(inDomainID) + 1.U
            }
        }
    }

    /*
        Generate interrupt when counter exceeds budget
    */
    for (i <- 0 until accCounterParams.num_cpus)
    {
        io.interrupt(i) := MissCounters(i) >= CoreBudgets(i)
    }

  
}