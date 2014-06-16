package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction(): Unit = {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output setSignal (a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val a1not, a2not, a1notAnda2not = new Wire
    inverter(a1, a1not)
    inverter(a2, a2not)
    andGate(a1not, a2not, a1notAnda2not)
    inverter(a1notAnda2not, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    require(out.length == math.pow(2, c.length).toInt, s"${out.length} != 2^${c.length}")

    /*
     * If the mux select wire is not present: the mux is equivalent to a wire
     * where the output is just connected to the input
     */
    if (c.isEmpty) {
      in addAction { () => {
        val inSig = in.getSignal; afterDelay(0) {out.head setSignal inSig}
      }}
    } else {
      /*
       * Mux Wires: Wire 'n' discriminate between zero half and one half of output wires
       * When Wire n == 0, zero half is chosen otherwise bottom half is chosen
       * oneHalf = in & c.head
       * zeroHalf = in & !c.head
       */
      val cnNot, zeroHalf, oneHalf = new Wire
      inverter(c.head, cnNot)
      andGate(in, c.head, oneHalf)
      andGate(in, cnNot, zeroHalf)

      /*
       * Partition the output wires into two equal halves: onePart, zeroPart
       * oneHalf is the new input to onePart:
       *   out(0..2^(n-1)-1) half the output wire: wire# 2^n-1 down to 2^(n-1)
       * zeroHalf is the new input to zeroPart:
       *   out(2^(n-1)..2^n) half the output wire: wire# 2^(n-1)-1 down to 0
       *   Note the output wires are sorted in decreasing order with the
       *   head of the list starting for wire index 2^n - 1
       */
      val (onePart, zeroPart) = out.splitAt(out.length/2)

      demux(oneHalf, c.tail, onePart)
      demux(zeroHalf, c.tail, zeroPart)
    }
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    println(List.range(0,10).map(_ => '#').mkString +
      "Begin-AND" + List.range(0,10).map(x=>'#').mkString)
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    run
    in1.setSignal(true)
    run
    in2.setSignal(true)
    run
    in1.setSignal(false)
    run
    in2.setSignal(false)
    run
    println(List.range(0,10).map(_ => '-').mkString +
      "End-AND" + List.range(0,10).map(x=>'-').mkString)
  }

  def orGateExample {
    println(List.range(0,10).map(_ => '#').mkString +
      "Begin-OR" + List.range(0,10).map(x=>'#').mkString)
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    run
    in1.setSignal(true)
    run
    in2.setSignal(true)
    run
    in1.setSignal(false)
    run
    in2.setSignal(false)
    run
    println(List.range(0,10).map(_ => '-').mkString +
      "End-OR" + List.range(0,10).map(x=>'-').mkString)
  }

  def orGate2Example {
    println(List.range(0,10).map(_ => '#').mkString +
      "Begin-OR2" + List.range(0,10).map(x=>'#').mkString)
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    run
    in1.setSignal(true)
    run
    in2.setSignal(true)
    run
    in1.setSignal(false)
    run
    in2.setSignal(false)
    run
    println(List.range(0,10).map(_ => '-').mkString +
      "End-OR2" + List.range(0,10).map(x=>'-').mkString)
  }

  def demuxGateExample {
    println(List.range(0,10).map(_ => '#').mkString +
      "Begin-DEMUX" + List.range(0,10).map(x=>'#').mkString)
    val in, c0, c1, out0, out1, out2, out3 = new Wire
    val c = List(c1, c0)
    val out = List(out3, out2, out1, out0)

    /*
     * Light the input wire so that we can discriminate the output
     * Wire that lights up based on mux select
     */
    demux(in, c, out)
    probe("in", in)
    probe("c1", c1)
    probe("c0", c0)
    probe("out3", out3)
    probe("out2", out2)
    probe("out1", out1)
    probe("out0", out0)
    run
    in.setSignal(true)
    run
    c(1).setSignal(true)
    run
    c(0).setSignal(true)
    run
    c(1).setSignal(false)
    run
    c(0).setSignal(false)
    run
    println(List.range(0,10).map(x=>'-').mkString +
      "End-DEMUX" + List.range(0,10).map(x=>'-').mkString)
  }
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
  Circuit.orGateExample
  Circuit.orGate2Example
  Circuit.demuxGateExample
}
