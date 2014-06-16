package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)

    run
    assert(out.getSignal === false, "and 1: false & false == false")

    in1.setSignal(true)
    run
    assert(out.getSignal === false, "and 2: false & true == false")

    in2.setSignal(true)
    run
    assert(out.getSignal === true, "and 3: true & true == false")

    in1.setSignal(false)
    run
    assert(out.getSignal === false, "and 4: true & false == false")

    in2.setSignal(false)
    run
    assert(out.getSignal === false, "and 5: false & false == false")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)

    run
    assert(out.getSignal === false, "or 1: false | false == false")

    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or 2: false | true == true")

    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 3: true | true == true")

    in1.setSignal(false)
    run
    assert(out.getSignal === true, "or 4: true | false == true")

    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 5: false | false == false")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)

    run
    assert(out.getSignal === false, "or2 1: false | false == false")

    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or2 2: false | true == true")

    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or2 3: true | true == true")

    in1.setSignal(false)
    run
    assert(out.getSignal === true, "or2 4: true | false == true")

    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or2 5: false | false == false")
  }

  test("demuxGate example") {
    val in, c0, c1, out0, out1, out2, out3 = new Wire
    val c = List(c1, c0)
    val out = List(out3, out2, out1, out0)

    /*
     * Light the input wire so that we can discriminate the output
     * Wire that lights up based on mux select
     */
    demux(in, c, out)

    run
    assert(out forall (x => !(x.getSignal)),
      "demux 1: 0{0,0} == {0,0,0,0}")

    in.setSignal(true)
    run
    assert(out(3).getSignal && !out(2).getSignal &&
      !out(1).getSignal && !out(0).getSignal,
      "demux 2: 1{0,0} == {0,0,0,1}")

    c(1).setSignal(true)
    run
    assert(!out(3).getSignal && out(2).getSignal &&
      !out(1).getSignal && !out(0).getSignal,
      "demux 3: 1{0,1} == {0,0,1,0}")

    c(0).setSignal(true)
    run
    assert(!out(3).getSignal && !out(2).getSignal &&
      !out(1).getSignal && out(0).getSignal,
      "demux 4: 1{1,1} == {1,0,0,0}")

    c(1).setSignal(false)
    run
    assert(!out(3).getSignal && !out(2).getSignal &&
      out(1).getSignal && !out(0).getSignal,
      "demux 5: 1{1,0} == {0,1,0,0}")

    c(0).setSignal(false)
    run
    assert(out(3).getSignal && !out(2).getSignal &&
      !out(1).getSignal && !out(0).getSignal,
      "demux 6: 1{0,0} == {0,0,0,1}")

    in.setSignal(false)
    run
    assert(out forall (x => !(x.getSignal)),
      "demux 7: 0{0,0} == {0,0,0,0}")
  }
}
