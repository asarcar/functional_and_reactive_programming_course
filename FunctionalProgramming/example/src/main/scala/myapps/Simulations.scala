package myapps

import common._
import mystl.NLists._

object Simulations {
  /*
   * Action is a "pure side effect function":
   * It calls a block of code with no return type
   */
  type Action = () => Unit

  /*
   * Discrete event simulation
   */
  abstract class DES {
    /*
     * Event class captures the notion of an
     * action that is scheduled at a particular absolute
     * time epoch: "time" (assuming simulation starts
     * at epoch 0)
     */
    case class Event(time: Int, desc: String, action: Action) {
      override def toString = "Epoch " + time + ": " + desc + " "
    }
    import math.Ordering
    object EventOrdering extends Ordering[Event] {
      def compare(a: Event, b:Event) = a.time compare b.time 
    }

    type Agenda = NList[Event]

    private var curTime: Int = 0
    private var agenda: Agenda = NList()
    /*
     * Schedules an event after delay epochs:
     * the pure side effect block is executed (no return)
     * after curTime + delay time
     */
    def scheduleEvent(delay: Int)
      (desc: => String = "")
      (sideEffectBlock: => Unit) = {
      agenda = agenda.insert(Event(curTime + delay, desc , () => sideEffectBlock))(EventOrdering)
     
    }

    scheduleEvent(0)("*** Simulation has started ***")(())

    def run = {
      while (!agenda.isEmpty) {
        val ev = agenda.head
        curTime = ev.time
        agenda = agenda.tail
        print(ev)
        ev.action()
        println()
      }
    }
  }

  object DESim extends DES

  class Wire(desc: String) {
    type TriggerList = NList[Action]
    private var signal: Boolean = false
    /*
     * A wire feeds to many gates.
     * todos: list of actions (TriggerList) activated whenever the
     * signal state of the wire changes or when an action is
     * registered for the first time.
     * Think that each logic (and, or, not, ...) that is fed by the wire
     * registers with an action to propagate the state with the Wire
     */
    private var todos: TriggerList = NList()

    def getSignal: Boolean = signal

    /*
     * Since the signal of the wire has changed: execute
     * all the registered callback actions from each Gate
     * connected to the wire
     */
    def setSignal(newsignal: Boolean): Unit = {
      if (newsignal != signal) {
        signal = newsignal
        todos.foreach(a => a())
      }
    }

    /*
     * Callback to add callback action
     */
    def addAction(a: Action): Unit = {todos = a :: todos; a()}

    override def toString = "Wire " + desc + " signal " + signal
  }

  class Inverter(input: Wire, output: Wire) {
    val InverterDelay = 1
    def invertAction(): Unit = {
      /*
       * Get the input signal now:
       * schedule output signal based on current input after InverterDelay
       */
      val ipsig = input.getSignal
      DESim.scheduleEvent(InverterDelay)("Inverter") {
        output setSignal !ipsig
        print(output)
      }
    }

    input addAction invertAction
  }

  class And(input1: Wire, input2: Wire, output: Wire) {
    val AndDelay = 3
    def andAction(): Unit = {
      /*
       * Get the input signal now:
       * schedule output signal based on current input after InverterDelay
       */
      val ipsig1 = input1.getSignal
      val ipsig2 = input2.getSignal
      DESim.scheduleEvent(AndDelay)("And") {
        output setSignal (ipsig1 && ipsig2)
        print(output)
      }
    }

    input1 addAction andAction
    input2 addAction andAction
  }

  class Or(input1: Wire, input2: Wire, output: Wire) {
    val OrDelay = 2
    def orAction(): Unit = {
      /*
       * Get the input signal now:
       * schedule output signal based on current input after InverterDelay
       */
      val ipsig1 = input1.getSignal
      val ipsig2 = input2.getSignal
      DESim.scheduleEvent(OrDelay)("Or") {
        output setSignal (ipsig1 || ipsig2)
        print(output)
      }
    }

    input1 addAction orAction
    input2 addAction orAction
  }

  class HalfAdder(i1: Wire, i2: Wire, sum: Wire, carry: Wire) {
    val i1Ori2 = new Wire("i1Ori2")
    val invi1Andi2 = new Wire("inv(i1Andi2)")
    val or = new Or(i1, i2, i1Ori2)
    val and1 = new And(i1, i2, carry)
    val inv = new Inverter(carry, invi1Andi2)
    val and2 = new And(invi1Andi2, i1Ori2, sum)
  }

  def main(args: Array[String]) {
    /*
     * Circuit = oOr = Or(i1OroAnd = And(i1AndoInv = Inv(w1i), i2And), i2Or)
     */
    val iInv = new Wire("iInv")
    val i1AndoInv = new Wire("i1AndoInv")
    val i2And = new Wire("i2And")
    val i1OroAnd = new Wire("i1OroAnd")
    val i2Or = new Wire("i2Or")
    val oOr = new Wire("oOr")
    val inv = new Inverter(iInv, i1AndoInv)
    val and = new And(i1AndoInv, i2And, i1OroAnd)
    val or = new Or(i1OroAnd, i2Or, oOr)

    val i1 = new Wire("i1")
    val i2 = new Wire("i2")
    val sum = new Wire("sum")
    val carry = new Wire("carry")
    val halfadder = new HalfAdder(i1, i2, sum, carry)

    var id: Int = 0

    println("Invocation #: " + id); id = id + 1
    DESim.run
    i2And.setSignal(true)
    println("Invocation #: " + id); id = id + 1
    DESim.run
    iInv.setSignal(true)
    println("Invocation #: " + id); id = id + 1
    DESim.run
    i1.setSignal(true)
    println("Invocation #: " + id); id = id + 1
    DESim.run
    i2.setSignal(true)
    println("Invocation #: " + id); id = id + 1
    DESim.run
  }
}
