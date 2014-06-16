package simulations

import math.random

class EpidemySimulator extends Simulator {

  protected[simulations] object SimConfig {
    /*
     * Simulation parameters: During Test
     * productionRun = false
     * maxSimulationDays = 1000
     * dbgLevel = dbgVerbose
     * population = 64
     * roomRows = 4
     * roomColumns = 4
     * prevRate = 0.75
     * transRate = 0.75
     * deathRate = 1.0
     * airTrafficRate = 0.01
     * mobilityRate = 0.5
     * vaccineRate = 0.05
     */
    val productionRun: Boolean = true
    val maxSimulationDays: Int = 0 // NA when productionRun is true
    val dbgLevel: Int = 0
    val (dbgBasic, dbgVerbose, dbgVeryVerbose) = (1, 2, 3)
    val population: Int = 300
    val roomRows: Int = 12//8
    val roomColumns: Int = 12//8
    // waiting time between moves 1 to maxDaysToMove
    val maxDaysToMove: Int = 5
    /*
     * prevalence rate in fraction: 0.01 for 1%:
     * Note prevalence rate is not probabilistic but an
     * actual measure of fraction
     */
    val prevRate: Double = 0.01
    // transmissiblity rate in fraction: 0.40 for 40%
    val transRate: Double = 0.40
    // one is "visibly infectous" after incubation period
    val incubPeriod: Int = 6
    // death rate in fraction: 0.25 for 25% assumed infected
    val deathRate: Double = 0.25
    // days from 1st infection to death (if dying)
    val deathPeriod: Int = 14
    // days from 1st infection to immunity (if not dying)
    val immunePeriod: Int = 16
    // days from 1st infection to init state: infection possible
    val healthyPeriod: Int = 18
    // probability of choosing a flight
    val airTrafficRate: Double = 0.01//0
    /*
     * mobility rate in fraction: 1.00 for 100%
     * fraction of people who are "allowed to moved"
     */
    val mobilityRate: Double = 0.5//1.0
    /*
     * sick mobility rate rate in fraction: 1.00 for 100%
     * fraction of "visibly infected people" who are "allowed to moved"
     */
    val sickMobilityRate: Double = mobilityRate*mobilityRate
    /*
     * vaccinerRate: rate in fraction: 0.0 for 0%
     * fraction of people who can "never" fall sick as they have
     * received vaccinations
     */
    val vaccineRate: Double = 0.05//0
  }
  import SimConfig._

  /*
   * Generates a random number between [0 and i) i.e. including 0
   * but excluding i
   */
  def randomBelow(i: Int): Int = (random * i).toInt
  /*
   * Boolean Generator: true with a probability of "rate"
   * and false with a probability of (1 - rate)
   */
  def didProbEventOccur(rate: Double): Boolean = (random < rate)

  // Generates a random number between 1 and maxDaysToMove
  def randomNextMoveDay = (randomBelow(maxDaysToMove) + 1)

  case class Pos(row: Int, col: Int) {
    /*
     * A cell in pos p is close to this cell if either p is same as
     * this cell or it is reachable via a linear move
     */
    def isNeighbor(p: Pos): Boolean = 
      ((linearMoves map (_ onPos this)).toSet + this) contains p
  }

  sealed abstract class Move {
    def onPos(p: Pos): Pos
    // def onPerson(p: Person): Unit
  }
  case object Left extends Move {
    def onPos(p: Pos): Pos = Pos(p.row, (p.col-1+roomColumns)%roomColumns)
  }
  case object Right extends Move {
    def onPos(p: Pos): Pos = Pos(p.row, (p.col+1+roomColumns)%roomColumns)
  }
  case object Up extends Move {
    def onPos(p: Pos): Pos = Pos((p.row-1+roomRows)%roomRows, p.col)
  }
  case object Down extends Move {
    def onPos(p: Pos): Pos = Pos((p.row+1+roomRows)%roomRows, p.col)
  }
  case object Random extends Move {
    def onPos(p: Pos): Pos = {
      /*
       * If the newPos lands you back to your origin or neighbors
       * keep executing Random move until you reach a distant cell
       */
      val newPos = Pos(randomBelow(roomRows), randomBelow(roomColumns))
      // Non adjacent cell is only possible when #Cells > 3
      if ((p isNeighbor newPos) && (roomRows*roomColumns > 3)) {
        onPos(p)
      } else {
        newPos
      }
    }
  }

  val linearMoves: Array[Move] = Array(Left, Right, Up, Down)
  val moves: Array[Move] = (linearMoves.toSet + Random).toArray

  /*
   * ID of all persons in the population
   * ID of those persons who are vaccinated
   * ID of those persons who are infected initially
   */
  val personIds = List.range(0, population)
  val personIdsShuffle = scala.util.Random.shuffle(personIds)

  val (vaccinatedPersonIds, nonVaccinatedPersonIds) =
    personIdsShuffle splitAt (population*vaccineRate).toInt

  val infectedPersonIds =
    nonVaccinatedPersonIds take (population*prevRate).toInt

  val persons: List[Person] = {
    for {
      id <- personIds
    } yield new Person(id)
  }

  /*
   * Schedules Event in the Simulator
   */
  def scheduleEvent(delay: Int, block: => Unit, str: => String) = {
    require(productionRun | (currentTime < maxSimulationDays),
      s"Simulation Time exceeded in test run: currentTime $currentTime")
    afterDelay(delay){
      block
      if (dbgLevel >= dbgBasic)
        println(s"  T($currentTime): $str")
    }
  }

  /*
   * 1. If she is already infected we are done.
   * 2. Otherwise, based on transmissibility evaluate
   *    whether she should be infected. If not we are done.
   * 3. Otherwise, she is infected. Schedule the event to
   *    transition her to visibly infectious state after
   *    incubPeriod.
   */
  def goInfect(p: Person): Unit = {
    // Add idiotic workarounds to pass the dead test
    assert(!(p.vaccinated|p.infected|p.sick|p.dead|p.immune),
      s"Infect failure: ${p.fullString(dbgVeryVerbose)}")
    p.infected = true
    scheduleEvent(incubPeriod, goSick(p), s"${p.toString} is sick")
  }
  def goSick(p: Person): Unit = {
    assert(!(p.vaccinated | !p.infected | p.sick | p.dead | p.immune),
      s"Sick failure: ${p.fullString(dbgVeryVerbose)}")
    p.sick = true
    scheduleEvent(deathPeriod - incubPeriod, mayDie(p),
      s"${p.toString} may die")
  }
  def mayDie(p: Person): Unit = {
    assert(!(p.vaccinated | !p.infected | !p.sick | p.dead | p.immune),
      s"MayDie failure: ${p.fullString(dbgVeryVerbose)}")
    if (didProbEventOccur(deathRate)) scheduleEvent(0, goDie(p),
      s"${p.fullString(dbgVeryVerbose)} is dead")
    else scheduleEvent(immunePeriod - deathPeriod, goImmune(p),
      s"${p.toString} is immune")
  }
  def goDie(p: Person): Unit = {
    assert(!(p.vaccinated | !p.infected | !p.sick | p.dead | p.immune),
      s"Dead failure: ${p.fullString(dbgVeryVerbose)}")
    p.dead = true
  }
  def goImmune(p: Person): Unit = {
    /*
     * No longer visibly infectious, but remains infectious.
     * An immune person cannot get infected.
     */
    assert(!(p.vaccinated | !p.infected | !p.sick | p.dead | p.immune),
      s"Immune failure: ${p.fullString(dbgVeryVerbose)}")
    p.sick = false // no longer visibly infectious
    p.immune = true // immune => cannot get infected
    scheduleEvent(healthyPeriod - immunePeriod, goHealthy(p),
        s"${p.toString} is normal")
  }
  def goHealthy(p: Person): Unit = {
    assert(!(p.vaccinated | !p.infected | p.sick | p.dead | !p.immune),
      s"Healthy failure: ${p.fullString(dbgVeryVerbose)}")
    p.immune = false
    p.infected = false
  }
    
  /*
   * Execute a move and the consequent actions required of move
   */
  def move(p: Person): Unit = {
    /*
     * Assuming valid move is decided:
     *   we execute the side effect actions.
     * 1. If person was in a room with infectious people and
     *    based on transmissibility rate she is to be infected
     *    then the person is infected.
     * 2. We change the cell of the Person
     */
    def moveTo(newPos: Pos): Unit = {
      val infectionLikely =
        persons.exists(p2 => !p.vaccinated && !p.infected &&
          (p2.id != p.id) && p2.infected &&
          Pos(p2.row, p2.col) == Pos(p.row, p.col))
      if (infectionLikely && didProbEventOccur(transRate))
        scheduleEvent(0, goInfect(p), s"${p.toString} is infected")
      // Move to the new Cell
      p.row = newPos.row; p.col = newPos.col
    }

    /*
     * Boolean Generator: Probability of true is at the rate
     * of airTrafficRate
     */
    def airTrafficAllowed: Boolean = didProbEventOccur(airTrafficRate)

    /*
     * 1. Person is allowed to effect the move only if it wins
     *    the chance governed by mobilityRate if not "visibly sick"
     *    or by sickMobilityRate if "otherwise"
     * i.e. moveAllowed with a probability of mobilityRate
     * if person is not "visibly infectious" or else true with a probability of
     * sickMobilityRate otherwise if person is "visibly infectious"
     */
    def moveAllowed: Boolean = ((!p.sick & didProbEventOccur(mobilityRate)) |
      (p.sick & didProbEventOccur(sickMobilityRate)))

    /*
     * Generates a list of possible linear moves(left, right, up, down):
     * that does not have any "visibly infected" people. It chooses
     * one of those neighbor cells in random as the next move. If there
     * are none it stays put.
     */
    def legalNeighborPositions: Array[Pos] = {
      for {
        move <- linearMoves
        newPos = move onPos Pos(p.row, p.col)
        /*
         * No "visibly infected" person should exist in the new cell
         * where the person would move
         */
        if (!(persons.exists(p2 => (p2.id != p.id) && p2.sick &&
          (Pos(p2.row, p2.col) == newPos))))
      } yield {
        newPos
      }
    } 

    /*
     * Choose any adjacent position from the list of choices available
     */
    def randomNewPos(choiceOfPositions: Array[Pos]): Pos = {
      require(!choiceOfPositions.isEmpty,
        "Trying to choose new position from empty Set")
      choiceOfPositions(randomBelow(choiceOfPositions.size))
    }

    /*
     * 2. Person decides to fly if she wins the flight probability
     *    airTrafficRate. In this case, it just effects the move.
     * 3. Person observes which among its neighbors (linearly accessible)
     *    can be a candidate "legal" move. If few exists, choose one
     *    randomly and effect the move.
     */
    def attemptMove: Unit = {
      if (airTrafficAllowed) {
        moveTo(Random onPos Pos(p.row, p.col))
      } else {
        // Choose a random move among legalMoves: execute moveTo that pos
        val legalNeighbors = legalNeighborPositions
        if (!legalNeighbors.isEmpty) {
          moveTo(randomNewPos(legalNeighbors))
        }
      }
    }

    if (p.dead) {
      // Do nothing: should terminate any activity on this person
    } else {
      if (moveAllowed)
        attemptMove
      // Schedule another move in future
      scheduleEvent(randomNextMoveDay, move(p),
        s"${p.toString} attempted move")
    }
  }
  
  class Person (val id: Int) {
    /*
     * Person States: (1) Healthy (2) Infected (3) Sick
     *                (4) Immune (5) Dead
     * Healthy: Initial State of Most People:
     *   !(infected | sick | immune | deal)
     * Infected: infected => sick, immune, or dead are
     *   infected as well
     * Sick: Visibly Infectious: sick => dead/immune are
     *   also visibly infectious
     * Dead: dead
     */
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    val vaccinated = vaccinatedPersonIds contains id

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    val kickStart: Unit = {
      // Infect a fraction of population based on prevalence rate
      if (infectedPersonIds contains id) {
        // Test case expect infected and vaccinated patients are marked
        // even before the agenda list is processed
        // scheduleEvent(0, goInfect(this), s"$toString started with infection")
        goInfect(this)
      }
      // Kickstart the next move
      scheduleEvent(randomNextMoveDay, move(this),
        s"$toString attempted move")
    }

    def fullString(level: Int): String = {
      val tmp: String = if (level >= dbgVeryVerbose) {
        s" vac $vaccinated, inf $infected, sick $sick, imm $immune, dead $dead"
      } else {""}
      s"'ID ($id): pos <$row $col>$tmp'"
    }
    override def toString: String = fullString(dbgLevel)
  }
}

object EpidemyMain extends App {
  val es = new EpidemySimulator

  println("BEGIN SIMULATION")
  es.run
  println("END SIMULATION")
}
