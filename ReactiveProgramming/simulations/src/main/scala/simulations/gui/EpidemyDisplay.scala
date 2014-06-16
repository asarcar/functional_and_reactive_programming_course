package simulations
package gui

import javax.swing.{JComponent, JFrame, JLabel, Timer, SwingUtilities}
import javax.swing.border.{EmptyBorder}
import java.awt.{Graphics, Graphics2D, GridLayout, BorderLayout, Color, Dimension, Rectangle, Polygon}
import java.awt.event.{ActionListener, ActionEvent}

object EpidemyDisplay extends EpidemySimulator with App {

  class Situation(var healthy: Int, var infected: Int, var sick: Int,
    var dead: Int, var immune: Int, var vaccinated: Int) {
    def reset { healthy = 0; infected = 0; sick = 0; immune = 0; dead = 0; vaccinated = 0 }
    def count(p: Person) {
      if (p.infected && !(p.sick || p.dead || p.immune)) infected += 1
      else if (p.sick && !p.dead) sick += 1
      else if (p.dead) dead += 1
      else if (p.immune) immune += 1
      else if (p.vaccinated) vaccinated += 1
      else healthy += 1
    }
    override def toString() = "Situation(" + healthy + ", " + infected +
    ", " + sick + ", " + dead + ", " + immune + ", " + vaccinated + ")"
  }

  val world: Grid[Situation] = new Grid[Situation](SimConfig.roomRows, SimConfig.roomColumns)
  for (row <- 0 to world.height - 1; col <- 0 to world.width - 1)
    world.update(row, col, new Situation(0, 0, 0, 0, 0, 0))
  var history: List[Situation] = Nil 
  var historyContinues = true

  def updateWorld() {
    for (p <- persons) world(p.row, p.col) count p
  }
  updateWorld()

  def updateHistory() {
    historyContinues = history.isEmpty
    for (p <- persons) {
      historyContinues = historyContinues || p.infected
    }
    val ns = new Situation(0, 0, 0, 0, 0, 0)
    for (s <- world) {
      ns.healthy += s.healthy
      ns.infected += s.infected
      ns.sick += s.sick
      ns.dead += s.dead
      ns.immune += s.immune
      ns.vaccinated += s.vaccinated
    }
    history = ns :: history
  }

  def hasStep: Boolean = !agenda.isEmpty

  private object GraphicConfig {
    val delay = 200
    val personSize = 8
    val interPersonSize = 4
    val roomBorderSize = 4
    val interRoomSize = 4
    val worldBorderSize = 12
    val doorSize = 12
    val lineCount = 4
    def roomSize = (lineCount * personSize) +
    ((lineCount - 1) * interPersonSize) + (2 * roomBorderSize) + 2
    def totalCount = lineCount * lineCount
    def doorWallSize = (roomSize - doorSize) / 2
  }

  import GraphicConfig._

  class Room (val worldRow: Int, val worldCol: Int) extends JComponent {
    val roomDimension = new Dimension(roomSize + 1, roomSize + 1)
    setPreferredSize(roomDimension)
    var situation: Situation = null
    def healthy = (situation.healthy) min totalCount
    def infected = (healthy + situation.infected) min totalCount
    def sick = (infected + situation.sick) min totalCount
    def dead = (sick + situation.dead) min totalCount
    def immune = (dead + situation.immune) min totalCount
    def vaccinated = (immune + situation.vaccinated) min totalCount
    override def paintComponent(g: Graphics) {
      val graph = g.asInstanceOf[Graphics2D]
      graph.setColor(Color.WHITE)
      graph.drawPolyline(Array(0, 0, doorWallSize), Array(doorWallSize, 0, 0), 3)
      graph.drawPolyline(Array(doorWallSize + doorSize, roomSize, roomSize),
        Array(0, 0, doorWallSize), 3)
      graph.drawPolyline(Array(roomSize, roomSize, doorWallSize + doorSize),
        Array(doorWallSize + doorSize, roomSize, roomSize), 3)
      graph.drawPolyline(Array(doorWallSize, 0, 0),
        Array(roomSize, roomSize, doorWallSize + doorSize), 3)
      for (row <- 0 until lineCount; col <- 0 until lineCount) {
        def color(state: Int) = (state / lineCount) > row ||
          ((state / lineCount) == row && (state % lineCount) > col)
        if (color(healthy)) graph.setColor(Color.GREEN)
        else if (color(infected)) graph.setColor(Color.MAGENTA)
        else if (color(sick)) graph.setColor(Color.ORANGE)
        else if (color(dead)) graph.setColor(Color.RED)
        else if (color(immune)) graph.setColor(Color.YELLOW)
        else if (color(vaccinated)) graph.setColor(Color.BLUE)
        else graph.setColor(Color.DARK_GRAY)
        graph.drawOval(roomBorderSize + 1 + (col * (personSize + interPersonSize)), roomBorderSize + 1 + (row * (personSize + interPersonSize)), personSize, personSize)
      }
    }
    def setSituation(s: Situation): this.type = {
      situation = s
      this
    }
  }

  val frame = new JFrame("Scaliosis") { frame =>
    setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    setBackground(Color.BLACK)
    val rooms: Grid[Room] = new Grid[Room](world.height, world.width)
    object populationGraph extends JComponent {
      val graphHeight = 100
      setPreferredSize(new Dimension(getWidth, graphHeight))
      override def paintComponent(g: Graphics) {
        val graph = g.asInstanceOf[Graphics2D]
        if (history.isEmpty) {
          graph.setColor(Color.DARK_GRAY)
          graph.fill(new Rectangle(getWidth, graphHeight))
        }
        else {
          val steps: Double = history.length - 1
          val advanceStep: Double = (((getWidth - 3).toDouble) / (steps + 1)).toDouble
          def proportion(count: Int): Int =
            getHeight - ((getHeight - 3) * (count.toDouble /
              SimConfig.population.toDouble)).toInt - 2
          def advance(index: Int): Int =
            getWidth - (advanceStep * ((index + 1.0))).toInt
          val infectedPoly = new Polygon()
          val sickPoly = new Polygon()
          val deadPoly = new Polygon()
          val immunePoly = new Polygon()
          val vaccinatedPoly = new Polygon()
          var prevStep = -1
          for ((s, i) <- history zip history.indices) {
            val infected = proportion(s.infected)
            val sick = proportion(s.sick + s.infected)
            val dead = proportion(s.sick + s.infected + s.dead)
            val immune = proportion(s.sick + s.infected + s.dead + s.immune)
            val vaccinated = proportion(s.sick + s.infected + s.dead +
              s.immune + s.vaccinated)
            val step = advance(i) - 2
            if (prevStep != step) {
              infectedPoly.addPoint(step, infected)
              sickPoly.addPoint(step, sick)
              deadPoly.addPoint(step, dead)
              immunePoly.addPoint(step, immune)
              vaccinatedPoly.addPoint(step, vaccinated)
            }
            prevStep = step
          }
          infectedPoly.addPoint(1, getHeight - 2)
          infectedPoly.addPoint(getWidth - 2, getHeight - 2)
          sickPoly.addPoint(1, getHeight - 2)
          sickPoly.addPoint(getWidth - 2, getHeight - 2)
          deadPoly.addPoint(1, getHeight - 2)
          deadPoly.addPoint(getWidth - 2, getHeight - 2)
          immunePoly.addPoint(1, getHeight - 2)
          immunePoly.addPoint(getWidth - 2, getHeight - 2)
          vaccinatedPoly.addPoint(1, getHeight - 2)
          vaccinatedPoly.addPoint(getWidth - 2, getHeight - 2)

          infectedPoly.addPoint(getWidth - 2, proportion(history.head.infected))
          sickPoly.addPoint(getWidth - 2,
            proportion(history.head.infected + history.head.sick))
          deadPoly.addPoint(getWidth - 2,
            proportion(history.head.infected + history.head.sick +
              history.head.dead))
          immunePoly.addPoint(getWidth - 2,
            proportion(history.head.infected + history.head.sick +
              history.head.dead + history.head.immune))
          vaccinatedPoly.addPoint(getWidth - 2,
            proportion(history.head.infected + history.head.sick +
              history.head.dead + history.head.immune +
              history.head.vaccinated))

          graph.setColor(Color.GREEN)
          graph.fill(new Rectangle(getWidth, graphHeight))

          graph.setColor(Color.BLUE)
          graph.fillPolygon(vaccinatedPoly)
          graph.setColor(Color.YELLOW)
          graph.fillPolygon(immunePoly)
          graph.setColor(Color.RED)
          graph.fillPolygon(deadPoly)
          graph.setColor(Color.ORANGE)
          graph.fillPolygon(sickPoly)
          graph.setColor(Color.MAGENTA)
          graph.fillPolygon(infectedPoly)
        }

        graph.setColor(Color.WHITE)
        graph.drawRect(0, 0, getWidth -1, graphHeight - 1)
      }
    }
    object roomDisplay extends JComponent {
      setLayout(new GridLayout(world.width, world.height, interRoomSize, interRoomSize))
      setBorder(new EmptyBorder(worldBorderSize, 0, worldBorderSize, 0))
      for (row <- 0 until world.height; col <- 0 until world.width) {
        val room = (new Room(row, col)) setSituation world(row, col)
        rooms.update(row, col, room)
        add(room)
      }
    }
    object clock extends JLabel with ActionListener {
      val time = new Timer(delay, this)
      def start = time.start
      setBackground(Color.BLACK)
      setForeground(Color.WHITE)
      setOpaque(true)
      setText("Starting...")
      var countTime = 0
      def actionPerformed(event: ActionEvent) {
        if (currentTime <= countTime) {
          assert(hasStep)
          for (w <- world) w.reset
          updateWorld()
          if (historyContinues) updateHistory()
          frame.repaint()
          next
          val previousTime = currentTime
          while (!agenda.isEmpty && agenda.head.time == previousTime) next
        } else if (historyContinues && !history.isEmpty) {
          history = history.head :: history
        }
        if (!history.isEmpty) {
          setText("Day " + countTime + ": " +
            history.head.healthy + " healthy(G), " +
            history.head.infected + " infected(M), " +
            history.head.sick + " sick(O), " +
            history.head.dead + " dead(R), " +
            history.head.immune + " immune(Y), " +
            history.head.vaccinated + " vaccinated(B).")
        }
        populationGraph.repaint()
      	countTime += 1
        if (countTime == 150) println("Dead people on day 150: "+persons.count(p => p.dead))
      }
    }
    setContentPane(new JComponent {
      setBorder(new EmptyBorder(worldBorderSize, worldBorderSize, worldBorderSize, worldBorderSize))
      setLayout(new BorderLayout)
      add(populationGraph, BorderLayout.SOUTH)
      add(roomDisplay, BorderLayout.CENTER)
      add(clock, BorderLayout.NORTH)
    })
    pack
    setResizable(false)
    setVisible(true)
    println("Scaliosis is ready to spread")
    clock.start
    override def paint(g: Graphics) {
      for (room <- rooms)
        room setSituation world(room.worldRow, room.worldCol)
      super.paint(g)
    }
  }

}
