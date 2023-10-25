import scala.swing._
import event._
import BorderPanel.Position._
import java.awt.Graphics2D
import java.awt.geom.Rectangle2D
import java.awt.BasicStroke
import java.awt.Color
import java.awt.geom.GeneralPath
import scala.util.Random
import java.awt.geom.Line2D



case class Pos(i: Int, j: Int)

class Cell(val pos: Pos, val radius: Double, var color: Color = Color.white):
  val x = pos.i * radius
  val y = pos.j * radius
  var alive: Boolean = false
  var neighbors = List[Cell]()

  override def toString() = f"($pos $alive)"
  // two different color states
  def nextState() =
    color match
      case Color.white => 
        color = Color(90, 90, 90)
        alive = true
      case _  => 
        color = Color.white
        alive = false

  def kill() = 
    color = Color.white
    alive = false

  def addNeighbor(cell: Cell) =
    neighbors = cell :: neighbors

  def stillAlive() =
    val aliveNeighbors = neighbors
      .map(c => if c.alive then 1 else 0)
      .foldLeft(0)((a,b) => a + b)

    if aliveNeighbors == 2 || aliveNeighbors == 3 then true else false

  def stillDead() = 
    val aliveNeighbors = neighbors
      .map(c => if c.alive then 1 else 0)
      .foldLeft(0)((a,b) => a + b)

    if aliveNeighbors != 3 then true else false



// method to generate a grid of cells with their own logic
def generateGrid(radius: Double, numCells: Int) =
  // defines base parts of the Cell Grid
  val length = numCells - 1
  val cellGrid = Array.ofDim[Cell](numCells, numCells)

  for (i <- 0 until numCells; j <- 0 until numCells)
    cellGrid(i)(j) = new Cell(Pos(i, j), radius)
    // creates the neighbors of the program
    if i != 0 then  
      cellGrid(i)(j).addNeighbor(cellGrid(i-1)(j))
      cellGrid(i-1)(j).addNeighbor(cellGrid(i)(j))
      if j != length then
        cellGrid(i)(j).addNeighbor(cellGrid(i-1)(j+1))
        cellGrid(i-1)(j+1).addNeighbor(cellGrid(i)(j))

    if j != 0 then
      cellGrid(i)(j).addNeighbor(cellGrid(i)(j-1))
      cellGrid(i)(j-1).addNeighbor(cellGrid(i)(j))

    if i != 0 && j != 0 then
      cellGrid(i)(j).addNeighbor(cellGrid(i-1)(j-1))
      cellGrid(i-1)(j-1).addNeighbor(cellGrid(i)(j))
    
  cellGrid


@main def run() : Unit = 

  val numOfCells: Int = 50
  val screenSize: Double = 900.0
  val radius: Double = screenSize / numOfCells
  var running: Boolean = false
  
  def pointToIndex(x: Double, y: Double): Pos =
    var i: Int = (x / radius).toInt
    var j: Int = (y / radius).toInt
    
    if i < 0 then i = 0
    else if i >= numOfCells then i = numOfCells-1

    if j < 0 then j = 0
    else if j >= numOfCells then j = numOfCells-1
    
    Pos(i, j)

  val cellGrid = generateGrid(radius, numOfCells)
  val cellSet = collection.mutable.Set[Pos]()
  val savedGrid = generateGrid(radius, numOfCells)
  var clickPos = Pos(0, 0)
  // the current cell being altered
  var current: Pos = pointToIndex(screenSize/2, screenSize/2)

  // creates a new object that extends Panel
  val drawPanel = new Panel {
    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)   // fixes repaint bug

      for (i <- 0 until numOfCells; j <- 0 until numOfCells)
        val oneCell = new Rectangle2D.Double(cellGrid(i)(j).x, cellGrid(i)(j).y, radius, radius)
        g.setColor(cellGrid(i)(j).color)
        g.fill(oneCell)
      // grid lines
      for (i <- 0 until numOfCells)
        g.setColor(Color.black)
        g.draw(new Line2D.Double(i*radius, 0, i*radius, screenSize))
        g.draw(new Line2D.Double(0, i*radius, screenSize, i*radius))
      // red selection box
      g.setColor(Color.red)
      g.draw(new Rectangle2D.Double(current.i*radius, current.j*radius, radius, radius))
    }

    listenTo(mouse.clicks, mouse.moves, this.keys)
    
    reactions += {
      case e: MousePressed =>
        clickPos = pointToIndex(e.point.x, e.point.y)   // for the mousedragged logic
        cellGrid(clickPos.i)(clickPos.j).nextState()
        cellSet += clickPos
        repaint()
      case e: MouseDragged =>
        val position = pointToIndex(e.point.x, e.point.y)
        if cellSet add position then {cellGrid(position.i)(position.j).nextState(); repaint()}
      case e: MouseReleased =>
        cellSet.clear()
        
      case e: KeyPressed =>
        e.key match {
          // pauses the automata
          case Key.P     => running = !running
          // clears the screen
          case Key.C     => {
            cellGrid.foreach(_.foreach(_.kill()))
            repaint()}
          case Key.Space => {cellGrid(current.i)(current.j).nextState();         repaint()}
          case Key.Up    => {current = Pos(current.i, current.j-1);              repaint()}
          case Key.Down  => {current = Pos(current.i, current.j+1);              repaint()}
          case Key.Right => {current = Pos(current.i+1, current.j);              repaint()}
          case Key.Left  => {current = Pos(current.i-1, current.j);              repaint()}
        }
    }
    focusable = true
    requestFocus()
    preferredSize = new Dimension(screenSize.toInt, screenSize.toInt)
  }

  val timer = new javax.swing.Timer(200, Swing.ActionListener(e => {
    if running then

      var changedCells = List[Cell]()
      for (i <- 0 until numOfCells; j <- 0 until numOfCells)
        // if alive
        if cellGrid(i)(j).alive && !cellGrid(i)(j).stillAlive() then
          changedCells = cellGrid(i)(j) :: changedCells
        // if dead
        if !cellGrid(i)(j).alive && !cellGrid(i)(j).stillDead() then
          changedCells = cellGrid(i)(j) :: changedCells

      changedCells.foreach(c => c.nextState())
      drawPanel.repaint()
  }))

  val frame = new MainFrame {
    title = "Game of Life"
    contents = new BorderPanel {
      layout += drawPanel -> Center
    }
    centerOnScreen()
  }

  frame.open()
  drawPanel.requestFocus()
  timer.start()
