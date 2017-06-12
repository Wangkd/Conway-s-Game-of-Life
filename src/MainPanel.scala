/**
 * Created by Wangkd on 10/9/15.
 */

import scala.swing._
import scala.swing.event._
import java.awt.{Color,Graphics2D,BasicStroke}
import java.awt.geom._

//implement the main grid panel where the game simulator runs
class MainPanel(var grid: Array[Array[Boolean]]) extends Panel{
  preferredSize = new Dimension(640, 640)
  listenTo(mouse.clicks)
  reactions += {
    case MouseClicked(_, p, _, _, _) => mouseClick(p.x, p.y)
  }

  def setArray(newGird: Array[Array[Boolean]]): Unit ={
    grid = newGird
  }

  //find the exact grid that is clicked on
  def mouseClick(x: Int, y: Int): Unit = {
    val (squareSide, x0, y0, wid) = squareGeometry
    if (x0 <= x && x < x0 + squareSide &&
      y0 <= y && y < y0 + squareSide) {
      val col = (x - x0) / wid
      val row = (y - y0) / wid
      publish(golEvent(col, row))
      println("button Pressed at "+ col + " "+ row)
    }

  }

  private def squareGeometry: (Int, Int, Int, Int) = {
    val d = size
    val squareSide = d.height min d.width
    val x0 = (d.width - squareSide)/2
    val y0 = (d.height - squareSide)/2
    (squareSide, x0, y0, squareSide/grid.length)
  }

  //paint the grid, set the dead grid white and the live one black
  override def paintComponent(g: Graphics2D)= {
    val (squareSide, x0, y0, wid) = squareGeometry
    g.setColor(Color.WHITE)
    g.fillRect(0,0, size.width, size.height)
    g.setColor(Color.GRAY)
    for (x <- 0 to grid.length) {
      // g.draw(new Line2D.Double(x0 + x * wid+1, y0, x0 + x * wid+1, y0 + squareSide))
      g.draw(new Line2D.Double(x0 + x * wid, y0, x0 + x * wid, y0 + squareSide))
      //g.draw(new Line2D.Double(x0 + x * wid-1, y0, x0 + x * wid-1, y0 + squareSide))
    }
    // horizontal lines
    for (y <- 0 to grid.length)
      g.draw(new Line2D.Double(x0, y0 + y * wid, x0 + squareSide, y0 + y * wid))
    g.setColor(Color.BLACK)
    for(i <- 0 to (grid.length-1)) {
      for(j <- 0 to (grid(0).length-1)){
        if(grid(i)(j) == true){
          g.fillRect(x0+i*wid,y0+j*wid,wid,wid)
        }
      }
    }
  }
}

case class golEvent(x: Int, y: Int) extends Event
