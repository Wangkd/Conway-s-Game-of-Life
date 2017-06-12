/**
 * Created by Wangkd on 10/9/15.
 */

import scala.swing._
import scala.swing.event._
import java.awt.{Color,Graphics2D,BasicStroke}
import scala.collection.mutable.ArrayBuffer
import scala.swing.MainFrame


object gameoflife extends SimpleSwingApplication{
  var width = 16
  var height = 16
  var running = false
  var array = Array.ofDim[Boolean](width,height)
  var panel: MainPanel = new MainPanel(array)

  def top =new MainFrame(){
    def restrictHeight(s: Component) {
      s.maximumSize = new Dimension(s.preferredSize.width, s.preferredSize.height)
    }
    title = "Conway's Game Of Life"
    var stepCount = 0  //save the current step number
    var currentStep = new Label("0")
    //implement button set to control the steps of game running time
    var stepChoice1 = new RadioButton("Infinity")
    var stepChoice2 = new RadioButton("FixedNum:")
    var ChoiceGroup = new ButtonGroup(stepChoice1, stepChoice2)
    var stepNum = new TextField{columns = 4}

    val startBtn = new Button("start")
    val nextStepBtn = new Button("next step")
    val resetBtn = new Button("reset")
    //set the time span between every two steps
    var timer = new javax.swing.Timer(500,Swing.ActionListener(e =>{
      step()
      stepCount += 1
      currentStep.text = stepCount.toString
    }))
    //stop the game when the step reaches the specific steps
    var timer2 = new javax.swing.Timer(100,Swing.ActionListener(e =>{
      if(stepCount >= stepNum.text.toInt && stepChoice2.selected){
        timer.stop()
        startBtn.text = "start"
        nextStepBtn.enabled = true
      }

    }))
    restrictHeight(stepNum)

    //add buttons and main panel to the frame
    contents = new BoxPanel(Orientation.Vertical){
      contents += panel
      contents += Swing.VStrut(10)
      contents += Swing.VGlue
      //ControlPanels
      contents += new BoxPanel(Orientation.Horizontal){
        contents += new Label("Steps:")
        contents += Swing.HStrut(3)
        contents += stepChoice1
        contents += stepChoice2
        contents += stepNum
        contents += Swing.HGlue
        contents += new Label("CurrentStep:")
        contents += currentStep
        contents += Swing.HStrut(100)
      }
      contents += new BoxPanel(Orientation.Horizontal){
        contents += startBtn
        contents += Swing.HStrut(30)
        contents += nextStepBtn
        contents += Swing.HStrut(30)
        contents += resetBtn
        contents += Swing.HGlue
        contents += Button("Quit"){sys.exit(0)} // exit the application after clicking the Quit button
        for (e <- contents)
          e.yLayoutAlignment = 0.0
      }
      border = Swing.EmptyBorder(10, 10, 10, 10)
    } // end of contents

    listenTo(panel)
    listenTo(nextStepBtn)
    listenTo(stepChoice1)
    listenTo(stepChoice2)
    listenTo(stepNum)
    listenTo(startBtn)
    listenTo(resetBtn)

    reactions += {
      //change the configuration of the game by clicking
      case golEvent(x, y) =>{
        array(x)(y) = !array(x)(y)
        panel.repaint()
      }

      case ButtonClicked(`startBtn`) => {
        if(stepChoice1.selected || stepChoice2.selected)
          startGame()
        println("Game Started!!!")
      }

      case ButtonClicked(`nextStepBtn`) => {
        step()
      }
      //set all cells to death after clicking the reset button
      case ButtonClicked(`resetBtn`) =>{
        for(x <- 0 to array.length - 1){
          for(y <- 0 to array.length - 1)
            array(x)(y) = false
        }
        timer.stop()
        timer2.stop()
        currentStep.text = "0"
        startBtn.text = "start"
        stepCount = 0
        panel.repaint()
      }
    }//end of reactions

    //start(stop) the game after clicking the start(stop) button
    def startGame(): Unit ={
      if(startBtn.text == "start"){
        running = true
        nextStepBtn.enabled = false
        startBtn.text = "stop"
        timer.start()
        if(stepChoice2.selected){
          timer2.start()
        }
      }
      else{
        nextStepBtn.enabled = true
        timer.stop()
        timer2.stop()
        //stepCount = 0
        //currentStep.text = stepCount.toString
        startBtn.text = "start"
        running = false
      }
    }
  }

  //single-step the game
  def step(): Unit = {
    val newGrid = Array.ofDim[Boolean](width,height)
    for (x <- 0 to array.length-1) {
      for (y <- 0 to array.length-1) {
        var neighborCount = 0
        if (x > 0 && y > 0 && array(x - 1)(y - 1))
          neighborCount += 1
        if (x < array.length - 1 && y > 0 && array(x + 1)(y - 1))
          neighborCount += 1
        if (y > 0 && array(x)(y - 1))
          neighborCount += 1
        if (x > 0 && array(x - 1)(y))
          neighborCount += 1
        if (x < array.length - 1 && array(x + 1)(y))
          neighborCount += 1
        if (x > 0 && y < array.length - 1 && array(x - 1)(y + 1))
          neighborCount += 1
        if (y < array.length - 1 && array(x)(y + 1))
          neighborCount += 1
        if (x < array.length - 1 && y < array.length - 1 && array(x + 1)(y + 1))
          neighborCount += 1
        if(array(x)(y) == true){
          if(neighborCount == 2 || neighborCount == 3)
            newGrid(x)(y) = true
          else
            newGrid(x)(y) = false
        }
        else{
          if(neighborCount == 3)
            newGrid(x)(y) = true
          else
            newGrid(x)(y) = false
        }
      }
    }
    array = newGrid
    panel.setArray(newGrid)
    panel.repaint()
  }

  /* val frame = new JFrame("Conway's Game of Life")
   frame.setSize(600,600)
   frame.setResizable(false)
   frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
   //frame.setLayout(new BorderLayout())
   //frame.add(panel, BorderLayout.CENTER)
   frame.setVisible(true)*/
}
