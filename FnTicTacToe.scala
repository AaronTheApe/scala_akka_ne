object FnTicTacToe {
  def newGame():String = (0 until 9).map(_ => " ").mkString

  def legalInput(input: String):Boolean = {
    (0 until 9).map(_.toString).count(_ == input) != 0
  }

  def move(board: String, player: Char, square: Int):String = {
    board.slice(0, square) + player + board.slice(square+1, 9)
  }

  val topBottom = "\n   |   |   \n"
  val verticalSpacer = "\n-----------\n"

  def checkHorizontal(board:String):Char = {
    val winnersList = (0 until 9 by 3).map(x => {
      if(board(x) == board(x+1) && board(x) == board(x+2))
        board(x)
      else
        ' '
    }).filter(_ != ' ')
    if(winnersList.size != 0)
      winnersList(0)
    else
      ' '
  }

  def checkVertical(board:String):Char = {
    val winnersList = (0 until 3).map(x => {
      if(board(x) == board(x+3) && board(x) == board(x+6))
        board(x)
      else
        ' '
    }).filter(_ != ' ')
    if(winnersList.size != 0)
      winnersList(0)
    else
      ' '
  }

  def checkDiagonal(board:String):Char = {
    val center = board(4)
    if((board(0) == center && board(8) == center) ||
      (board(2) == center && board(6) == center))
      center
    else
      ' '
  }

  def checkWinner(board:String):Char = {
    val winnersList = 
      ("" +
        checkHorizontal(board) +
        checkVertical(board) +
        checkDiagonal(board)).filter(_ != ' ')
    if(winnersList.size != 0)
      winnersList(0)
    else
      ' '
  }

  def boardFull(board:String):Boolean = {
    board.count(_ == ' ') == 0
  }

  def gameOver(board:String):Boolean = {
    boardFull(board) || checkWinner(board) != ' '
  }

  def rowToString(board:String, rowNum:Int) = {
    val (i0, i1, i2) = (0 + rowNum*3, 1 + rowNum*3, 2 + rowNum*3)
    val (a, b, c) = (board(i0), board(i1), board(i2))
    " " + a + " | " +  b  + " | " + c + " " 
  }

  def renderBoard(board:String):String = {
    topBottom +
    rowToString(board, 0) +
    verticalSpacer +
    rowToString(board, 1) +
    verticalSpacer +
    rowToString(board, 2) +
    topBottom
  }

  def getMove(player: Char):Int = {
    println("Enter # of square to move " + player + ":")
    val input = readLine
    if(legalInput(input))
      input.toInt
    else
      getMove(player)
  }

  def playGame(board:String, player:Char):Unit = {
    println(board)
    println(renderBoard(board))
    if(gameOver(board)) {
      val winner = checkWinner(board)
      if(winner == 'X' || winner == 'O')
        println("Congrats to " + winner)
      else
        println ("Tie game")
    }
    else {
      val newBoard = move(board, player, getMove(player))
      val newPlayer = player match {
        case 'X' => 'O'
        case _ => 'X'
      }
      playGame(newBoard, newPlayer)
    }
  }

  def main(args:Array[String]) = {
    playGame(newGame, 'X')
  }
}
