object OOTicTacToe {
  class Board {
    var _result = "UNDECIDED"
    var _squares = "0123456789"

    def move(who:Char, square:Int):Boolean = {
      if(square < 0 || square > 8 ||
        _squares.charAt(square) == 'X' ||
        _squares.charAt(square) == 'O') {
        false
      }
      else {
        _squares = _squares.slice(0, square) + who + _squares.slice(square+1, 9)
        true
      }
    }
  }

  class Player(id: Char, board: Board) {
    def makeMove() = {
      println("Please select a numbered square: ")
      var bMoved = false
      var line = ""
      while(!bMoved) {
        try {
          line = readLine
          bMoved = board.move(id, line.toInt)
        }
        catch {
          case _ => {
          }
        }
      }
    }
  }
}
