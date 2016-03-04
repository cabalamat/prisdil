/* Pris.scala */

/**
Scala program to play Iterated Prisoners' Dilemma.
See <http://en.wikipedia.org/wiki/Prisoner%27s_dilemma>.

Start with 3 strategies: AllC, AllD, TFT.

They take one parameter, a list of pairs. Each pair is a previous round
of the game (new rounds first) with the strategy's move before the
opponent's.
Eg. [(D,D), (C,D)]
Means: move 1, we cooperated, opponent defected;
       move 2, both defected

*/

object Pris {/*-----object-----*/

/** a single move by one player */
sealed abstract class Move
case object Cooperate extends Move
case object Defect extends Move

/* a Round is a move by both players */
type Round = (Move,Move)

/* a Log is a list of Rounds -- it is what has happened in the
game so far. The most recent move goes at the head of the list. */
type Log = List[Round]


//--------------------------------------------------------------------
// Strategies

/* a Strategy takes in a Log, and returns that player's next move */
trait Strategy {
    /* within a round, it is (myMove,otherPlayersMove) */
    def nextMove(log: Log): Move
}

/* always defect */
class AllD extends Strategy {
    def nextMove(log: Log) = Defect
}

/* always cooperate */
class AllC extends Strategy {
    def nextMove(log: Log) = Cooperate
}

/* C first, then do what the other player did last time */
class TitForTat extends Strategy {
    def nextMove(log: Log) = log match {
        case Nil => Cooperate
        case (me,other)::rs => other /* other player's last move */
    }
}

//--------------------------------------------------------------------
// A game

/* play a game between two strategies and return the result */
def game(s1: Strategy, s2:Strategy, numMoves: Int): Log = {
    var log: Log = List() // from s1's point of view
    for (movNum <- 1 to numMoves){
        val s1move: Move = s1.nextMove(log)
        val s2move: Move = s2.nextMove(switchSide(log))
        log = (s1move,s2move)::log
    }
    log
}

/* switch the players in a log */
def switchSide(log: Log): Log = {
    for ((m1,m2) <- log) yield (m2,m1)
}

/* return the scores of a game */
def getScores(log: Log): (Int, Int) =
    (getScore(log), getScore(switchSide(log)))

/* get the score for player 1 */
def getScore(log: Log): Int =
    log.map{moveScore(_)}.sum

def moveScore(round: Round): Int = round match {
    case (Cooperate,Cooperate) => 3
    case (Cooperate,Defect)    => 0
    case (Defect,Cooperate)    => 5
    case (Defect,Defect)       => 1
}


//--------------------------------------------------------------------
// play a game

def main(args: Array[String]) = {
    val tft = new TitForTat()
    val ad = new AllD()
    var log = game(tft, ad, 10)
    println("Result of game:")
    println(log)
    println(s"Scores: ${getScores(log)}")
}


}/*-----object Pris-----*/
/*end*/
