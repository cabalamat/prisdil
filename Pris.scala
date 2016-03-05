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
case object None extends Move

/* a Round is a move by both players */
type Round = (Move,Move)

/* a Log is a list of Rounds -- it is what has happened in the
game so far. The most recent move goes at the head of the list.
The log is from the player's point of view so it is
(myMoves, otherPlayersMoves)
*/
type Log = (List[Move],List[Move])


//--------------------------------------------------------------------
// Strategies

/* a Strategy takes in a Log, and returns that player's next move */
trait Strategy {
    /* within a round, it is (myMove,otherPlayersMove) */
    def nextMove(log: Log): Move

    //----- helper functions:

    def otherPrev(log:Log, i:Int =1):Move = {
        /* otherPrev(log,1) == the other player's last move
           otherPrev(log,2) == the other player's last but one move,
           etc.
        */
        val otherMoves = log._2
        if (i < 1 || i > otherMoves.length)
            None
        else
            otherMoves(i-1)
    }

    /* note that we have to use split("\\$") because the argument
       is a regexp. */
    def name = this.getClass.getName.split("\\$").last
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
        case (_, Nil) => Cooperate
        case (_, m::ms) => m
    }
}

/* punish twice for defection (2 tits for tat) */
class Punisher extends Strategy {
    def nextMove(log: Log) = {
        if (otherPrev(log,1)==Defect || otherPrev(log,2)==Defect)
            Defect
        else
            Cooperate
    }
}

/* punish only if opponent defected in both his last two moves */
class Lenient extends Strategy {
    def nextMove(log: Log) = {
        if (otherPrev(log,1)==Defect && otherPrev(log,2)==Defect)
            Defect
        else
            Cooperate
    }
}


/* randomly C or D, 50% prob of each */
class RandomStrat extends Strategy {
    def nextMove(log: Log) = {
        if (scala.util.Random.nextInt(2)==0)
            Defect
        else
            Cooperate
    }
}


//--------------------------------------------------------------------
// A game

/* play a game between two strategies and return the result */
def game(s1: Strategy, s2:Strategy, numMoves: Int): Log = {
    var log: Log = (Nil,Nil) // from s1's point of view
    for (movNum <- 1 to numMoves){
        val s1move: Move = s1.nextMove(log)
        val s2move: Move = s2.nextMove(switchSide(log))
        log = (s1move::log._1,s2move::log._2)
    }
    log
}

/* switch the players in a log */
def switchSide(log: Log): Log = {
    return (log._2,log._1)
}

/* return the scores of a game */
def getScores(log: Log): (Int, Int) =
    (getScore(log), getScore(switchSide(log)))


/* get the score for player 1 */
def getScore(log: Log): Int = {
    val zippedLog = log._1 zip log._2
    zippedLog.map{moveScore(_)}.sum
}

def moveScore(round: Round): Int = round match {
    case (Cooperate,Cooperate) => 3
    case (Cooperate,Defect)    => 0
    case (Defect,Cooperate)    => 5
    case (Defect,Defect)       => 1
    case (_,_)                 => 0
}

//--------------------------------------------------------------------
/* a tournament */

val gameLength = 20
val strats = List(new AllC, new AllD, new RandomStrat,
    new TitForTat, new Punisher, new Lenient)
    
// number of rounds that each strategy plays    
val numRounds = gameLength * strats.length    

def printStrats = {
    for (s <- strats){
        val sn = s.name
        println(s"Strategy: ${sn}  ${playStrat(s)}")
    }
}

/* get a score for each strat, being the average score per round */
def playStrats: List[Int] = {
    for (st <- strats) yield playStrat(st)
}
def playStrat(st: Strategy): Int = {
    //val scores = strats.map({getScore(game(st, _, gameLength))})
    //scores.sum
    val scores = for (op <- strats) yield getScore(game(st, op, gameLength))
    scores.sum
    
}


//--------------------------------------------------------------------
// play a game

def main(args: Array[String]) = {
    printStrats
/*
    val tft = new TitForTat()
    val ad = new AllD()
    var log = game(tft, ad, 10)
    println("Result of game:")
    println(log)
    println(s"Scores: ${getScores(log)}")
*/
}


}/*-----object Pris-----*/
/*end*/
