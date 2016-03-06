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
    
    /* psuedo-random number generator that uses the log */
    def prng(log: Log): scala.util.Random = {
        val seed = scala.util.hashing.MurmurHash3.stringHash(s"$log")
        new scala.util.Random(seed)
    }
    
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
        if (prng(log).nextInt(2)==0)
            Defect
        else
            Cooperate
    }
}

/* mostly TFT, but defect randomly 10% of the time */
class MostlyTFT extends TitForTat {
    override def nextMove(log: Log) = {
        if (prng(log).nextInt(10)==0)
            Defect
        else
            super.nextMove(log)
    }
}


/* a deliberately bad strategy */
class Bad extends Strategy {
    def nextMove(log: Log) = {
        if (log._1.length < 2)
            Defect
        else
            Cooperate
    }
}

/* TFT, but defect on last (defectLast) moves */
class DefAtEnd(defectLast: Int) extends TitForTat {
    override def name = 
        super.name + s"($defectLast)"
    override def nextMove(log: Log) = {
        val defectZone = gameLength - log._1.length <= defectLast
        if (defectZone)
            Defect
        else
            super.nextMove(log)
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
    //println(s"${s1.name} ${s2.name} ${log}")
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

val gameLength = 200
val strats = List(new AllC, new AllD, new RandomStrat,
    new TitForTat, new Punisher, new Lenient, new MostlyTFT,
    new DefAtEnd(1), new DefAtEnd(2), new DefAtEnd(3),
    new Bad)
    
// number of rounds that each strategy plays    
val numRounds = gameLength * strats.length    

def playStrat(st: Strategy): Int = {
    //val scores = strats.map({getScore(game(st, _, gameLength))})
    //scores.sum
    val scores = for (op <- strats) yield getScore(game(st, op, gameLength))
    scores.sum 
}

/* get a score for each strat, being the average score per round */
def playStrats: List[Int] = {
    for (st <- strats) yield playStrat(st)
}

def printStrats = {
    val scores = playStrats
    val stratScores: List[(String, Int)] = strats.map(_.name) zip scores
    val sortedScores = stratScores.sorted(
        Ordering[(Int, String)].on(
            (x:(String, Int)) => (-x._2, x._1)))
    for (nameScore <- sortedScores) {
        val stratName = nameScore._1
        val score = nameScore._2
        val avScore:Double = score * 1.0 / numRounds
        println(f"$stratName%-12s $score%4d  $avScore%6.4f")   
    }
}
    
//--------------------------------------------------------------------
// play a game

def main(args: Array[String]) = {
    printStrats
}


}/*-----object Pris-----*/
/*end*/
