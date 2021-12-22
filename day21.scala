import scala.collection.mutable.HashMap
import scala.io.Source

object Day21 {
    val (input1: Int, input2: Int) = Source.fromFile("day21.input")
        .getLines
        .toArray
        .map {
            case s"Player $x starting position: $p" => p.toInt
        } match {
            case Array(a, b) => (a, b)
        }

    extension(i: Int) {
        def truncate(max: Int): Int = if (i > max) (i-max).truncate(max) else i
        def swap: Int = if (i == 1) 2 else 1
    }
    extension(a: (Long, Long)) {
        def `*`(b: Int): (Long, Long) = (a._1*b, a._2*b)
    }

    val deterministicDie: LazyList[Int] = 1 #:: deterministicDie.map(v => if (v == 100) 1 else v+1)

    def rounds(turn: Int, p1: Int, s1: Int, p2: Int, s2: Int, die: Iterator[Int], rolls: Int): (Int, Int) = {
        if (s1 >= 1000) (s2, rolls)
        else if (s2 >= 1000) (s1, rolls)
        else {
            val (pc, sc) = if (turn == 1) (p1, s1) else (p2, s2)
            val pnew = (pc + die.next + die.next + die.next).truncate(10)
            if (turn == 1) 
                rounds(turn.swap, pnew, s1+pnew, p2, s2, die, rolls+3)
            else 
                rounds(turn.swap, p1, s1, pnew, s2+pnew, die, rolls+3)           
        }
    }

    def part1(): (Int, Int) = rounds(1, input1, 0, input2, 0, deterministicDie.iterator, 0)

    val dejaVu: HashMap[(Int, Int, Int, Int, Int), (Long, Long)] = HashMap[(Int, Int, Int, Int, Int), (Long, Long)]()

    def multiverse(turn: Int, p1: Int, s1: Int, p2: Int, s2: Int): (Long, Long) = {
        if (dejaVu.contains((turn, p1, s1, p2, s2))) dejaVu((turn, p1, s1, p2, s2))
        else if (s1 >= 21) (1L, 0)
        else if (s2 >= 21) (0, 1L)
        else {
            val (pc, sc) = if (turn == 1) (p1, s1) else (p2, s2)
            val result = Seq.tabulate(3,3,3)((x,y,z) => 3+x+y+z)
                .flatten
                .flatten
                .groupBy(identity)
                .view.mapValues(_.size)
                .toSeq
                .map {
                    case (roll, count) =>
                        val pnew = (pc + roll).truncate(10)
                        if (turn == 1) 
                            multiverse(turn.swap, pnew, s1+pnew, p2, s2) * count
                        else 
                            multiverse(turn.swap, p1, s1, pnew, s2+pnew) * count                      
                }
                .foldLeft((0L, 0L)) {
                    case ((a, b), (c, d)) => (a+c, b+d)
                }
            dejaVu.update((turn, p1, s1, p2, s2), result)
            result
        }
    }

    def part2(): (Long, Long) = multiverse(1, input1, 0, input2, 0)

    def main(args: Array[String]): Unit = {
        val (score, rolls) = part1()
        println(s"$score,$rolls,${score*rolls}")
        val (player1, player2) = part2()
        println(s"${player1.min(player2)},${player1.max(player2)}")
    }
}