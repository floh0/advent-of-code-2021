import scala.io.Source

object Day11 {
    val input: Seq[Seq[Int]] = Source.fromFile("day11.input")
        .getLines
        .map(_.map(_.toString.toInt))
        .toSeq

    val h = input.size
    val w = input(0).size

    def chainReaction(initial: Seq[Seq[Int]]): LazyList[Seq[Seq[Option[Int]]]] =
        initial.map(_.map(v => Some(v+1))) #:: chainReaction(initial)
            .map { s =>
                val impacted = (
                    for {
                        i <- 0 until h
                        j <- 0 until w if s(i)(j).map(_ > 9).getOrElse(false)
                        k <- i-1 to i+1
                        l <- j-1 to j+1 if k >= 0 && k < h && l >= 0 && l < w && (k, l) != (i, j)
                    } yield (k, l)
                ).groupBy(e => e).view.mapValues(_.size)
                s.map(_.map {
                    case Some(v) if v > 9 => None
                    case e => e
                }).map(_.zipWithIndex).zipWithIndex.map { case (a, i) =>
                    a.map { 
                        case (Some(a), j) => Some(a+impacted.getOrElse((i, j), 0))
                        case (e, _) => e
                    }
                }
            }

    def number(state: Seq[Seq[Option[Int]]]): Int =
        state.flatten.count(_.isEmpty)

    def states: LazyList[(Seq[Seq[Int]], Int)] = (input, 0) #:: states.map { 
        case (s, _) =>
            val next = chainReaction(s).map(v => (v, number(v)))
            next.zip(next.drop(1))
                .dropWhile { case ((_, a), (_, b)) => a != b }
                .map { case ((v, c), (_, _)) => (v.map(_.map(_.getOrElse(0))), c) }
                .head
    }

    def part1(iter: Int): Int = states
        .take(iter+1)
        .foldLeft(0){ case (a, (_, v)) => a+v }

    def part2(): Int = states
        .takeWhile { case (s, n) => s.flatten.size != n }
        .size

    def main(args: Array[String]): Unit = {
        println(part1(100))
        println(part2())
    }
}