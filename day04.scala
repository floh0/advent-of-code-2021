import scala.io.Source
import scala.util.matching.Regex

object Day4 {
    val (order: LazyList[Int], boards: Seq[Seq[Seq[Either[Int, Int]]]]) = Source.fromFile("day04.input")
        .mkString
        .split(s"${System.lineSeparator}${System.lineSeparator}")
        .toSeq match {
            case h +: t => 
                val nr = "[0-9]+".r
                val o = h.split(",").map(_.toInt).to(LazyList)
                val b = t.map(_.split(System.lineSeparator).toSeq.map(bl => nr.findAllIn(bl).map(v => Left(v.toInt): Either[Int, Int]).toSeq))
                (o, b)
        }

    def bingo(board: Seq[Seq[Either[Int, Int]]]): Option[Int] = {
        val s = board.size
        (0 until s).map { i => (0 until s)
            .map(j => (board(i)(j), board(j)(i)))
            .unzip
        }
            .flatMap { case (a, b) => Seq(a, b) }
            .flatMap(_.foldLeft(Some(()): Option[Unit]) { 
                case (None, _) => None
                case (Some(_), Left(_)) => None
                case (s, Right(_)) => s
            })
            .headOption
            .map(_ => board.flatten.foldLeft(0) {
                case (x, Left(y)) => x+y
                case (x, Right(_)) => x 
            })
    }

    def update(boards: Seq[Seq[Seq[Either[Int, Int]]]], n: Int): Seq[Seq[Seq[Either[Int, Int]]]] =
        boards.map(_.map(_.map {
            case Left(m) if m == n => Right(m): Either[Int, Int]
            case o => o
        }))

    def rounds: LazyList[(Option[Int], Seq[Option[Int]], Seq[Seq[Seq[Either[Int, Int]]]])] =
        (None, Seq(None), boards) #:: rounds.zip(order).map { 
            case ((_, _, b), n) => 
                val r = update(b, n)
                (Some(n), r.map(bingo), r) 
        }

    def part1(): Option[(Int, Int)] =
        rounds.dropWhile { case (_, w, bs) => w.flatten.isEmpty }.headOption.flatMap {
            case (n, w, _) => w.flatten.headOption.flatMap(l => n.map(v => (l, v)))
        }

    def part2(): Option[(Int, Int)] = {
        val (a, b) = rounds.span { case (_, w, bs) => w.exists(_.isEmpty) }
        for {
            (_, ow, _) <- a.lastOption
            (n, nw, _) <- b.headOption
            (nv, _) <- nw.zip(ow).find { case (x, y) => x.isDefined && y.isEmpty}
            a <- nv
            b <- n
        } yield (a, b)
    }

    def main(args: Array[String]): Unit = {
        val Some(l1, v1) = part1()
        println(s"$l1, $v1, ${l1*v1}")

        val Some(l2, v2) = part2()
        println(s"$l2, $v2, ${l2*v2}")
    }
}