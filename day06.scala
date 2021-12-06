import scala.io.Source

object Day6 {
    val input: Seq[Int] = Source.fromFile("day06.input")
        .getLines
        .toSeq
        .flatMap(_.split(",").map(_.toInt))

    def states: LazyList[Seq[(Int, Long)]] =
        input.groupBy(x => x).view.mapValues(_.size.toLong).toSeq #:: states.map(_
            .flatMap { case (k, v) => if (k == 0) Seq((8, v), (6, v)) else Seq((k-1, v)) }
            .groupBy { case (k, _) => k }
            .view
            .mapValues(_.foldLeft(0L) { case (n, (_, v)) => n+v })
            .toSeq
        )

    def after(days: Int): Long = states.take(days+1).lastOption.getOrElse(Seq.empty).foldLeft(0L) { case (n, (_, v)) => n+v }

    def main(args: Array[String]): Unit = {
        val part1 = after(80)
        println(part1)
        val part2 = after(256)
        println(part2)
    }
}