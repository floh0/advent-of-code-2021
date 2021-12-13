import scala.io.Source

object Day13 {
    val (input: Seq[(Int, Int)], folds: Seq[(String, Int)]) = Source.fromFile("day13.input")
        .mkString
        .split(s"${System.lineSeparator}${System.lineSeparator}")
        .map(_.split(System.lineSeparator).toSeq) match {
            case Array(c, f) => (
                c.map(_.split(",").map(_.toInt)).map { case Array(a, b) => (a, b) }, 
                f.map(_.replace("fold along ", "").split("=")).map { case Array(a, b) => (a, b.toInt) }
            )
        }

    def step(state: Seq[(Int, Int)], side: String, value: Int) =
        state.collect(
            if (side == "y") {
                case (a, b) if b < value => (a, b)
                case (a, b) if b > value => (a, 2*value-b)
            } else {
                case (a, b) if a < value => (a, b)
                case (a, b) if a > value => (2*value-a, b)
            }
        ).distinct

    def part1(): Int = {
        val (s, v) = folds.head
        step(input, s, v).size
    }

    def part2(): String = {
        val r = folds.foldLeft(input) { case (a, (s, v)) => step(a, s, v) }
        (0 to r.map { case (_, b) => b }.max).map(j =>
            (0 to r.map { case (a, _) => a }.max).map(i =>
                if (r.contains(i,j)) '#' else '.'
            ).mkString
        ).mkString(System.lineSeparator)
    }

    def main(args: Array[String]): Unit = {
        println(part1())
        println(part2())
    }
}