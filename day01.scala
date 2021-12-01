import scala.io.Source

object Day1 {
    val input = Source.fromFile("day01.input").getLines.map(_.toInt).toSeq

    def part1(): Int = input
        .sliding(2)
        .map { case Seq(a, b) => if (a < b) 1 else 0 }
        .sum

    def part2(): Int = input
        .sliding(3)
        .map(_.sum)
        .sliding(2)
        .map { case Seq(a, b) => if (a < b) 1 else 0 }
        .sum

    def main(args: Array[String]): Unit = {
        println(part1())
        println(part2())
    }
}