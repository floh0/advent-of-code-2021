import scala.io.Source

object Day7 {
    val input: Seq[Int] = Source.fromFile("day07.input")
        .getLines
        .toSeq
        .flatMap(_.split(",").map(_.toInt))

    def fuel(rate: (Int, Int) => Int)(p: Int): Int = input.foldLeft(0)((a, v) => a+rate(v,p))

    def part1() = {
        val fuel1 = fuel((a,b) => (a-b).abs)
        val median = input.sorted.drop(input.size / 2).head
        fuel1(median)
    }

    def part2() = {
        val fuel2 = fuel((a,b) => (a-b).abs*((a-b).abs+1)/2)
        val mean: Int = input.sum / input.size
        Seq(mean, mean+1).map(fuel2).min
    }

    def main(args: Array[String]): Unit = {
        println(part1())
        println(part2())
    }
}


