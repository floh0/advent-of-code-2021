import scala.io.Source

object Day2 {
    val input: Seq[(String, Int)] = Source.fromFile("day02.input")
        .getLines
        .map(_.split(" "))
        .map { case Array(i, v) => (i, v.toInt) }
        .toSeq

    def part1(): (Int, Int) = input
        .foldLeft((0, 0)) {
            case ((h, d), (i, v)) =>
                if (i == "forward") (h+v, d)
                else if (i == "down") (h, d+v)
                else if (i == "up") (h, d-v)
                else (h, d)
        }

    def part2(): (Int, Int, Int) = input
        .foldLeft((0, 0, 0)) {
            case ((a, h, d), (i, v)) =>
                if (i == "forward") (a, h+v, d+v*a)
                else if (i == "down") (a+v, h, d)
                else if (i == "up") (a-v, h, d)
                else (a, h, d)
        }

    def main(args: Array[String]): Unit = {
        val (h1, d1) = part1()
        println(s"$h1, $d1, ${h1*d1}") 
        val (a2, h2, d2) = part2()
        println(s"$a2, $h2, $d2, ${h2*d2}")
    }
}