import scala.io.Source

object Day3 {
    val input: Seq[Seq[Int]] = Source.fromFile("day03.input")
        .getLines
        .map(_.split("").map(_.toInt).toSeq)
        .toSeq

    extension(i: Seq[Int]) {
        def toNumeric: Int = Integer.parseInt(i.mkString, 2)
        def not: Seq[Int] = i.map(x => if (x == 0) 1 else 0)
    }
    
    def part1(): Seq[Int] = input
        .foldLeft(None: Option[Seq[(Int, Int)]]) {
            case (None, v) => Some(v.map(x => if (x == 1) (0, 1) else (1, 0)))
            case (a, v) => a.map(_.zip(v).map { case ((z, o), x) => if (x == 1) (z, o+1) else (z+1, o)})
        }
        .getOrElse(Seq.empty)
        .map { case (z, o) => if (z > o) 0 else 1 }

    def part2(i: Seq[Seq[Int]], a: Int, p: (Int, Int) => Boolean): Seq[Int] =
        i match {
            case Seq(e) => e
            case _ =>
                val (z, o) = i.foldLeft((0, 0)) {
                    case ((z, o), v) => if (v(a) == 1) (z, o+1) else (z+1, o)
                }
                val k = if (z > o) 0 else 1
                part2(i.filter(v => p(v(a), k)), a+1, p)
        }

    def main(args: Array[String]): Unit = {
        val r = part1()
        val g = r.toNumeric
        val e = r.not.toNumeric
        println(s"$g, $e, ${g*e}")

        val o2 = part2(input, 0, _ == _).toNumeric
        val co2 = part2(input, 0, _ != _).toNumeric
        println(s"$o2, $co2, ${o2*co2}")
    }
}