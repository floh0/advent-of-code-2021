import scala.io.Source

object Day3 {
    val input: Seq[Seq[Int]] = Source.fromFile("day03.input")
        .getLines
        .map(_.split("").map(_.toInt).toIndexedSeq)
        .toSeq

    extension(i: Seq[Int]) {
        def toNumeric: Int = i.foldRight((1, 0)) {
            case (a, (e, b)) => (e*2, b+a*e)
        }._2

        def not: Seq[Int] = i.map(x => if (x == 0) 1 else 0)
    }
    
    def part1(): Seq[Int] = input
        .foldLeft(None: Option[Seq[(Int, Int)]]) {
            case (a, v) => Some(a
                .map(_.zip(v).map { case ((z, o), x) => if (x == 1) (z, o+1) else (z+1, o)})
                .getOrElse(v.map(x => if (x == 1) (0, 1) else (1, 0)))
            )
        }
        .getOrElse(Seq.empty)
        .map { case (z, o) => if (z > o) 0 else 1 }

    def part2(i: Seq[Seq[Int]], a: Int, p: (Int, Int) => Boolean): Seq[Int] =
        if (i.length == 1) i(0)
        else {
            val (z, o) = i
                .foldLeft((0, 0)) {
                    case ((z, o), v) => if (v(a) == 1) (z, o+1) else (z+1, o)
                }
            val k = if (z > o) 0 else 1
            part2(i.filter(v => p(v(a), k)), a+1, p)
        }


    def main(args: Array[String]): Unit = {
        val g = part1()
        val e = g.not
        println(s"${g.mkString}, ${g.toNumeric}, ${e.mkString}, ${e.toNumeric}, ${g.toNumeric*e.toNumeric}")

        val o2 = part2(input, 0, _ == _)
        val co2 = part2(input, 0, _ != _)
        println(s"${o2.mkString}, ${o2.toNumeric}, ${co2.mkString}, ${co2.toNumeric}, ${o2.toNumeric*co2.toNumeric}")
    }
}