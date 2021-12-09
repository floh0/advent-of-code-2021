import scala.io.Source

object Day8 {
    val input: Seq[(Seq[String], Seq[String])] = Source.fromFile("day08.input")
        .getLines
        .map(_.split("\\|").map(_.strip.split(" ").toSeq))
        .map { case Array(a, b) => (a, b) }
        .toSeq

    val uniqueSegments = Seq(2, 3, 4 ,7)

    def part1(): Int = 
        input.flatMap { case (_, a) => a }.foldLeft(0)((a, v) => if (uniqueSegments.contains(v.length)) a+1 else a)

    def fixedSize(i: Int)(a: Seq[Seq[Char]], g: Seq[String]): Option[String] =
        g.find(_.size == i)

    def fixedSizeContains(i: Int, j: Int)(a: Seq[Seq[Char]], g: Seq[String]): Option[String] =
        g.filter(_.size == i).filter(v => a(j).forall(v.contains)).headOption

    def contained(j: Int)(a: Seq[Seq[Char]], g: Seq[String]): Option[String] =
        g.filter(_.forall(a(j).contains)).headOption

    def last(a: Seq[Seq[Char]], g: Seq[String]): Option[String] =
        g.headOption

    def solveLine(g: Seq[String]): Seq[(Seq[Char], Int)] = { 
        val (_, s) = Seq(
            (8, fixedSize(7)), 
            (4, fixedSize(4)), 
            (7, fixedSize(3)), 
            (1, fixedSize(2)), 
            (9, fixedSizeContains(6, 4)), 
            (0, fixedSizeContains(6, 1)), 
            (6, fixedSize(6)), 
            (3, fixedSizeContains(5, 1)), 
            (5, contained(6)), 
            (2, last)
        )
            .foldLeft((g, (0 to 9).map(_ => ('a' to 'g'): Seq[Char]))) {
                case ((l, a), (i, f)) => 
                    val n = a.zipWithIndex.map { case (e, j) => if (i == j) e.filter(f(a, l).getOrElse("").contains) else e }
                    (n.foldLeft(l)((b, v) => b.filterNot(x => x.size == v.size && v.forall(x.contains))), n)
            }
        s.zipWithIndex
    }

    def part2(): Int = input
        .map {
            case (l, r) =>
                val s = solveLine(l)
                r.map { v => s.collect { case (e, i) if e.size == v.size && e.forall(v.contains) => i }.head }
        }
        .map(_.mkString.toInt)
        .sum

    def main(args: Array[String]): Unit = {
        println(part1())
        println(part2())
    }
}