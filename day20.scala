import scala.io.Source

object Day20 {
    val (enhancement: Seq[Int], input: Map[(Int, Int), Int]) = Source.fromFile("day20.input")
        .mkString
        .split(s"${System.lineSeparator}${System.lineSeparator}") match {
            case Array(a, b) =>
                val e = a.map(v => if (v == '.') 0 else 1)
                val s = b.split(System.lineSeparator).zipWithIndex.flatMap {
                    case (l, i) => l.zipWithIndex.map {
                        (v, j) => (i, j) -> (if (v == '.') 0 else 1)
                    }
                }.toMap
                (e, s)
        }

    extension(s: String) {
        def binaryToInt: Int = Integer.parseInt(s, 2)
    }

    def doStuff(state: Map[(Int, Int), Int], infinite: Int): (Map[(Int, Int), Int], Int) = {
        val newInfinite = enhancement(infinite)
        val newState = state.keys
            .toSet
            .flatMap { case (i, j) => Seq.tabulate(3,3)((x, y) => (i+x-1, j+y-1)).flatten }
            .map {
                case (i, j) => 
                    val ref = Seq.tabulate(3,3)((x, y) => state.getOrElse((i+x-1, j+y-1), infinite))
                        .flatten
                        .mkString
                        .binaryToInt
                    (i, j) -> enhancement(ref)
            }
            .filter {
                case (_, v) => v != newInfinite
            }
            .toMap
        (newState, newInfinite)
    }

    def after(iter: Int): Int = {
        val (state, infinite) = (1 to iter).foldLeft((input, 0)) { case ((a, i), _) => doStuff(a, i) }
        state.filter { case (_, v) => v != infinite }.size
    }

    def part1(): Int = after(2)
    def part2(): Int = after(50)

    def main(args: Array[String]): Unit = {
        println(part1())
        println(part2())
    }
}