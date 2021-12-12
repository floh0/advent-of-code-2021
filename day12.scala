import scala.io.Source

object Day11 {
    val input: Seq[(String, String)] = Source.fromFile("day12.input")
        .getLines
        .map(_.split("-"))
        .map { case Array(a, b) => (a, b) }
        .toSeq

    def filterBig(curr: String, caves: Seq[(String, String)]): Seq[(String, String)] = 
        if (curr.matches("[A-Z]+")) caves else caves.filterNot { case (a, b) => a == curr || b == curr }

    def throughIt(curr: String, caves: Seq[(String, String)], path: Seq[String], twice: Boolean): Set[Seq[String]] = {
        val newPath = curr +: path
        if (curr == "end") Set(newPath)
        else {
            if (twice && !curr.matches("start|end"))
                Set((caves, false), (filterBig(curr, caves), true))
            else
                Set((filterBig(curr, caves), twice))
        }.flatMap {
            case (newCaves, newTwice) => caves
                .collect { 
                    case (a, b) if a == curr => b 
                    case (a, b) if b == curr => a
                }
                .flatMap(throughIt(_, newCaves, newPath, newTwice))
        }
    }

    def part1(): Int = throughIt("start", input, Seq.empty, false).size

    def part2(): Int = throughIt("start", input, Seq.empty, true).size

    def main(args: Array[String]): Unit = {
        println(part1())
        println(part2())
    }
}