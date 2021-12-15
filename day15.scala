import scala.io.Source

object Day15 {
    val input: Seq[Seq[Int]] = Source.fromFile("day15.input")
        .getLines
        .map(_.map(_.toString.toInt))
        .toSeq

    val h = input.size
    val w = input(0).size

    def astar(cave: Seq[Seq[Int]], width: Int, height: Int, heuristic: (Int, Int) => Int)
             (i: Int, j: Int, weight: Int, candidates: Seq[(Int, Int, Int, Int)], visited: Seq[(Int, Int)]): Int = {
        val newVisited = (i, j) +: visited
        if (i == height-1 && j == width-1) weight
        else {
            val newCandidates = Seq((i-1, j), (i+1, j), (i, j-1), (i, j+1))
                .filterNot { case (a, b) => a < 0 || a >= height || b < 0 || b >= width || newVisited.contains((a, b)) }
                .map { case (a, b) => (a, b, weight + cave(a)(b), heuristic(a, b)) }
            (candidates ++ newCandidates)
                .groupBy { case (a, b, _, _) => (a, b) }
                .values
                .flatMap(_.sortBy { case (_, _, c, d) => c+d }.headOption)
                .toSeq
                .sortBy { case (_, _, c, d) => c+d } match {
                    case (a, b, c, _) +: t => astar(cave, width, height, heuristic)(a, b, c, t, newVisited)
                }
        }
    }

    def part1(): Int = {
        val heuristic: (Int, Int) => Int = (x: Int, y: Int) => (h-1-x).abs+(w-1-y).abs
        astar(input, h, w, heuristic)(0, 0, 0, Seq.empty, Seq.empty)
    }

    val newInput = (0 to 4).flatMap { a => (0 until h).map { i => 
            (0 to 4).flatMap { b => (0 until w).map { j => 
                val newValue = a+b+input(i)(j)
                if (newValue > 9) newValue%9 else newValue
            }}
    }}
    val newH = newInput.size
    val newW = newInput(0).size

    def part2(): Int = {
        val heuristic: (Int, Int) => Int = (x: Int, y: Int) => (newH-1-x).abs+(newW-1-y).abs
        astar(newInput, newH, newW, heuristic)(0, 0, 0, Seq.empty, Seq.empty)
    }

    def main(args: Array[String]): Unit = {
        println(part1())
        println(part2())
    }
}