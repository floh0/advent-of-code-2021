import scala.io.Source

object Day5 {
    val input: Seq[((Int, Int), (Int, Int))] = Source.fromFile("day05.input")
        .getLines
        .map(_.split(" -> ").map(_.split(",").map(_.toInt)))
        .map { case Array(Array(a, b), Array(c, d)) => ((a, b), (c, d)) }
        .toSeq

    extension(s: Seq[(Int, Int)])
        def makeNewMap(m: Map[(Int, Int), Int]): Map[(Int, Int), Int] =
            m ++ s.map(v => v -> (m.getOrElse(v, 0)+1)).toMap

    extension(m: Map[(Int, Int), Int])
        def getIntersectCount: Int =
            m.foldLeft(0) { case (n, (_, v)) => if (v > 1) n+1 else n }

    def part1(): Map[(Int, Int), Int] = input
        .foldLeft(Map.empty: Map[(Int, Int), Int]) {
            case (m, ((a, b), (c, d))) if a == c => (if (b < d) (b to d) else (d to b)).map((a, _)).makeNewMap(m)
            case (m, ((a, b), (c, d))) if b == d => (if (a < c) (a to c) else (c to a)).map((_, b)).makeNewMap(m)
            case (m, _) => m
        }

    def part2(): Map[(Int, Int), Int] = input
        .foldLeft(Map.empty: Map[(Int, Int), Int]) {
            case (m, ((a, b), (c, d))) if a == c => (if (b < d) (b to d) else (d to b)).map((a, _)).makeNewMap(m)
            case (m, ((a, b), (c, d))) if b == d => (if (a < c) (a to c) else (c to a)).map((_, b)).makeNewMap(m)
            case (m, ((a, b), (c, d))) if (c-a).abs == (d-b).abs => 
                val x = (c-a).abs / (c-a)
                val y = (d-b).abs / (d-b)
                (0 to (c-a).abs).map(v => (a+v*x, b+v*y)).makeNewMap(m)
            case (m, _) => m
        }
        

    def main(args: Array[String]): Unit = {
        println(part1().getIntersectCount)
        println(part2().getIntersectCount)
    }
}