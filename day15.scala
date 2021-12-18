import scala.collection.mutable.{HashSet, PriorityQueue}
import scala.io.Source

object Day15 {
    val input: Seq[Seq[Int]] = Source.fromFile("day15.input")
        .getLines
        .map(_.map(_.toString.toInt))
        .toSeq

    val h = input.size
    val w = input(0).size

    def dijkstra(cave: Seq[Seq[Int]], width: Int, height: Int, pq: PriorityQueue[(Int, (Int, Int))], visited: HashSet[(Int, Int)]): Int = {
        val (d, (i, j)) = pq.dequeue()
        if (i == height-1 && j == width-1) d
        else {
            Seq((i-1, j), (i+1, j), (i, j-1), (i, j+1))
                .filterNot { case (a, b) => a < 0 || a >= height || b < 0 || b >= width || visited.contains((a, b)) }
                .map { case (a, b) => (d + cave(a)(b), (a, b)) }
                .foreach { case (c, (a, b)) => 
                    pq.enqueue((c, (a, b)))
                    visited.add((a, b))
                }
            dijkstra(cave, width, height, pq, visited)
        }
    }

    def findMin(cave: Seq[Seq[Int]]): Int = {
        val ord = Ordering.by[(Int, (Int, Int)), Int] { case (d, _) => d }.reverse
        val pq = PriorityQueue((0, (0, 0)))(ord)
        val visited = HashSet((0, 0))
        dijkstra(cave, cave.size, cave(0).size, pq, visited)
    }

    def part1(): Int = findMin(input)

    def computeValue(i: Int): Int = if (i > 9) computeValue(i-9) else i

    val newInput = (0 to 4).flatMap { a => (0 until h).map { i => 
        (0 to 4).flatMap { b => (0 until w).map { j => computeValue(a+b+input(i)(j)) }}
    }}
    val newH = newInput.size
    val newW = newInput(0).size

    def part2(): Int = findMin(newInput)

    def main(args: Array[String]): Unit = {
        println(part1())
        println(part2())
    }
}