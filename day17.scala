import scala.io.Source

object Day17 {
    val (x1: Int, x2: Int, y1: Int, y2: Int) = Source.fromFile("day17.input").mkString match { 
        case s"target area: x=$a..$b, y=$c..$d" => (a.toInt, b.toInt, c.toInt, d.toInt)
    }

    def part1(): Int = (y1*(y1+1))/2

    def hitTarget(x: Int, y: Int, vx: Int, vy: Int): Boolean = {
        if (x >= x1 && x <= x2 && y >= y1 && y <= y2) true
        else if (y < y1 || x > x2) false
        else hitTarget(x+vx, y+vy, if (vx > 0) vx-1 else if (vx < 0) vx+1 else vx, vy-1)
    }

    def part2(): Int = {
        for {
            i <- 0 to x2
            j <- y1 to y1.abs if hitTarget(0, 0, i, j)
        } yield (i, j)
    }.size

    def main(args: Array[String]): Unit = {
        println(part1())
        println(part2())
    }
}