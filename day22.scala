import scala.io.Source

object Day22 {
    val input: Seq[(Boolean, Int, Int, Int, Int, Int, Int)] = Source.fromFile("day22.input")
        .getLines
        .map {
            case s"$o x=$x1..$x2,y=$y1..$y2,z=$z1..$z2" => 
                (o == "on", x1.toInt, x2.toInt, y1.toInt, y2.toInt, z1.toInt, z2.toInt)
        }
        .toSeq

    extension(t: (Int, Int, Int, Int)) {
        def splitSegments: Seq[(Int, Int)] = {
            val (a, b, c, d) = t
            Seq(a, b, c, d).sorted match {
                case Seq(i, j, k, l) => Seq((i, j-1), (j, k), (k+1, l)).filter(_ <= _)
            }
        }
    }

    extension(a: (Int, Int)) {
        def overlap(b: (Int, Int)): Boolean = {
            val (x1, x2) = a
            val (x3, x4) = b
            (x3 >= x1 && x3 <= x2) || (x4 >= x1 && x4 <= x2) || (x1 >= x3 && x1 <= x4) || (x2 >= x3 && x2 <= x4)
        }
    }

    extension(a: ((Int, Int), (Int, Int), (Int, Int))) {
        def overlap3d(b: ((Int, Int), (Int, Int), (Int, Int))) ={
            val ((x1, x2), (y1, y2), (z1, z2)) = a
            val ((x3, x4), (y3, y4), (z3, z4)) = b
            ((x1, x2).overlap((x3, x4)) && (y1, y2).overlap((y3, y4)) && (z1, z2).overlap((z3, z4)))       
        }

        def minus(b: ((Int, Int), (Int, Int), (Int, Int))): Seq[((Int, Int), (Int, Int), (Int, Int))] =
            if (a.overlap3d(b)) {
                val ((x1, x2), (y1, y2), (z1, z2)) = a
                val ((x3, x4), (y3, y4), (z3, z4)) = b
                for {
                    x <- (x1, x2, x3, x4).splitSegments
                    y <- (y1, y2, y3, y4).splitSegments
                    z <- (z1, z2, z3, z4).splitSegments
                    c = (x, y, z) if c.overlap3d(a) && !c.overlap3d(b)
                } yield (x, y, z)
            } else Seq(a)
    }

    val initialization = input
        .map {
            case (o, x1, x2, y1, y2, z1, z2) => 
                (o, x1.max(-50), x2.min(50), y1.max(-50), y2.min(50), z1.max(-50), z2.min(50))
        }
        .filter { case (_, x1, x2, y1, y2, z1, z2) => x1 <= x2 && y1 <= y2 && z1 <= z2 }

    def turnOnAndOff(instructions: Seq[(Boolean, Int, Int, Int, Int, Int, Int)]): Long = instructions
        .foldLeft(Seq.empty: Seq[((Int, Int), (Int, Int), (Int, Int))]) {
            case (a, (o, x1, x2, y1, y2, z1, z2)) =>
                val value = ((x1, x2), (y1, y2), (z1, z2))
                val filtered = a.flatMap(_.minus(value))
                if (o) value +: filtered else filtered
        }
        .foldLeft(0L) {
            case (a, ((x1, x2), (y1, y2), (z1, z2))) => 
                a + (1+(x2-x1).abs).toLong * (1+(y2-y1).abs).toLong * (1+(z2-z1).abs).toLong
        }

    def part1() = turnOnAndOff(initialization)
    def part2() = turnOnAndOff(input)

    def main(args: Array[String]): Unit = {
        println(part1())
        println(part2())
    }
}