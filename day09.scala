import scala.io.Source

object Day9 {
    val input: Seq[Seq[Int]] = Source.fromFile("day09.input")
        .getLines
        .map(_.map(_.toString.toInt))
        .toSeq

    val w = input.size
    val h = input(0).size

    def getLowPoints(): Seq[(Int, Int)] = {
        val map = for {
            i <- 0 until w
            j <- 0 until h
        } yield {
            val filtered = Seq((i-1, j), (i+1, j), (i, j-1), (i, j+1))
                .filterNot { case (a, b) => a < 0 || b < 0 || a > w-1 || b > h-1 }
            (i, j, filtered)   
        }  
        map.foldLeft(Seq.empty: Seq[(Int, Int)]) {
            case (a, (i, j, v)) => 
                if (v.forall { case (k, l) => input(k)(l) > input(i)(j) }) (i, j) +: a
                else a
        }
    }

    def part1(): Int = getLowPoints()
        .map { case (i, j) => input(i)(j)+1 }
        .sum

    def around(i: Int, j: Int): LazyList[Seq[(Int, Int)]] =
        Seq((i, j)) #:: 
            around(i, j).map(_
                .flatMap { case (k, l) => Seq((k, l), (k-1, l), (k+1, l), (k, l-1), (k, l+1)) }
                .filterNot { case (a, b) => a < 0 || b < 0 || a > w-1 || b > h-1 }
                .filterNot { case (a, b) => input(a)(b) == 9 }
                .distinct
            )

    def bassin(i: Int, j: Int): Int = around(i, j)
        .zip(around(i, j).drop(1))
        .dropWhile { case (a, b) => a.size < b.size }
        .map { case (_, b) => b.size }
        .head

    def part2(): Int = getLowPoints()
        .map { case (i, j) => bassin(i, j) }
        .sortWith(_ > _)
        .take(3)
        .product

    def main(args: Array[String]): Unit = {
        println(part1())
        println(part2())
    }
}