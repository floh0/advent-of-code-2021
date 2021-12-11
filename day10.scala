import scala.io.Source

object Day2 {
    val input:Seq[String] = Source.fromFile("day10.input")
        .getLines
        .toSeq

    val illegal: Map[Char, Int] = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
    val validation: Map[Char, Int] = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')

    def validate(line: String): Either[Int, Seq[Char]] = line
        .foldLeft(Right(Seq.empty): Either[Int, Seq[Char]]) {
            case (Left(a), _) => Left(a)
            case (Right(a), v) if validation.contains(v) => Right(v +: a)
            case (Right(h +: t), a) if validation(h) == a => Right(t)
            case (Right(a), v)  => Left(illegal(v))
        }

    def part1(): Int = input
        .map(validate)
        .map {
            case Left(a) => a
            case Right(_) => 0
        }
        .sum

    val incomplete: Map[Char, Int] = Map('(' -> 1, '[' -> 2, '{' -> 3, '<' -> 4)

    def part2(): Long = {
        val remaining =input
            .map(validate)
            .flatMap {
                case Right(v) if v.size > 0 => Some(v)
                case _ => None
            }
        remaining
            .map(_.foldLeft(0L)(_*5+incomplete(_)))
            .sorted
            .drop(remaining.size / 2)
            .head
    }

    def main(args: Array[String]): Unit = {
        println(part1())
        println(part2())
    }
}