import scala.io.Source

object Day14 {
    val (input: String, transformations: Map[String, Char]) = Source.fromFile("day14.input")
        .mkString
        .split(s"${System.lineSeparator}${System.lineSeparator}") match {
            case Array(s, t) => (s, t.split(System.lineSeparator).map(_.split(" -> ")).map { case Array(a, b) => a -> b.head }.toMap)
        }

    extension[C](m: Seq[(C, Long)])
        def merge: Seq[(C, Long)] = m
            .groupBy { case (a, _) => a }
            .view.mapValues(_.map { case (_, n) => n }.sum).toSeq

    def states: LazyList[Seq[(Seq[Char], Long)]] = input
        .toSeq.sliding(2)
        .toSeq.groupBy(identity)
        .view.mapValues(_.size.toLong).toSeq #:: states.map(_.flatMap {
            case (Seq(a, b), n) => transformations
                .get(s"$a$b")
                .map(v => Seq((Seq(a, v), n), (Seq(v, b), n)))
                .getOrElse(Seq((Seq(a, b), n)))
        }.merge)
    
    def after(steps: Int): Long = {
        val r = states.take(steps+1).last
            .flatMap { case (Seq(a, b), n) => Seq((a, n), (b, n)) }
            .merge
            .map { 
                case (a, n) if a == input.head || a == input.last => (n+1)/2
                case (_, n) => n/2 
            }
        r.max - r.min
    }

    def part1(): Long = after(10)
    def part2(): Long = after(40)

    def main(args: Array[String]): Unit = {
        println(part1())
        println(part2())
    }
}