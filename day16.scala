import scala.io.Source

object Day16 {
    val input: String = Source.fromFile("day16.input").mkString

    val binaryInput: String = BigInt(input, 16).toString(2)
    val binaryInputTrailingZeros = Array.fill(input.size*4-binaryInput.size)(0).mkString + binaryInput

    extension(s: String) {
        def binaryToInt: Int = Integer.parseInt(s, 2)
        def binaryToBigInt: BigInt = BigInt(s, 2)
    }

    trait Packet(version: Int, id: Int)
    final case class Literal(version: Int, id: Int, value: BigInt) extends Packet(version, id)
    final case class Operator(version: Int, id: Int, subpackets: Seq[Packet]) extends Packet(version, id)

    def parse(packet: String): Seq[Packet] = {
        val (header, remaining) = packet.splitAt(6)
        val (version, id) = header.splitAt(3)
        val (v, t) = (version.binaryToInt, id.binaryToInt)  
        if (t == 4) {
            val (ones, zeros) = remaining.grouped(5).span(_.head == '1')
            val h = (ones.toSeq :+ zeros.next).map(_.drop(1)).mkString.binaryToBigInt
            val left = zeros.mkString
            Seq(Literal(v, t, h)) ++ (if (left.forall(_ == '0')) Seq.empty else parse(left))
        } else {
            val i = remaining.head
            if (i == '0') {
                val (length, subpackets) = remaining.tail.splitAt(15)
                val l = length.binaryToInt
                val (left, right) = subpackets.splitAt(l)
                Seq(Operator(v, t, parse(left))) ++ (if (right.forall(_ == '0')) Seq.empty else parse(right))       
            } else {
                val (number, subpackets) = remaining.tail.splitAt(11)
                val l = number.binaryToInt
                val (left, right) = parse(subpackets).splitAt(l)
                Seq(Operator(v, t, left)) ++ right  
            }
        }
    }

    val packet: Packet = parse(binaryInputTrailingZeros).head

    def sumVersions(packet: Packet): Int = packet match {
        case Literal(v, _, _) => v
        case Operator(v, _, s) => v + s.map(sumVersions).sum
    }

    def part1() = sumVersions(packet)

    def getValue(packet: Packet): BigInt = packet match {
        case Literal(v, _, n) => n
        case Operator(v, 0, s) => s.map(getValue).sum
        case Operator(v, 1, s) => s.map(getValue).product
        case Operator(v, 2, s) => s.map(getValue).min
        case Operator(v, 3, s) => s.map(getValue).max
        case Operator(v, 5, a +: b +: _) => if (getValue(a) > getValue(b)) BigInt(1) else BigInt(0)
        case Operator(v, 6, a +: b +: _) => if (getValue(a) < getValue(b)) BigInt(1) else BigInt(0)
        case Operator(v, 7, a +: b +: _) => if (getValue(a) == getValue(b)) BigInt(1) else BigInt(0)
    }

    def part2() = getValue(packet)

    def main(args: Array[String]): Unit = {
        println(part1())
        println(part2())
    }
}