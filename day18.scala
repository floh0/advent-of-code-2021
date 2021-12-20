import scala.io.Source

object Day18 {
    trait Token
    case object BracketLeft extends Token
    case object BracketRight extends Token
    case object Comma extends Token

    trait Tree extends Token
    final case class Node(left: Tree, right: Tree) extends Tree
    final case class Leaf(value: Int) extends Tree

    def reduceNodes(acc: List[Token], left: List[Token]): List[Token] = left match {
        case BracketLeft :: (t1: Tree) :: Comma :: (t2: Tree) :: BracketRight :: tail => reduceNodes(Node(t1, t2) :: acc, tail)
        case head :: tail => reduceNodes(head :: acc, tail)
        case _ => acc
    }

    def buildTree(state: List[Token]): Tree = state match {
        case (h: Tree) :: Nil => h
        case e => buildTree(reduceNodes(List.empty, e).reverse)
    }

    def lineToTree(line: String): Tree =
        buildTree(line.map {
            case '[' => BracketLeft
            case ']' => BracketRight
            case ',' => Comma
            case e => Leaf(e.toString.toInt)
        }.toList)

    val input = Source.fromFile("day18.input")
        .getLines
        .map(lineToTree)
        .toSeq

    extension(t1: Tree) {
        def `==`(t2: Tree): Boolean = (t1, t2) match {
            case (Leaf(a), Leaf(b)) => a == b
            case (Node(left1, right1), Node(left2, right2)) => left1 == left2 && right1 == right2
            case _ => false
        }
        def height: Int = t1 match {
            case Leaf(_) => 0
            case Node(left, right) => 1+left.height.max(right.height)
        }
    }

    def explodeOnce(tree: Tree, height: Int, exploded: Boolean): (Tree, Option[Int], Option[Int], Boolean) = tree match {
        case t if exploded => (t, None, None, exploded)
        case Node(Leaf(a), Leaf(b)) if height >= 4 => (Leaf(0), Some(a), Some(b), true)
        case Leaf(a) => (Leaf(a), None, None, exploded)
        case Node(left, right) => 
            val (newLeft, leftLeftExplosion, leftRightExplosion, leftExploded) = explodeOnce(left, height+1, exploded)
            val rightModified = leftRightExplosion.map(addValueLeft(right, _)).getOrElse(right)
            val (newRight, rightLeftExplosion, rightRightExplosion, rightExploded) = explodeOnce(rightModified, height+1, leftExploded)
            val leftModified = rightLeftExplosion.map(addValueRight(newLeft, _)).getOrElse(newLeft)
            (Node(leftModified, newRight), leftLeftExplosion, rightRightExplosion, rightExploded)
    }

    def addValueRight(tree: Tree, value: Int): Tree = tree match {
        case Leaf(a) => Leaf(a+value)
        case Node(left, right) => Node(left, addValueRight(right, value))
    }

    def addValueLeft(tree: Tree, value: Int): Tree = tree match {
        case Leaf(a) => Leaf(a+value)
        case Node(left, right) => Node(addValueLeft(left, value), right)
    }

    def reduceOnce(tree: Tree, reduced: Boolean): (Tree, Boolean) = tree match {
        case t if reduced => (t, reduced)
        case Leaf(a) if a > 9 => (Node(Leaf(a/2), Leaf(a/2+a%2)), true)
        case Leaf(a) => (Leaf(a), reduced)
        case Node(left, right) => 
            val (newLeft, leftReduced) = reduceOnce(left, reduced)
            val (newRight, rightReduced) = reduceOnce(right, leftReduced)
            (Node(newLeft, newRight), rightReduced)
    }

    def doStuff(tree: Tree): Tree =
        val (exploded, _, _, hasExploded) = explodeOnce(tree, 0, false)
        if (hasExploded) doStuff(exploded)
        else {
            val (reduced, hasReduced) = reduceOnce(exploded, false)
            if (hasReduced) doStuff(reduced)
            else reduced
        }
    
    def magnitude(tree: Tree): Int = tree match {
        case Leaf(a) => a
        case Node(left, right) => 3*magnitude(left) + 2*magnitude(right)
    }

    def part1(): Int = magnitude(input.reduce((a, v) => doStuff(Node(a, v))))

    def part2(): Int = Seq.tabulate(input.size, input.size)((x,y) => (x,y))
        .flatten
        .filter { case (x,y) => x != y }
        .map { case (x, y) => magnitude(doStuff(Node(input(x), input(y)))) }
        .max

    def main(args: Array[String]): Unit = {
        println(part1())
        println(part2())
    }
}