package patmat

/**
  * A huffman code is represented by a binary tree.
  *
  * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
  * The weight of a `Leaf` is the frequency of appearance of the character.
  *
  * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
  * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
  * leaves.
  */
abstract class CodeTree
case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int)
    extends CodeTree
case class Leaf(char: Char, weight: Int) extends CodeTree

/**
  * Assignment 4: Huffman coding
  *
  */
trait Huffman extends HuffmanInterface {

  // Part 1: Basics
  def weight(tree: CodeTree): Int = tree match {
    case Leaf(_, w)       => w
    case Fork(_, _, _, w) => w
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(c, _)        => List(c)
    case Fork(_, _, cs, _) => cs
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(
      left,
      right,
      chars(left) ::: chars(right),
      weight(left) + weight(right)
    )

  // Part 2: Generating Huffman trees
  def string2Chars(str: String): List[Char] = str.toList

  def times(chars: List[Char]): List[(Char, Int)] = {

    def insert(c: Char, list: List[(Char, Int)]): List[(Char, Int)] =
      list match {
        case (d, i) :: tail =>
          if (d == c) (d, i + 1) :: tail else (d, i) :: insert(c, tail)
        case Nil => List((c, 1))
      }

    chars.foldLeft[List[(Char, Int)]](List.empty) { (acc, c) => insert(c, acc) }
  }
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {

    def insert(freq: (Char, Int), leafs: List[Leaf]): List[Leaf] = leafs match {
      case Leaf(_, w) :: tail =>
        if (freq._2 < w) Leaf(freq._1, freq._2) :: leafs
        else leafs.head :: insert(freq, tail)
      case Nil => List(Leaf(freq._1, freq._2))
    }
    freqs.foldLeft[List[Leaf]](List.empty) { (acc, freq) => insert(freq, acc) }
  }

  def singleton(trees: List[CodeTree]): Boolean = trees.size == 1

  def combine(trees: List[CodeTree]): List[CodeTree] = {
    def insert(tree: CodeTree, acc: List[CodeTree]): List[CodeTree] =
      acc match {
        case head :: next =>
          if (weight(tree) < weight(head)) tree :: head :: next
          else head :: insert(tree, next)
        case Nil => tree :: Nil
      }

    trees match {
      case l :: r :: rest =>
        insert(Fork(l, r, chars(l) ::: chars(r), weight(l) + weight(r)), rest)
      case _ :: Nil => trees
      case Nil      => trees
    }
  }

  def until(
      done: List[CodeTree] => Boolean,
      merge: List[CodeTree] => List[CodeTree]
  )(trees: List[CodeTree]): List[CodeTree] = {
    if (done(trees)) trees
    else until(done, merge)(merge(trees))
  }

  def createCodeTree(chars: List[Char]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  // Part 3: Decoding

  type Bit = Int

  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def inner(
        currentTree: CodeTree,
        currentBits: List[Bit],
        acc: List[Char]
    ): List[Char] = currentTree match {
      case Fork(left, right, chars, weight) =>
        currentBits match {
          case b :: nextBits =>
            inner(if (b == 0) left else right, nextBits, acc)
          case Nil => acc
        }
      case Leaf(char, weight) =>
        currentBits match {
          case _ :: _ => inner(tree, currentBits, char :: acc)
          case Nil    => char :: acc
        }
    }
    inner(tree, bits, List.empty).reverse
  }

  val frenchCode: CodeTree = Fork(
    Fork(
      Fork(
        Leaf('s', 121895),
        Fork(
          Leaf('d', 56269),
          Fork(
            Fork(
              Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279),
              Leaf('f', 16351),
              List('x', 'j', 'f'),
              30630
            ),
            Fork(
              Fork(
                Fork(
                  Fork(
                    Leaf('z', 2093),
                    Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492),
                    List('z', 'k', 'w'),
                    4585
                  ),
                  Leaf('y', 4725),
                  List('z', 'k', 'w', 'y'),
                  9310
                ),
                Leaf('h', 11298),
                List('z', 'k', 'w', 'y', 'h'),
                20608
              ),
              Leaf('q', 20889),
              List('z', 'k', 'w', 'y', 'h', 'q'),
              41497
            ),
            List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'),
            72127
          ),
          List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'),
          128396
        ),
        List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'),
        250291
      ),
      Fork(
        Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430),
        Fork(
          Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856),
          Leaf('u', 96785),
          List('m', 'p', 'u'),
          188641
        ),
        List('o', 'l', 'm', 'p', 'u'),
        355071
      ),
      List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm',
        'p', 'u'),
      605362
    ),
    Fork(
      Fork(
        Fork(
          Leaf('r', 100500),
          Fork(
            Leaf('c', 50003),
            Fork(
              Leaf('v', 24975),
              Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110),
              List('v', 'g', 'b'),
              52085
            ),
            List('c', 'v', 'g', 'b'),
            102088
          ),
          List('r', 'c', 'v', 'g', 'b'),
          202588
        ),
        Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915),
        List('r', 'c', 'v', 'g', 'b', 'n', 't'),
        422503
      ),
      Fork(
        Leaf('e', 225947),
        Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575),
        List('e', 'i', 'a'),
        458522
      ),
      List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'),
      881025
    ),
    List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm',
      'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'),
    1486387
  )

  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0,
    1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0,
    0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  def decodedSecret: List[Char] = decode(frenchCode, secret)

  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def inner(
        currentTree: CodeTree,
        text: List[Char],
        acc: List[Bit]
    ): List[Bit] =
      text match {
        case char :: remaining =>
          currentTree match {
            case Fork(left, right, forkChars, weight) => {
              if (chars(left).contains(char)) inner(left, text, 0 :: acc)
              else if (chars(right).contains(char)) inner(right, text, 1 :: acc)
              else
                throw new IllegalArgumentException(
                  "Left or right should contain the character"
                )
            }
            case Leaf(leafChar, _) => {
              assert(leafChar == char)
              inner(tree, remaining, acc)
            }
          }
        case Nil => acc
      }

    inner(tree, text, List.empty).reverse
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
    case (key, bits) :: remaining =>
      if (key == char) bits else codeBits(remaining)(char)
    case Nil => throw new IllegalArgumentException("This should never happen")
  }

  def convert(tree: CodeTree): CodeTable = {
    def inner(
        tree: CodeTree,
        currentPath: List[Bit],
        acc: CodeTable
    ): CodeTable = tree match {
      case Fork(left, right, _, _) =>
        inner(left, 0 :: currentPath, acc) ::: inner(
          right,
          1 :: currentPath,
          acc
        )
      case Leaf(char, _) => (char, currentPath.reverse) :: acc
    }
    inner(tree, List.empty, List.empty)
  }

  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val table = codeBits(convert(tree))(_)
    text
      .foldLeft[List[Bit]](List.empty) { (acc, char) =>
        table(char).reverse ::: acc
      }
      .reverse
  }
}

object Huffman extends Huffman
