package chess


import bitboard.Bitboard
import bitboard.Bitboard.*
import Pos.*


type Castles = Bitboard
object Castles:

  import cats.syntax.all.*
  extension (c: Castles)
    inline def can(inline color: Color) = Castles.Can(c, color)

    def whiteKingSide: Boolean  = (c & H1.bitboard).nonEmpty
    def whiteQueenSide: Boolean = (c & A1.bitboard).nonEmpty
    def blackKingSide: Boolean  = (c & H8.bitboard).nonEmpty
    def blackQueenSide: Boolean = (c & A8.bitboard).nonEmpty

    def without(color: Color): Castles =
      color match
        case White =>
          c & ~A1.bitboard & ~H1.bitboard
        case Black =>
          c & ~A8.bitboard & ~H8.bitboard

    def without(color: Color, side: Side): Castles =
      (color, side) match
        case (White, KingSide)  => c & ~H1.bitboard
        case (White, QueenSide) => c & ~A1.bitboard
        case (Black, KingSide)  => c & ~H8.bitboard
        case (Black, QueenSide) => c & ~A8.bitboard

    def add(color: Color, side: Side): Castles =
      (color, side) match
        case (White, KingSide)  => c & H1.bitboard
        case (White, QueenSide) => c & A1.bitboard
        case (Black, KingSide)  => c & H8.bitboard
        case (Black, QueenSide) => c & A8.bitboard

    def update(color: Color, kingSide: Boolean, queenSide: Boolean): Castles = color match
        case White => c.without(color) | kingSide.whiteKing | queenSide.whiteQueen
        case Black => c.without(color) | kingSide.blackKing | queenSide.blackQueen

    def toFenString: String = {
      (if (whiteKingSide) "K" else "") +
        (if (whiteQueenSide) "Q" else "") +
        (if (blackKingSide) "k" else "") +
        (if (blackQueenSide) "q" else "")
    } match
      case "" => "-"
      case n  => n

    def toSeq: Array[Boolean] = Array(whiteKingSide, whiteQueenSide, blackKingSide, blackQueenSide)

    def isEmpty = c.isEmpty

  extension (b: Boolean)
    def whiteKing: Castles  = if (b) H1.bitboard else Bitboard.empty
    def whiteQueen: Castles = if (b) A1.bitboard else Bitboard.empty
    def blackKing: Castles  = if (b) H8.bitboard else Bitboard.empty
    def blackQueen: Castles = if (b) A8.bitboard else Bitboard.empty

  def apply(
      castles: (Boolean, Boolean, Boolean, Boolean)
  ): Castles =
      val whiteKing  = castles._1.whiteKing
      val whiteQueen = castles._2.whiteQueen
      val blackKing  = castles._3.blackKing
      val blackQueen = castles._4.blackQueen
      whiteKing | whiteQueen | blackKing | blackQueen

  def apply(str: String): Castles = str match
    case "-" => Bitboard.empty
    case _ =>
      str.toList.traverse(charToSquare)
        .map(_.foldRight(Bitboard.empty)((s, b) => s.bitboard | b))
        .getOrElse(Bitboard.empty)

  private def charToSquare: (c: Char) => Option[Pos] =
    case 'k' => Some(H8)
    case 'q' => Some(A1)
    case 'K' => Some(H1)
    case 'Q' => Some(A1)
    case _   => None

  val all: Castles  = Bitboard.corners
  val none: Castles = Bitboard.empty
  def init: Castles = all

  final class Can(castles: Castles, color: Color):
    def on(side: Side): Boolean =
      (color, side) match
        case (White, KingSide)  => castles.whiteKingSide
        case (White, QueenSide) => castles.whiteQueenSide
        case (Black, KingSide)  => castles.blackKingSide
        case (Black, QueenSide) => castles.blackQueenSide
    def any = on(KingSide) || on(QueenSide)
