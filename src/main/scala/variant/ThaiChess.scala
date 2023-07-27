package chess
package variant

import chess.Color.{ Black, White }
import chess.bitboard.Bitboard
import chess.format.EpdFen
import chess.variant.Standard.{ backRank, genEvasions, isSafe }

case object ThaiChess
    extends Variant(
      id = Variant.Id(11),
      key = Variant.LilaKey("thailchess"),
      uciKey = Variant.UciKey("thailchess"),
      name = "Thai Chess",
      shortName = "Makruk",
      title = "Thai Chess",
      standardInitialPosition = false
    ):

  def validMoves(situation: Situation): List[Move] =
    import situation.{ genNonKing, genSafeKing, genCastling, color, board, ourKing }
    val enPassantMoves = situation.genEnPassant(situation.us & board.pawns)
    ourKing
      .fold(Nil): king =>
        val checkers = board.attackers(king, !situation.color)
        val candidates =
          if checkers.isEmpty then
            val targets = ~situation.us
            genNonKing(targets) ::: genSafeKing(king, targets) ::: genCastling(king) ::: enPassantMoves
          else genEvasions(king, situation, checkers) ::: enPassantMoves
        val sliderBlockers = board.sliderBlockers(king, color)
        if sliderBlockers.nonEmpty || enPassantMoves.nonEmpty then
          candidates.filter(isSafe(situation, king, sliderBlockers))
        else candidates

  private def genEvasions(king: Square, situation: Situation, checkers: Bitboard): List[Move] =
    import situation.{ genNonKing, genSafeKing, us, board }
    // Checks by these sliding pieces can maybe be blocked.
    val sliders   = checkers & board.sliders
    val attacked  = sliders.fold(Bitboard.empty)((a, s) => a | (Bitboard.ray(king, s) ^ s.bl))
    val safeKings = genSafeKing(king, ~us & ~attacked)
    val blockers  = checkers.singleSquare.fold(Nil)(c => genNonKing(Bitboard.between(king, c) | checkers))
    safeKings ++ blockers

  val initial: EpdFen = EpdFen("rnbkqbnr/8/pppppppp/8/8/PPPPPPPP/8/RNBKQBNR w KQkq - 0 1")

  override def isValidPromotion(promotion: Option[PromotableRole]) = promotion.contains(Queen)

  override protected def pawnsOnPromotionRank(board: Board, color: Color) =
    board(color, Pawn).intersects(Bitboard.rank(color.fold(Rank.Sixth, Rank.Third)))

  def pieces =
    File.all.zip(backRank).map { (x, role) => Square(x, Rank.First) -> (White - role) } ++
      File.all.map {
        Square(_, Rank.Third) -> White.pawn
      } ++
      File.all.map {
        Square(_, Rank.Sixth) -> Black.pawn
      } ++
      File.all.zip(backRank).map { (x, role) => Square(x, Rank.Eighth) -> (Black - role) } toMap

  override def allowsCastling = false
  override val backRank       = Vector(Rook, Knight, Bishop, King, Queen, Bishop, Knight, Rook)
