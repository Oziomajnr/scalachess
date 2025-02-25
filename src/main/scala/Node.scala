package chess

import cats.*
import cats.derived.*
import cats.syntax.all.*
import scala.annotation.tailrec

sealed abstract class Tree[A](val value: A, val child: Option[Node[A]]) derives Functor, Traverse:

  final def withValue(value: A): TreeSelector[A, this.type] = this match
    case n: Node[A]      => n.copy(value = value)
    case v: Variation[A] => v.copy(value = value)

  final def updateValue(f: A => A): TreeSelector[A, this.type] = this match
    case n: Node[A]      => n.copy(value = f(value))
    case v: Variation[A] => v.copy(value = f(value))

  final def withChild(child: Option[Node[A]]): TreeSelector[A, this.type] = this match
    case n: Node[A]      => n.copy(child = child)
    case v: Variation[A] => v.copy(child = child)

  final def updateChild(f: Option[Node[A]] => Option[Node[A]]): TreeSelector[A, this.type] =
    withChild(f(child))

  final def withoutChild: TreeSelector[A, this.type] = withChild(none)

  final def isNode: Boolean = this match
    case n: Node[A]      => true
    case v: Variation[A] => false

  final def isVariation: Boolean = !this.isNode

  // child and it's variations
  def childAndVariations: List[Tree[A]] = child.toList ++ variations

  // variations of the child
  def childVariations: List[Tree[A]] = child.fold(Nil)(_.variations)

  // child and child's variations as List
  def childAndChildVariations: List[Tree[A]] = child.fold(Nil)(x => x +: x.variations)

  def variations: List[Tree[A]] =
    this match
      case n: Node[A]      => n.variations
      case v: Variation[A] => Nil

  final def mainlineValues: List[A] =
    @tailrec
    def loop(tree: Tree[A], acc: List[A]): List[A] = tree.child match
      case None        => tree.value :: acc
      case Some(child) => loop(child, tree.value :: acc)
    loop(this, Nil).reverse

  def mainlinePath[Id](using HasId[A, Id]): List[Id] =
    @tailrec
    def loop(tree: Tree[A], acc: List[Id]): List[Id] =
      tree.child match
        case Some(child) => loop(child, tree.value.id +: acc)
        case None        => tree.value.id +: acc
    loop(this, Nil).reverse

  final def findPathReserve[Id](path: List[Id])(using HasId[A, Id]): Option[List[Tree[A]]] =
    @tailrec
    def loop(tree: Tree[A], path: List[Id], acc: List[Tree[A]]): Option[List[Tree[A]]] = path match
      case Nil                             => None
      case head :: Nil if tree.hasId(head) => (tree :: acc).some
      case head :: rest if tree.hasId(head) =>
        tree.child match
          case None        => None
          case Some(child) => loop(child, rest, tree :: acc)
      case head :: _ =>
        tree match
          case node: Node[A] =>
            node.findVariation(head) match
              case None            => None
              case Some(variation) => loop(variation, path, acc)
          case _ => None

    if path.isEmpty then None else loop(this, path, Nil)

  final def findPath[Id](path: List[Id])(using HasId[A, Id]): Option[List[Tree[A]]] =
    findPathReserve(path).map(_.reverse)

  final def find[Id](path: List[Id])(using HasId[A, Id]): Option[Tree[A]] =
    findPath(path).flatMap(_.lastOption)

  final def pathExists[Id](path: List[Id]): HasId[A, Id] ?=> Boolean =
    findPath(path).isDefined

  def modifyAt[Id](path: List[Id], f: TreeMapOption[A]): HasId[A, Id] ?=> Option[TreeSelector[A, this.type]]

  // modify the child of the node at the given path
  // if the chile does not exist, return None
  def modifyChildAt[Id](
      path: List[Id],
      f: Node[A] => Option[Node[A]]
  ): HasId[A, Id] ?=> Option[TreeSelector[A, this.type]]

  def deleteAt[Id](path: List[Id])(using HasId[A, Id]): Option[Option[TreeSelector[A, this.type]]]

  // promote a variation to a mainline
  // returns none if path does not exist
  // or all nodes in the path are Mainline nodes
  def promote[Id](id: Id)(using HasId[A, Id]): Option[TreeSelector[A, this.type]]

  // Add a value as a child or child's variation
  // if the tree has no child, add value as child
  // if the value has the same id as the child, merge the values
  // otherwise add value as a variation (and merge it to one of the existing variation if necessary)
  final def addValueAsChild[Id](value: A)(using HasId[A, Id], Mergeable[A]): TreeSelector[A, this.type] =
    addChild(Node(value))

  // Add a node as a child or child's variation
  def addChild[Id](other: Node[A])(using HasId[A, Id], Mergeable[A]): TreeSelector[A, this.type] =
    val newChild = child.fold(other)(_.mergeOrAddAsVariation(other))
    withChild(newChild.some)

  // find a node with path
  // if not found, return None
  // if found node has no child, add new value as a child
  // if found node has a child, add new value as it's child's variation
  def addValueAsChildAt[Id](path: List[Id], value: A)(using
      HasId[A, Id],
      Mergeable[A]
  ): Option[TreeSelector[A, this.type]] =
    modifyAt(path, _.addValueAsChild(value).some)

  def addChildAt[Id](path: List[Id], node: Node[A])(using
      HasId[A, Id],
      Mergeable[A]
  ): Option[TreeSelector[A, this.type]] =
    modifyAt(path, _.addChild(node).some)

type TreeSelector[A, X <: Tree[A]] <: Tree[A] = X match
  case Node[A]      => Node[A]
  case Variation[A] => Variation[A]

type TreeMapper[A]    = (tree: Tree[A]) => TreeSelector[A, tree.type]
type TreeMapOption[A] = (tree: Tree[A]) => Option[TreeSelector[A, tree.type]]

final case class Node[A](
    override val value: A,
    override val child: Option[Node[A]] = None,
    override val variations: List[Variation[A]] = Nil
) extends Tree[A](value, child)
    derives Functor,
      Traverse:

  import Tree.given

  final def mainline: List[Node[A]] =
    @tailrec
    def loop(tree: Node[A], acc: List[Node[A]]): List[Node[A]] =
      tree.child match
        case Some(child) => loop(child, tree :: acc)
        case None        => tree :: acc
    loop(this, Nil).reverse

  // take the first n nodes in the mainline
  // keep all variations
  // n > 0
  def take(n: Int): Node[A] =
    @tailrec
    def loop(n: Int, node: Node[A], acc: List[Node[A]]): List[Node[A]] =
      if n <= 0 then acc
      else
        node.child match
          case None        => node :: acc
          case Some(child) => loop(n - 1, child, node.withoutChild :: acc)
    if n == 0 then this
    else loop(n, this, Nil).foldLeft(none[Node[A]])((acc, node) => node.withChild(acc).some).getOrElse(this)

  // get the nth node of in the mainline
  def apply(n: Int): Option[Node[A]] =
    @tailrec
    def loop(tree: Option[Node[A]], n: Int): Option[Node[A]] =
      if n <= 0 then tree
      else loop(tree.flatMap(_.child), n - 1)
    loop(this.some, n)

  def modifyChildAt[Id](path: List[Id], f: Node[A] => Option[Node[A]]): HasId[A, Id] ?=> Option[Node[A]] =
    path match
      case Nil                             => f(this)
      case head :: Nil if this.hasId(head) => child.map(c => withChild(f(c)))
      case head :: rest if this.hasId(head) =>
        child.flatMap(_.modifyChildAt(rest, f)) match
          case None    => None
          case Some(c) => copy(child = c.some).some
      case _ =>
        variations.foldLeft((false, List.empty[Variation[A]])) {
          case ((true, acc), n) => (true, acc :+ n)
          case ((false, acc), n) =>
            n.modifyChildAt(path, f) match
              case Some(nn) => (true, acc :+ nn)
              case None     => (false, acc :+ n)
        } match
          case (true, ns) => copy(variations = ns).some
          case (false, _) => none

  def modifyAt[Id](path: List[Id], f: TreeMapOption[A]): HasId[A, Id] ?=> Option[Node[A]] =
    path match
      case Nil                             => None
      case head :: Nil if this.hasId(head) => f(this)
      case head :: rest if this.hasId(head) =>
        child.flatMap(_.modifyAt(rest, f)) match
          case None    => None
          case Some(c) => copy(child = c.some).some
      case head :: _ =>
        variations.foldLeft((false.some, List.empty[Variation[A]])):
          case ((Some(true), acc), n) => (true.some, n :: acc)
          case ((Some(fale), acc), n) if n.hasId(head) =>
            n.modifyAt(path, f) match
              case None     => (None, n :: acc)
              case Some(nn) => (true.some, nn :: acc)
          case ((x, acc), n) => (x, n :: acc)
        match
          case (Some(true), ns) => copy(variations = ns.reverse).some
          case _                => none

  // delete the node at the end of the path
  // return None if path is not found
  // return Some(None) if the node is deleted
  def deleteAt[Id](path: List[Id])(using HasId[A, Id]): Option[Option[Node[A]]] =
    path match
      case Nil                             => None
      case head :: Nil if this.hasId(head) => Some(None)
      case head :: rest if this.hasId(head) =>
        child.flatMap(_.deleteAt(rest)) match
          case None    => None
          case Some(c) => copy(child = c).some.some
      case head :: _ =>
        variations.foldLeft((false.some, List.empty[Variation[A]])) {
          case ((Some(true), acc), n) => (true.some, n :: acc)
          case ((Some(false), acc), n) if n.hasId(head) =>
            n.deleteAt(path) match
              case None     => (None, n :: acc)
              case Some(nn) => (true.some, nn ++: acc)
          case ((x, acc), n) => (x, n :: acc)
        } match
          case (Some(true), ns) => copy(variations = ns.reverse).some.some
          case _                => none

  def promoteToMainline[Id](path: List[Id])(using HasId[A, Id]): Option[Node[A]] = path match
    case Nil => None
    case head :: Nil =>
      promote(head)
    case head :: rest =>
      promote(head)
        .flatMap(x => x.child.flatMap(_.promoteToMainline(rest)).map(c => x.withChild(c.some)))

  // findPath
  // find the lastest variation in the path
  // promote it into mainline
  def promote[Id](path: List[Id])(using HasId[A, Id]): Option[Node[A]] = path match
    case Nil => None
    case head :: _ =>
      findPathReserve(path).flatMap: ps =>
        if ps.forall(_.isVariation) then this.promote(head)
        else if ps.forall(_.isNode) then None
        else
          ps.dropWhile(_.isNode).dropWhile(_.isVariation) match
            case Nil => this.promote(head)
            case rest =>
              val swappingNodePath = rest.map(_.id).reverse
              path
                .drop(rest.size)
                .headOption
                .flatMap: vId =>
                  this.modifyChildAt(swappingNodePath, _.promote(vId))

  // promote a variation to mainline if it exist
  def promote[Id](id: Id)(using HasId[A, Id]): Option[Node[A]] =
    if this.hasId(id) then this.some
    else
      variations.find(_.hasId(id)) match
        case None => None
        case Some(v) =>
          val vs = this.toVariations.removeById(id)
          v.toNode.withVariations(vs).some

  def clearVariations: Node[A] =
    this.withoutVariations.copy(child = child.map(_.clearVariations))

  // Akin to map, but allows to keep track of a state value when calling the function.
  // This is different from Traverse.mapAccumulate
  // because variations only use accumulated value from the parent
  def mapAccuml[S, B](init: S)(f: (S, A) => (S, B)): (S, Node[B]) =
    val (s1, b) = f(init, value)
    val v       = variations.map(_.mapAccuml(init)(f)._2)
    child.map(_.mapAccuml(s1)(f)) match
      case None    => (s1, Node(b, None, v))
      case Some(s) => (s._1, Node(b, s._2.some, v))

  def mapAccuml_[S, B](init: S)(f: (S, A) => (S, B)): Node[B] =
    mapAccuml(init)(f)._2

  // Akin to mapAccuml, return an Option[Node[B]]
  // when a node from mainline returns None, we stop traverse down that line
  // when a variation node returns None, we just ignore it and continue to traverse next variations
  // TODO: now if the f(value) is None, the whole tree is None
  // should we promote a variation to mainline if the f(value) is None?
  def mapAccumlOption[S, B](init: S)(f: (S, A) => (S, Option[B])): (S, Option[Node[B]]) =
    f(init, value) match
      case (s1, None) => (s1, None)
      case (s1, Some(b)) =>
        val vs = variations.map(_.mapAccumlOption(init)(f)._2).flatten
        child.map(_.mapAccumlOption(s1)(f)) match
          case None    => (s1, Node(b, None, vs).some)
          case Some(s) => (s._1, Node(b, s._2, vs).some)

  def mapAccumlOption_[S, B](init: S)(f: (S, A) => (S, Option[B])): Option[Node[B]] =
    mapAccumlOption(init)(f)._2

  // find node in the mainline
  @tailrec
  def findInMainline(predicate: A => Boolean): Option[Node[A]] =
    if predicate(value) then this.some
    else
      child.match
        case None    => None
        case Some(c) => c.findInMainline(predicate)

  def modifyInMainline(predicate: A => Boolean, f: Node[A] => Node[A]): Option[Node[A]] =
    if predicate(value) then f(this).some
    else
      child.flatMap(_.modifyInMainline(predicate, f)) match
        case Some(c) => withChild(c.some).some
        case None    => None

  @tailrec
  def lastMainlineNode: Node[A] =
    child match
      case None    => this
      case Some(c) => c.lastMainlineNode

  def modifyLastMainlineNode(f: Node[A] => Node[A]): Node[A] =
    child.fold(f(this))(c => withChild(c.modifyLastMainlineNode(f).some))

  // map values in the mainline
  // remove all variations
  def mapMainline[B](f: A => B): Node[B] =
    copy(value = f(value), child = child.map(_.mapMainline(f)), variations = Nil)

  // akin to mapMainline, but also pass index to the function
  def mapMainlineWithIndex[B](f: (A, Int) => B): Node[B] =
    def loop(n: Node[A], i: Int): Node[B] =
      n.copy(value = f(n.value, i), child = n.child.map(c => loop(c, i + 1)), variations = Nil)
    loop(this, 0)

  def toVariation: Variation[A]        = Variation(value, child)
  def toVariations: List[Variation[A]] = Variation(value, child) +: variations

  // we assume that they have the same path from the roof
  // merge two nodes
  // if they have same id, merge values and child, and variations
  // else add as variation
  def mergeOrAddAsVariation[Id](other: Node[A])(using Mergeable[A]): Node[A] =
    value.merge(other.value) match
      case Some(newValue) =>
        val newChild = Tree.merge(child, other.child)
        Node(newValue, newChild, variations.add(other.variations))
      case _ =>
        addVariations(other.toVariations)

  def addVariation[Id](v: Variation[A])(using Mergeable[A]): Node[A] =
    withVariations(variations.add(v))

  def addVariations[Id](vs: List[Variation[A]])(using Mergeable[A]): Node[A] =
    withVariations(variations.add(vs))

  def withVariations(variations: List[Variation[A]]): Node[A] =
    copy(variations = variations)

  def withoutVariations: Node[A] =
    copy(variations = Nil)

  // find the first variation by id
  def findVariation[Id](id: Id)(using HasId[A, Id]): Option[Variation[A]] =
    variations.find(_.hasId(id))

  // check if exist a variation that has Id
  def hasVariation[Id](id: Id)(using HasId[A, Id]): Boolean =
    variations.exists(_.hasId(id))

  def withoutChildAndVariations: Node[A] =
    copy(child = None, variations = Nil)

final case class Variation[A](override val value: A, override val child: Option[Node[A]] = None)
    extends Tree[A](value, child) derives Functor, Traverse:

  def modifyChildAt[Id](
      path: List[Id],
      f: Node[A] => Option[Node[A]]
  ): HasId[A, Id] ?=> Option[Variation[A]] =
    path match
      case Nil                             => None
      case head :: Nil if this.hasId(head) => child.map(c => withChild(f(c)))
      case head :: rest if this.hasId(head) =>
        child.flatMap(_.modifyChildAt(rest, f)) match
          case None    => None
          case Some(c) => copy(child = c.some).some
      case _ => None

  def modifyAt[Id](path: List[Id], f: TreeMapOption[A]): HasId[A, Id] ?=> Option[Variation[A]] =
    path match
      case Nil                             => None
      case head :: Nil if this.hasId(head) => f(this)
      case head :: rest if this.hasId(head) =>
        child.flatMap(_.modifyAt(rest, f)) match
          case None    => None
          case Some(c) => copy(child = c.some).some
      case _ => None

  // delete the node at the end of the path
  // return None if path is not found
  // return Some(None) if the node is deleted
  def deleteAt[Id](path: List[Id])(using HasId[A, Id]): Option[Option[Variation[A]]] =
    path match
      case Nil                             => None
      case head :: Nil if this.hasId(head) => Some(None)
      case head :: rest if this.hasId(head) =>
        child.flatMap(_.deleteAt(rest)) match
          case None    => None
          case Some(c) => copy(child = c).some.some
      case _ => None

  def promote[Id](id: Id)(using HasId[A, Id]): Option[Variation[A]] = None

  // Akin to map, but allows to keep track of a state value when calling the function.
  def mapAccuml[S, B](init: S)(f: (S, A) => (S, B)): (S, Variation[B]) =
    val (s1, b) = f(init, value)
    child.map(_.mapAccuml(s1)(f)) match
      case None    => (s1, Variation(b, None))
      case Some(s) => (s._1, Variation(b, s._2.some))

  // Akin to mapAccuml, return an Option[Node[B]]
  // when a node from mainline returns None, we stop traverse down that line
  // when a variation node returns None, we just ignore it and continue to traverse next variations
  // TODO: now if the f(value) is None, the whole tree is None
  // should we promote a variation to mainline if the f(value) is None?
  def mapAccumlOption[S, B](init: S)(f: (S, A) => (S, Option[B])): (S, Option[Variation[B]]) =
    f(init, value) match
      case (s1, None) => (s1, None)
      case (s1, Some(b)) =>
        child.map(_.mapAccumlOption(s1)(f)) match
          case None    => (s1, Variation(b, None).some)
          case Some(s) => (s._1, Variation(b, s._2).some)

  def toNode: Node[A] = Node(value, child)

object Tree:
  def lift[A](f: A => A): TreeMapper[A]          = tree => tree.withValue(f(tree.value))
  def liftOption[A](f: A => A): TreeMapOption[A] = tree => tree.withValue(f(tree.value)).some

  extension [A](tm: TreeMapper[A]) def toOption: TreeMapOption[A] = x => tm(x).some

  def merge[A](node: Option[Node[A]], other: Option[Node[A]]): Mergeable[A] ?=> Option[Node[A]] =
    (node, other) match
      case (Some(c1), Some(c2)) => Some(c1.mergeOrAddAsVariation(c2))
      case _                    => node.orElse(other)

  def build[A, B](s: Seq[A], f: A => B): Option[Node[B]] =
    s.reverse.foldLeft(none[Node[B]])((acc, a) => Node(f(a), acc).some)

  def buildWithIndex[A, B](s: Seq[A], f: (A, Int) => B): Option[Node[B]] =
    build(s.zipWithIndex, f.tupled)

  def buildWithNode[A, B](s: Seq[A], f: A => Node[B]): Option[Node[B]] =
    s.reverse match
      case Nil     => none[Node[B]]
      case x :: xs => xs.foldLeft(f(x))((acc, a) => f(a).withChild(acc.some)).some

  given [A, Id](using HasId[A, Id]): HasId[Tree[A], Id] =
    _.value.id

  given [A, Id](using HasId[A, Id]): HasId[Node[A], Id] =
    _.value.id

  given [A, Id](using HasId[A, Id]): HasId[Variation[A], Id] =
    _.value.id

  given [A](using Mergeable[A]): Mergeable[Variation[A]] with
    extension (a: Variation[A])
      def merge(other: Variation[A]): Option[Variation[A]] =
        a.value.merge(other.value) match
          case Some(v) =>
            val newChild = Tree.merge(a.child, other.child)
            Variation(v, newChild).some
          case _ => None
