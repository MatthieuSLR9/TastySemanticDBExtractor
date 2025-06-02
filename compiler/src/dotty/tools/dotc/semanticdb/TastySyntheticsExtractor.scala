package dotty.tools.dotc.semanticdb

import tastyquery.Trees.*
import tastyquery.Contexts.Context
import dotty.tools.dotc.SDBtype
import dotty.tools.dotc.{semanticdb => s}
import dotty.tools.dotc.Extensions.* 
import tastyquery.Symbols.TermSymbol
import dotty.tools.dotc.SDBSymbolNameBuilder
import dotty.tools.dotc.SDBtype.toSemanticType
import tastyquery.SourcePosition



class TastySyntheticsExtractor:
  import Scala3.{_, given}

  val visited = collection.mutable.HashSet[(Tree, SourcePosition)]()

  def tryFindSynthetic(tree: Tree)(using Context, SDBSymbolNameBuilder): Option[s.Synthetic] =
    extension (synth: s.Synthetic)
      def toOpt: Some[s.Synthetic] = Some(synth)

    if visited.contains((tree, tree.pos)) then None
    else
      tree match
        case tree: TypeApply
          if !tree.pos.isZeroExtent && (isSynthetic(tree) || tree.pos.isSynthetic) &&
            (tree.fun match {
              // for `Bar[Int]` of `class Foo extends Bar[Int]`
              // we'll have `TypeTree(Select(New(AppliedTypeTree(...))), List(Int))`
              // in this case, don't register `*[Int]` to synthetics as we already have `[Int]` in source.
              case Select(New(AppliedTypeTree(_, _)), _) => false
              case _ => true
            }) =>
          visited.add((tree, tree.pos))
          val fnTree = tree.fun match
            // Something like `List.apply[Int](1,2,3)`
            case select @ Select(qual, _) if isSynthetic(select) =>
              s.SelectTree(
                s.OriginalTree(tree.pos.range),
                Some(select.toSemanticId)
              )
            case _ =>
              tree.pos.endColumn
              s.OriginalTree(
                tree.fun.pos.range
              )
          val targs = tree.args.map(x => x.toType.toSemanticType("", None))
          s.Synthetic(
            tree.fun.pos.range,
            s.TypeApplyTree(
              fnTree, targs
            )
          ).toOpt
        case _ => None
  //work around as the synthetic flag seems to not be working
  private def isSynthetic(typeApply: TypeApply): Boolean =
    typeApply.args match
      case head :: next =>
        if (head.pos.isSynthetic) then true
        else{
          typeApply.pos.endOffset == head.pos.endOffset
        }
      case Nil => false
  
  private def isSynthetic(select: Select): Boolean =
    select.pos.endOffset == select.qualifier.pos.endOffset

  extension (select: tastyquery.Trees.Select)
    private def toSemanticId(using Context, SDBSymbolNameBuilder) =
      s.IdTree(select.symbol.SDBname)
  
end TastySyntheticsExtractor
