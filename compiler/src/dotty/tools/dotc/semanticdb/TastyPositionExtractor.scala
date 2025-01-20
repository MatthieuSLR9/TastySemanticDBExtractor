package dotty.tools.dotc
import tastyquery.SourcePosition
import tastyquery.Symbols.*
import scala.io.Source
import java.nio.file.{Paths, Path}
import tastyquery.Symbols
import tastyquery.Trees.ClassDef
import tastyquery.Trees.TypeMember
import tastyquery.Trees.TypeParam
import tastyquery.Trees.ValDef
import tastyquery.Trees.DefDef
import tastyquery.Trees.Bind
import tastyquery.Trees.TypeTreeBind
import tastyquery.Trees.Ident
import tastyquery.Trees.Select
import tastyquery.Trees.SelectOuter
import tastyquery.Trees.This
import tastyquery.Trees.Super
import tastyquery.Trees.Apply
import tastyquery.Trees.TypeApply
import tastyquery.Trees.New
import tastyquery.Trees.Typed
import tastyquery.Trees.Assign
import tastyquery.Trees.NamedArg
import tastyquery.Trees.Block
import tastyquery.Trees.If
import tastyquery.Trees.InlineIf
import tastyquery.Trees.Lambda
import tastyquery.Trees.Match
import tastyquery.Trees.InlineMatch
import tastyquery.Trees.SeqLiteral
import tastyquery.Trees.While
import tastyquery.Trees.Throw
import tastyquery.Trees.Try
import tastyquery.Trees.Literal
import tastyquery.Trees.Return
import tastyquery.Trees.Inlined
import tastyquery.Trees.Quote
import tastyquery.Trees.Splice
import tastyquery.Trees.SplicePattern
import tastyquery.Trees.Template
import scala.io.BufferedSource

def extractInLine(name: String, line: String, span: SourcePosition, source: BufferedSource): dotty.tools.dotc.semanticdb.Range = 
  source.close()
  val foundStartColumn =  
    if span.startLine == span.endLine then
      line.lastIndexOf(name, span.endColumn)
    else
      line.lastIndexOf(name)
  val foundEndColumn = foundStartColumn + name.size
  if foundStartColumn < 0 then {
    new dotty.tools.dotc.semanticdb.Range(span.startLine,span.startColumn,span.startLine, span.startColumn)
  }
  else           
    new dotty.tools.dotc.semanticdb.Range(span.startLine,foundStartColumn,span.startLine, foundEndColumn)

class TastyPositionExtractor(sourceFilePath: String):

  def extract(name: String, span: SourcePosition, sym: Symbol): dotty.tools.dotc.semanticdb.Range =
    if span.isZeroExtent then {
      new dotty.tools.dotc.semanticdb.Range(span.startLine,span.startColumn,span.endLine, span.endColumn)
    }
    else {
      val source = Source.fromFile(sourceFilePath)
      source.getLines.zipWithIndex.collectFirst { case (line, idx) if idx == span.startLine  => line } match

        case None =>
          source.close()
          new dotty.tools.dotc.semanticdb.Range(span.startLine,span.startColumn,span.endLine, span.endColumn)
        
        case Some(line) => 
          sym.tree match
            case None => 
              extractInLine(name, line, span, source)
            case Some(tree) => 
              tree match
                case classDef : ClassDef =>
                  classDef.rhs match
                    case template : Template => 
                      template.body match
                        case head :: next => 
                          if head.pos.startLine == span.startLine then 
                            val searchingEndColumn = head.pos.startColumn
                            val cutString = line.substring(0, searchingEndColumn).nn
                            extractInLine(name, cutString, span, source)
                          else
                            extractInLine(name, line, span, source)
                        case Nil =>
                          extractInLine(name, line, span, source)

                case defDef : DefDef =>
                  if span== defDef.pos then
                    defDef.paramLists match
                      case head :: next => 
                        head match
                          case Left(listValdDef) => 
                            listValdDef match
                              case head :: next => 
                                if head.pos.endLine == span.endLine then 
                                  val searchingEndColumn = head.pos.startColumn
                                  val cutString = line.substring(0, searchingEndColumn).nn
                                  extractInLine(name, cutString, span, source)
                                else
                                  extractInLine(name, line, span, source)
                              case Nil =>
                                extractInLine(name, line, span, source)
                          case Right(listTypeParam) =>
                            listTypeParam match
                              case head :: next => 
                                if head.pos.startLine == span.startLine then
                                  val searchingEndColumn = listTypeParam.head.pos.startColumn
                                  val cutString = line.substring(0, searchingEndColumn).nn
                                  extractInLine(name, cutString, span, source)
                                else
                                  extractInLine(name, line, span, source)
                              case Nil =>
                                extractInLine(name, line, span, source)
                        
                      case Nil =>
                        defDef.rhs match
                          case None => 
                            extractInLine(name, line, span, source)
                          case Some(tree2) =>
                            if tree2.pos.startLine != span.startLine then
                              extractInLine(name, line, span, source)
                            else 
                              val searchingEndColumn = tree2.pos.startColumn
                              val cutString = line.substring(0, searchingEndColumn).nn
                              extractInLine(name, cutString, span, source)
                  else 
                    extractInLine(name, line, span, source)

                case valDef : ValDef => 
                  val span2 = valDef.pos

                  if span == span2 then
                    val tptPos = valDef.tpt.pos 
                    if tptPos.isUnknown then 
                      valDef.rhs match
                        case None => 
                          println("noRHS")
                          extractInLine(name, line, span, source)

                        case Some(tree) => 
                          if span== span2 then
                            val searchingEndColumn = tree.pos.startColumn
                            val cutString = line.substring(0, searchingEndColumn).nn
                            extractInLine(name, cutString, span, source)
                          else 
                            extractInLine(name, line, span, source)
                    else
                      if (!tptPos.isZeroExtent && tptPos.endLine == span.startLine) then 
                        val searchingEndColumn = tptPos.startColumn
                        val cutString = line.substring(0, searchingEndColumn).nn
                        extractInLine(name, cutString, span, source)
                      else
                        extractInLine(name, line, span, source)
                  else
                    extractInLine(name, line, span, source)


                case bind : Bind =>
                  val span2 = bind.pos
                  if span == span2 then
                    val pos3 = bind.body.pos 
                    if (!pos3.isZeroExtent && pos3.endLine == span.endLine) then 
                      val searchingEndColumn = pos3.startColumn
                      val cutString = line.substring(0, searchingEndColumn).nn
                      extractInLine(name, cutString, span, source)
                    else
                      extractInLine(name, line, span, source)
                  else
                    extractInLine(name, line, span, source)

                case _ =>
                    extractInLine(name, line, span, source)
              
    }