package dotty.tools.dotc
import tastyquery.SourcePosition
import scala.io.Source
import java.nio.file.{Paths, Path}
class TastyPositionExtractor(sourceFilePath: String):

  def extract(name: String, span: SourcePosition): dotty.tools.dotc.semanticdb.Range =
    if span.isZeroExtent then {
      new dotty.tools.dotc.semanticdb.Range(span.startLine,span.startColumn,span.endLine, span.endColumn)
    }
    else {
      val source = Source.fromFile(sourceFilePath)
      val text = source.getLines.zipWithIndex.collectFirst { case (line, idx) if idx == span.startLine  => line }.get
      val startColumn = text.indexOf(name, span.startColumn)
      if startColumn < 0 then {
        new dotty.tools.dotc.semanticdb.Range(span.startLine,span.startColumn,span.startLine, span.startColumn)

      }
      else {
        val endColumn = startColumn + name.size
        val selectedLines = source.slice(span.startLine, span.startLine)
        val sourceString = source.mkString
        val range = new dotty.tools.dotc.semanticdb.Range(span.startLine,startColumn,span.startLine, endColumn)
        range

      }
    }