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
      
      source.getLines.zipWithIndex.collectFirst { case (line, idx) if idx == span.startLine  => line } match
        case None => 
          val source2 = Source.fromFile(sourceFilePath)
          println(source2.getLines().foreach(println))
          source2.close()
          println(sourceFilePath)
          println(source.getLines().map(println(_)).toString())
          println(span.sourceFile.name)
          println(name)
          println("Very weird case")
          source.close()
          new dotty.tools.dotc.semanticdb.Range(span.startLine,span.startColumn,span.endLine, span.endColumn)
        case Some(value) =>
          
      
          val startColumn = value.indexOf(name, span.startColumn)
          if startColumn < 0 then {
            source.close()
            new dotty.tools.dotc.semanticdb.Range(span.startLine,span.startColumn,span.startLine, span.startColumn)
            

          }
          else {
            val endColumn = startColumn + name.size
            val selectedLines = source.slice(span.startLine, span.startLine)
            val sourceString = source.mkString
            source.close()
            new dotty.tools.dotc.semanticdb.Range(span.startLine,startColumn,span.startLine, endColumn)
            

          }
    }