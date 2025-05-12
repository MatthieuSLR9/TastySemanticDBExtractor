package dotty.tools.dotc.semanticdb

import scala.language.unsafeNulls

import java.net.URLClassLoader
import java.util.regex.Pattern
import java.io.File
import java.nio.file._
import java.nio.charset.StandardCharsets
import java.util.stream.Collectors
import java.util.Comparator
import scala.util.control.NonFatal
import scala.collection.mutable
import scala.jdk.CollectionConverters._

import javax.tools.ToolProvider

import org.junit.Assert._
import org.junit.Test
import org.junit.experimental.categories.Category

import dotty.BootstrappedOnlyTests
import dotty.tools.dotc.Main
import dotty.tools.dotc.semanticdb.Scala3.given
import dotty.tools.dotc.util.SourceFile


@Category(Array(classOf[BootstrappedOnlyTests]))
class TastySemanticdbTests:
  val javaFile = FileSystems.getDefault.getPathMatcher("glob:**.java")
  val scalaFile = FileSystems.getDefault.getPathMatcher("glob:**.scala")
  val expectFile = FileSystems.getDefault.getPathMatcher("glob:**.expect.scala")
  val rootSrc = Paths.get(System.getProperty("dotty.tools.dotc.semanticdb.test"))
  val expectSrc = rootSrc.resolve("expect")
  val javaRoot = rootSrc.resolve("javacp")
  val metacExpectFile = rootSrc.resolve("metac.expect")

  @Category(Array(classOf[dotty.SlowTests]))
  @Test def expectTestsTasty: Unit = 
    println("TASTY Running test...")
    if (!scala.util.Properties.isWin) runExpectTest(updateExpectFiles = false)

  def runExpectTest(updateExpectFiles: Boolean): Unit =

    val target = generateSemanticdb()
    val errors = mutable.ArrayBuffer.empty[Path]
    val metacSb: StringBuilder = StringBuilder(5000)
    def collectErrorOrUpdate(expectPath: Path, obtained: String) =
      if updateExpectFiles then
        Files.write(expectPath, obtained.getBytes(StandardCharsets.UTF_8))
        println("updated: " + expectPath)
      else
        val expected = new String(Files.readAllBytes(expectPath), StandardCharsets.UTF_8)
        val expectName = expectPath.getFileName
        val relExpect = rootSrc.relativize(expectPath)
        if expected.trim != obtained.trim then
          println("INCORRECT PATH IS:")
          Files.write(expectPath.resolveSibling("" + expectName + ".out"), obtained.getBytes(StandardCharsets.UTF_8))
          errors += expectPath
    
    for source <- inputFiles().sorted do
      val filename = source.getFileName.toString
      val relpath = expectSrc.relativize(source)
      val semanticdbPath = target
        .resolve("META-INF")
        .resolve("semanticdb")
        .resolve(relpath)
        .resolveSibling(filename + ".semanticdb")
      val expectPath = source.resolveSibling(filename.replace(".scala", ".expect.scala"))
      val doc = Tools.loadTextDocument(source, relpath, semanticdbPath)
      Tools.metac(doc, rootSrc.relativize(source))(using metacSb)
      val obtained = trimTrailingWhitespace(SemanticdbTests.printTextDocument(doc))
      collectErrorOrUpdate(expectPath, obtained)

    collectErrorOrUpdate(metacExpectFile, metacSb.toString)
    for expect <- errors do
      def red(msg: String) = Console.RED + msg + Console.RESET
      def blue(msg: String) = Console.BLUE + msg + Console.RESET
      println(s"""[${red("error")}] check file ${blue(expect.toString)} does not match generated.
      |If you meant to make a change, replace the expect file by:
      |  mv ${expect.resolveSibling("" + expect.getFileName + ".out")} $expect
      |inspect with:
      |  diff $expect ${expect.resolveSibling("" + expect.getFileName + ".out")}
      |Or else update all expect files with
      |  sbt 'scala3-compiler-bootstrapped/Test/runMain dotty.tools.dotc.semanticdb.updateExpect'""".stripMargin)
    Files.walk(target).sorted(Comparator.reverseOrder).forEach(Files.delete)
    if errors.nonEmpty then
      fail(s"${errors.size} errors in expect test.")

  def trimTrailingWhitespace(s: String): String =
    Pattern.compile(" +$", Pattern.MULTILINE).matcher(s).replaceAll("")

  def inputFiles(): List[Path] =
    val ls = Files.walk(expectSrc)
    val files =
      try ls.filter(p => scalaFile.matches(p) && !expectFile.matches(p)).collect(Collectors.toList).asScala
      finally ls.close()
    require(files.nonEmpty, s"No input files! $expectSrc")
    
    val filteredFiles = files.toList.filter { file =>
    file.getFileName.toString.startsWith("TastyQuery")}
    filteredFiles

  def javaFiles(): List[Path] =
    val ls = Files.walk(javaRoot)
    val files =
      try ls.filter(p => javaFile.matches(p)).collect(Collectors.toList).asScala
      finally ls.close()
    require(files.nonEmpty, s"No input files! $expectSrc")
    files.toList

  def generateSemanticdb(): Path =
    val target = Files.createTempDirectory("semanticdb")
    val javaArgs = Array("-d", target.toString) ++ javaFiles().map(_.toString)
    val javac = ToolProvider.getSystemJavaCompiler
    val exitJava = javac.run(null, null, null, javaArgs*)
    assert(exitJava == 0, "java compiler has errors")
    println(s"source root is : ${expectSrc.toString}")
    val args = Array(
      "-Ysdb-using-tasty",
      "-Xsemanticdb",
      "-d", target.toString,
      "-feature",
      "-deprecation",
      "-sourceroot", expectSrc.toString,
      "-classpath", target.toString,
      "-Xignore-scala2-macros",
      "-usejavacp",
      "-Wunused:all"
    ) ++ inputFiles().map(_.toString)
    
    val exit = Main.process(args)
    assertFalse(s"dotc errors: ${exit.errorCount}", exit.hasErrors)
    target

end TastySemanticdbTests