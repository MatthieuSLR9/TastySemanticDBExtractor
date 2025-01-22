
package dotty.tools.dotc
import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Types.*
import tastyquery.Names.*
import tastyquery.Modifiers.*
import tastyquery.Trees.*

import scala.annotation.tailrec
import tastyquery.Symbols.*

import scala.collection.mutable
import _root_.dotty.tools.dotc.semanticdb.Scala3.*
import _root_.dotty.tools.dotc.semanticdb.Scala3.StringOps
import _root_.dotty.tools.dotc.symbolKind
import dotty.tools.dotc.Extensions.* 
import tastyquery.SourcePosition

            
class SDBSymbolNameBuilder:
  var nextLocalIdx: Int = 0
  val locals = mutable.HashMap[Symbol, Int]()
  val symsAtOffset = new mutable.HashMap[SourcePosition, mutable.Set[Symbol]]
  
  def symbolName(sym: Symbol)(using Context): String =
      
      val b = StringBuilder(20)
      addSymName(b, sym)
      b.toString

  def symbolName(sym: TastyFakeSymbol)(using Context): String =
    sym match
      case sym: TastyWildcardTypeSymbol=> 
        val b = StringBuilder(20)
        addSymName(b, sym.owner)
        b.append('['); addName(b, sym.name); b.append(']')
        b.toString
      case sym: TastyTermParamRefSymbol=>
        val b = StringBuilder(20)
        addSymName(b, sym.owner)
        b.append('('); addName(b, sym.name); b.append(')')
        b.toString

      case sym: TastyTypeParamRefSymbol=>
        val b = StringBuilder(20)
        addSymName(b, sym.owner)
        b.append('['); addName(b, sym.name); b.append(']')
        b.toString
      case sym:  TastyRefinementSymbol=>
        val b = StringBuilder(20)
        addLocalSymName(b)
        b.toString
      

  def addName(b: StringBuilder, name: Name): Unit =
      val str = name.toString.unescapeUnicode
      if str.isJavaIdent then b append str
      else b append '`' append str append '`'
  
  private def addLocalSymName(b: StringBuilder): Unit =
    val idx = nextLocalIdx
    nextLocalIdx += 1
    b.append(Symbols.LocalPrefix).append(idx)

  def addSymName(b: StringBuilder, sym: Symbol)(using Context):Unit = {
      
    def addOwner(owner: Symbol): Unit = {

      if !owner.isRoot then addSymName(b, owner)
    }

    def addDescriptor(sym: Symbol)(using Context): Unit = {

        if sym.isClass && sym.asClass.isModuleClass then
          addDescriptor(sym.asClass.moduleValue.get)
        else if sym.isRoot then b.append("_root_/")
        else if sym.isEmptyPackage then b.append("_empty_/")
        else if sym.isInstanceOf[TermSymbol] && sym.asInstanceOf[TermSymbol].isParam then
          b.append('('); addName(b, sym.name); b.append(')')
        else if sym.isInstanceOf[TypeParamSymbol] then {
          b.append('[')
          addName(b, sym.name)
          b.append(']')
          }
        else
          addName(b, sym.name)
          if sym.isPackage then
            b.append('/')
          else if sym.isType then b.append('#')
          else if (sym.isMethod || sym.isVar)
          && (!sym.isStableMember || sym.isConstructor) then
            b.append('('); 
            addOverloadIdx(sym);
            b.append(").")
          else b.append('.')
      }

    def addOverloadIdx(initSym: Symbol): Unit =
      sym match
        case termSymbol: TermSymbol => 
          sym.owner match
            case owner: ClassSymbol =>
              
              val decls = owner.declarations.filter(x=> x.name == initSym.name)
              val alts = decls.filter( x => x.isMethod || (x.asInstanceOf[TermSymbol].kind == TermSymbolKind.Var)).partition(x => !x.isSynthetic).toList.flatten
              
              alts match
                case head :: next => 
                  val idx = next.indexWhere(x => x.isTerm && x.asInstanceOf[TermSymbol].signature == termSymbol.signature)
                  if (idx >= 0){
                    b.append('+').append(idx + 1)
                  }
                case _ =>
            case _ => 
        case _ =>

    def computeLocalIdx(sym: Symbol)(using Context): Int = 

      val startPos = sym.tree match
        case None => None
        case Some(value) => if value.pos.isUnknown then None else Some(value.pos)//if value.pos.exists then Some(value.pos.span.start) else 
      locals get sym match
          case Some(idx) => idx
          case None => 
            (for {
            pos <- startPos
            syms <- symsAtOffset.get(pos)
            found <- syms.find(_.name == sym.name)
          } yield found) match
            case Some(other) => computeLocalIdx(other)
            case None =>
              val idx = nextLocalIdx
              nextLocalIdx += 1
              locals(sym) = idx
              symsAtOffset.get(startPos.get).foreach(_ += sym)
              idx
      
    if !sym.isGlobal then
      b.append("local").append(computeLocalIdx(sym))
    else if sym.owner != null then 
      addOwner(sym.owner);
      addDescriptor(sym);
  }
