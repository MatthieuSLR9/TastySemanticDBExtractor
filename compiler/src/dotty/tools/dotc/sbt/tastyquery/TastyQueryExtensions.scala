package dotty.tools.dotc

import tastyquery.Contexts.*
import tastyquery.Symbols.*
import tastyquery.Types.*
import tastyquery.Names.*
import tastyquery.Modifiers.*
import tastyquery.Trees.*

import scala.annotation.tailrec
import tastyquery.Names
import dotty.tools.dotc.core.SourceLanguage
import dotty.tools.dotc.core.StdNames.str

extension (name: TermName) def isPackageObjectName: Boolean = name match
  case SimpleName(name) => name == "package" || name.endsWith("package$")
  case _                => false

extension (name: UnsignedTermName) def isSetterName: Boolean = name match
  case SimpleName(name) => name == "_$eq" || name.endsWith("_=")
  case _                => false

extension (name: TypeName) def isPackageObjectClassName: Boolean = name match
  case ObjectClassTypeName(underlying) => underlying.toTermName.isPackageObjectName
  case _                               => false

extension (sym: dotty.tools.dotc.semanticdb.Scala3.TastyFakeSymbol){
  def SDBFakeSymName (using builder: SDBSymbolNameBuilder)(using Context): String =
    builder.symbolName(sym)
}
object Extensions: 
  extension (name: Name)(using Context)
    def toTermName: TermName = name match
      case name: TypeName => name.toTermName
      case name: TermName => name
    end toTermName    

    def toTypeName: TypeName = name match
      case name: TypeName => name
      case name: TermName => name.toTypeName
    end toTypeName

  end extension

  extension (sym: Symbol)(using Context)
    def SDBname (using builder: SDBSymbolNameBuilder): String =
      builder.symbolName(sym)

    /** Is this symbol the root class or its companion object? */
    def isRoot: Boolean =
      (sym.owner == null) && sym.name.toTermName == Names.nme.RootName || sym.name == Names.nme.UserLandRootPackageName

    /** Is this symbol the empty package class or its companion object? */
    def isEmptyPackage: Boolean =
      val owner = sym.owner
      sym.name.toTermName == Names.nme.EmptyPackageName && owner != null && owner.isRoot
    end isEmptyPackage

    /** Is this symbol the empty package class or its companion object? */
    def isEffectiveRoot: Boolean = sym.isRoot || sym.isEmptyPackage

    def isConstructor: Boolean =
      sym.name == nme.Constructor
    end isConstructor
    def isPrimary: Boolean =
      sym.owner match
        case owner: ClassSymbol => owner.tree.get.rhs.constr.symbol == sym
        case _ => false
    def isTopLevel: Boolean =
      val owner = sym.owner
      owner != null && owner.isPackage

    /** Is this symbol directly owner by a term symbol, i.e., is it local to a block? */
    def isLocalToBlock: Boolean =
      val owner = sym.owner
      owner != null && owner.isTerm

    /** Is symbol directly or indirectly owned by a term symbol? */
    @tailrec final def isLocal: Boolean = {
      val owner = sym.owner
      if (owner == null) false
      else if (isLocalToBlock) true
      else if (owner.isPackage) false
      else owner.isLocal
    }

    private inline def predicateAs[T <: Symbol](inline p: T => Boolean): Boolean = (sym: @unchecked) match
        case sym: T => p(sym)
        case _ => false
    end predicateAs

    // `ClassSymbol` predicates
    def isTrait: Boolean =
      predicateAs[ClassSymbol](_.isTrait)
    end isTrait

    def isAbstractClass: Boolean =
      predicateAs[ClassSymbol](_.isAbstractClass)
    end isAbstractClass
    
    private inline def hasOpenLevel(inline level: OpenLevel): Boolean =
      predicateAs[ClassSymbol](_.openLevel == level)
    end hasOpenLevel

    def isFinal: Boolean = sym match
      case sym: ClassSymbol => sym.openLevel == OpenLevel.Final
      case sym: TermOrTypeSymbol => sym.isFinalMember
      case _ => false
    end isFinal
    
    def isSealed: Boolean =
      hasOpenLevel(OpenLevel.Sealed)
    end isSealed

    def isPackageClass: Boolean =
      sym.isPackage
    end isPackageClass

    // `TermSymbol` predicates

    def isAbstractOverride: Boolean =
      predicateAs[TermSymbol](_.isAbstractOverride)
    end isAbstractOverride

    def isAbstractMember: Boolean =
      predicateAs[TermSymbol](_.isAbstractMember)
    end isAbstractMember

    def isCovariant: Boolean =
      predicateAs[ClassTypeParamSymbol](_.variance == Variance.Covariant)
    end isCovariant
    def isContravariant: Boolean =
      predicateAs[ClassTypeParamSymbol](_.variance == Variance.Contravariant)
    end isContravariant

    def isEnum: Boolean =
      predicateAs[ClassSymbol](_.isEnum)
    end isEnum

    def isGivenOrUsing: Boolean =
      predicateAs[TermSymbol](_.isGivenOrUsing)
    end isGivenOrUsing

    def isJavaDefined: Boolean =
      predicateAs[TermOrTypeSymbol](_.sourceLanguage == SourceLanguage.Java)
    end isJavaDefined

    def isImplicit: Boolean =
      predicateAs[TermSymbol](_.isImplicit)
    end isImplicit

    private inline def hasKind(kind: TermSymbolKind): Boolean =
      predicateAs[TermSymbol](_.kind == kind)
    end hasKind

    def isLazyVal: Boolean =
      hasKind(TermSymbolKind.LazyVal)
    end isLazyVal

    def isMacro: Boolean =
      predicateAs[TermSymbol](_.isMacro)
    end isMacro
    def isStableMember: Boolean =
      predicateAs[TermSymbol](_.isStableMember)
    end isStableMember
    def isInline: Boolean =
      predicateAs[TermSymbol](_.isInline)
    end isInline

    def isMethod: Boolean =
      predicateAs[TermSymbol](_.isMethod)
    end isMethod

    def isVar: Boolean =
      predicateAs[TermSymbol](_.kind == TermSymbolKind.Var)
    end isVar
    
    def isModuleVal: Boolean =
      predicateAs[TermSymbol](_.isModuleVal)
    end isModuleVal


    def isParamWithDefault: Boolean =
      predicateAs[TermSymbol](_.isParamWithDefault)
    end isParamWithDefault

    def isParamAccessor: Boolean =
      predicateAs[TermSymbol](_.isParamAccessor)
    end isParamAccessor
    def isGlobal: Boolean =
      sym.isPackage || ((sym.isInstanceOf[TermOrTypeSymbol] && sym.asInstanceOf[TermOrTypeSymbol].isParam)  || (sym.owner!= null && (sym.owner.nn.isClass || sym.owner.isPackage)))
      && (sym.owner!= null && sym.owner.nn.isGlobal)
    end isGlobal
  end extension

  extension (sym: TermOrTypeSymbol) 
    def isParam: Boolean =
      sym.owner.isTerm && sym.owner.asTerm.isTerm
        && sym.owner.asTerm.paramSymss.exists { p =>
          p match
            case Left(termParams)  => termParams.contains(sym)
            case Right(typeParams) => typeParams.contains(sym)
        }
    
end Extensions