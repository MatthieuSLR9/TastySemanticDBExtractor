package dotty.tools.dotc

import dotty.tools.dotc.sbt.*
import tastyquery.Names as tqn
import tastyquery.Classpaths.*
import tastyquery.Symbols.*
import tastyquery.Names.*
import tastyquery.Symbols.*
import core.*
import Contexts.*
import tastyquery.Contexts.Context
import tastyquery.Trees.*
import tastyquery.Symbols.*

import tastyquery.Classpaths.*
import scala.collection.mutable.{Map, ArrayBuffer}
import dotty.tools.dotc.semanticdb.ExtractSemanticDB


import dotty.tools.dotc.Extensions.*
import dotty.tools.dotc.config.Settings.*
import dotty.tools.dotc.semanticdb.Range.*
import dotty.tools.dotc.config.Settings.Setting.value
import dotty.tools.dotc.semanticdb.SymbolInformation
import dotty.tools.dotc.semanticdb.SymbolInformation.Kind.PACKAGE_OBJECT.isPackageObject
import dotty.tools.dotc.semanticdb.Scala3.SymbolKind
import tastyquery.Types.NoPrefix
import tastyquery.Types.*
import tastyquery.Annotations
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.semanticdb.SymbolInformation.Kind.*
import dotty.tools.dotc.semanticdb.PrivateThisAccess
import dotty.tools.dotc.semanticdb.Access
import dotty.tools.dotc.semanticdb.PrivateAccess
import dotty.tools.dotc.semanticdb.ProtectedThisAccess
import dotty.tools.dotc.semanticdb.AccessMessage.SealedValue.ProtectedAccess
import dotty.tools.dotc.semanticdb.AccessMessage.SealedValue.ProtectedWithinAccess
import dotty.tools.dotc.semanticdb.PrivateWithinAccess
import dotty.tools.dotc.semanticdb.PublicAccess
import tastyquery.Modifiers.TermSymbolKind
import tastyquery.Modifiers.Visibility
import tastyquery.Modifiers.Variance
import dotty.tools.dotc.SDBtype.toSemanticSig
import dotty.tools.dotc.semanticdb.LinkMode
import scala.collection.mutable.ListBuffer
import dotty.tools.dotc.semanticdb.Scala3.symbolName
import dotty.tools.dotc.SDBtype.toSemanticType
import dotty.tools.dotc.SymbolScopeOps.sscopeOpt
import dotty.tools.dotc.semanticdb.Scala3.StringOps
import tastyquery.SourcePosition
import dotty.tools.dotc.semanticdb.SymbolOccurrence
import scala.collection.mutable
 import dotty.tools.dotc.SymbolScopeOps.sscopeOpt
import dotty.tools.dotc.ast.Trees.NamedDefTree
import tastyquery.Traversers.TreeTraverser
import dotty.tools.dotc.semanticdb.Scala3.name
import dotty.tools.dotc.semanticdb.Synthetic
import dotty.tools.dotc.semanticdb.SemanticSymbolBuilder
import dotty.tools.dotc.semanticdb.Scala3.TastyFakeSymbol




import dotty.tools.dotc.SDBSymbolNameBuilder
import dotty.tools.dotc.core.StdNames.str
import dotty.tools.dotc.semanticdb.TastySyntheticsExtractor

extension (sym : Symbol){
  
  def symbolProps(symkinds: Set[SymbolKind])(using Context): Int =

    var props = 0
    if sym.isTerm && sym.asTerm.isPrimary then
      props |= SymbolInformation.Property.PRIMARY.value
    if sym.isFinal then
      props |= SymbolInformation.Property.FINAL.value
    if sym.isSealed then
      props |= SymbolInformation.Property.SEALED.value
    if sym.isGivenOrUsing || sym.isImplicit then
      props |= SymbolInformation.Property.IMPLICIT.value
    if sym.isLazyVal then
      props |= SymbolInformation.Property.LAZY.value
    if sym.isCovariant then
      props |= SymbolInformation.Property.COVARIANT.value
    if sym.isContravariant then
      props |= SymbolInformation.Property.CONTRAVARIANT.value
    if symkinds.exists(_.isVal) then
      props |= SymbolInformation.Property.VAL.value
    if symkinds.exists(_.isVar) then
      props |= SymbolInformation.Property.VAR.value
    props

  def symbolAccess(kind: SymbolInformation.Kind )(using Context, SDBSymbolNameBuilder): Access = {
    kind match
      case LOCAL => Access.Empty
      case PARAMETER => Access.Empty
      case SELF_PARAMETER => Access.Empty
      case TYPE_PARAMETER => Access.Empty
      case PACKAGE => Access.Empty
      case PACKAGE_OBJECT => Access.Empty
      case _ =>
        sym.asInstanceOf[TermOrTypeSymbol].visibility match
          case Visibility.PrivateThis => PrivateThisAccess()
          case tastyquery.Modifiers.Visibility.Private => dotty.tools.dotc.semanticdb.PrivateAccess()
          case Visibility.ProtectedThis => dotty.tools.dotc.semanticdb.ProtectedThisAccess()
          case tastyquery.Modifiers.Visibility.Protected => dotty.tools.dotc.semanticdb.ProtectedAccess()
          case Visibility.ScopedProtected(scope) => scope match
            case x: ClassSymbol =>
              val ssym = x.SDBname
              x.visibility match
                case Visibility.ProtectedThis => dotty.tools.dotc.semanticdb.ProtectedWithinAccess(ssym)
                case Visibility.ScopedProtected(scope) => dotty.tools.dotc.semanticdb.ProtectedWithinAccess(ssym)
                case _ => dotty.tools.dotc.semanticdb.PrivateWithinAccess(ssym)

            case x: PackageSymbol => PublicAccess()
          case Visibility.ScopedPrivate(scope) =>scope match
            case x: ClassSymbol =>
              val ssym = x.SDBname
              x.visibility match
                case Visibility.ProtectedThis => dotty.tools.dotc.semanticdb.ProtectedWithinAccess(ssym)
                case Visibility.ScopedProtected(scope) => dotty.tools.dotc.semanticdb.PrivateWithinAccess(ssym)
                case _ => PrivateWithinAccess(ssym)
            case _: PackageSymbol => PublicAccess()

          case Visibility.Public => PublicAccess()
  }

  def symbolKind(symkinds: Set[SymbolKind])(using ctx: Context): SymbolInformation.Kind = {
    
    if sym.isType && sym.isInstanceOf[TypeParamSymbol] then
      SymbolInformation.Kind.TYPE_PARAMETER
    else if sym.isTerm && sym.asTerm.isParam then
      SymbolInformation.Kind.PARAMETER 
    else if sym.isTerm && sym.isLocal then
      SymbolInformation.Kind.LOCAL
    else if sym.isMacro then
      SymbolInformation.Kind.MACRO
    else if sym.isConstructor then
      SymbolInformation.Kind.CONSTRUCTOR
    else if sym.isMethod || symkinds.exists(_.isVarOrVal) then
      SymbolInformation.Kind.METHOD
    else if sym.isClass && sym.asClass.name.isPackageObjectClassName then
      SymbolInformation.Kind.PACKAGE_OBJECT
    else if sym.isModuleVal then
      SymbolInformation.Kind.OBJECT
    else if sym.isPackage then
      SymbolInformation.Kind.PACKAGE
    else if sym.isTrait then
      SymbolInformation.Kind.TRAIT
    else if sym.isClass then
      SymbolInformation.Kind.CLASS
    else if sym.isType then
      SymbolInformation.Kind.TYPE
    else if sym.isParamAccessor then
      SymbolInformation.Kind.FIELD
    else
      SymbolInformation.Kind.UNKNOWN_KIND
  }
  def overriddenSymbols(using Context, SDBSymbolNameBuilder): List[String] =
    sym match
      case x: TermOrTypeSymbol => x.allOverriddenSymbols.map(_.SDBname).toList
      case x: PackageSymbol => List.empty

  def symbolAnnotations(using ctx: Context)(using SDBSymbolNameBuilder): List[dotty.tools.dotc.semanticdb.Annotation] =
    val child = ctx.findTopLevelClass("scala.annotation.internal.Child")
    val body = ctx.findTopLevelClass("scala.annotation.internal.Body")

    sym.annotations.collect{
      case annot
      if annot.symbol != child
      && annot.symbol != body
      && false
      => dotty.tools.dotc.semanticdb.Annotation(annot.tree.tpe.toSemanticType(sym.SDBname, Some(annot.symbol.asInstanceOf[TermSymbol])))

  }

  def replaceUnitInMethodType(methodType: TypeOrMethodic, newResultType: Type)(using ctx: Context): TypeOrMethodic = {
    val unitSym = ctx.defn.scalaPackage.findDecl(typeName("Unit"))
    methodType match
      case mt: MethodType =>
        val newResult = replaceUnitInMethodType(mt.resultType, newResultType)

        MethodType(mt.paramNames, mt.paramInfos, newResult)
        MethodType(mt.paramNames)(
          pt2 => mt.instantiateParamTypes(pt2.paramRefs),
          pt2 => replaceUnitInMethodType(mt.instantiate(pt2.paramRefs), newResultType)
        )
      case pt: PolyType =>
        PolyType(pt.paramNames)(
          pt2 => pt.instantiateParamTypeBounds(pt2.paramRefs),
          pt2 => replaceUnitInMethodType(pt.instantiate(pt2.paramRefs), newResultType)
        )
      case t : TypeRef if t.optSymbol.contains(unitSym) =>
        newResultType
      case _ => methodType
  }

  def symbolInfo(symkinds: Set[SymbolKind])(using ctx: Context)(using SDBSymbolNameBuilder): SymbolInformation = {
    val kind = sym.symbolKind(symkinds)

    sym match
      case termSymbol:  TermSymbol => {

        val kind = sym.symbolKind(symkinds)
        val annotations = termSymbol.symbolAnnotations
        val signature = if (kind == SymbolInformation.Kind.CONSTRUCTOR) then {
          val nonTranformed = termSymbol.declaredType
          val res = replaceUnitInMethodType(nonTranformed, termSymbol.owner.asClass.appliedRefInsideThis)
          res.toSemanticSig(using LinkMode.SymlinkChildren)(sym.asInstanceOf[TermSymbol])
        }
        else {

          termSymbol.declaredType.toSemanticSig(using LinkMode.SymlinkChildren)(sym.asInstanceOf[TermSymbol])
        }

        SymbolInformation(
            symbol = sym.SDBname,
            language = dotty.tools.dotc.semanticdb.Language.SCALA,
            kind = kind,
            properties = symbolProps(symkinds),
            displayName = termSymbol.name.toString(),
            access = termSymbol.symbolAccess(kind),
            overriddenSymbols = termSymbol.overriddenSymbols,
            annotations = termSymbol.symbolAnnotations,
            signature = signature
            )
        
      }

      case typeSymbol : TypeMemberSymbol => {
        val signature = typeSymbol.declaredBounds.high.toSemanticSig(using LinkMode.SymlinkChildren)(typeSymbol)
        SymbolInformation(
            symbol = sym.SDBname,
            language = dotty.tools.dotc.semanticdb.Language.SCALA,
            kind = kind,
            properties = symbolProps(symkinds),
            displayName = sym.name.toString(),
            overriddenSymbols = sym.overriddenSymbols,
            signature = signature
            )
      }

      case classSymbol: ClassSymbol => {

        val stparams = classSymbol.typeParams.sscopeOpt(using LinkMode.SymlinkChildren)
        val sparents = classSymbol.parents.map(_.toSemanticType(sym.SDBname, None))
        val sself = classSymbol.appliedRefInsideThis.toSemanticType(sym.SDBname, None)
        val typeParams = classSymbol.typeParams.toSet
        val declsSet = classSymbol.declarations.toSet
        val decls = (typeParams ++ declsSet).toList.sscopeOpt(using LinkMode.SymlinkChildren)
        val signature = semanticdb.ClassSignature(stparams, sparents, sself, decls)

        SymbolInformation(
            symbol = sym.SDBname,
            language = dotty.tools.dotc.semanticdb.Language.SCALA,
            kind = kind,
            properties = symbolProps(symkinds),
            displayName = sym.name.toString(),
            overriddenSymbols = sym.overriddenSymbols,
            signature = signature
            )
      }
      case _ => {
        SymbolInformation(
            symbol = sym.SDBname,
            language = dotty.tools.dotc.semanticdb.Language.SCALA,
            kind = kind,
            properties = symbolProps(symkinds),
            displayName = sym.name.toString(),
            overriddenSymbols = sym.overriddenSymbols
            )
  }}
}

class CustomTreeTraverser(sourceFilePath: String)(using ctx: Context)(using SDBSymbolNameBuilder) extends TreeTraverser:
  val sdbStringBuilder = new SDBSymbolNameBuilder()
  val extractor = TastyPositionExtractor(sourceFilePath)
  val synth = TastySyntheticsExtractor()

  
  private def registerSymbol(sym: Symbol, symkinds: Set[SymbolKind])(using Context, SDBSymbolNameBuilder): Unit =
    val sname = sym.SDBname
    val isLocal = sname.isLocal
    if !isLocal || !localNames.contains(sname) then
        if isLocal then
          localNames += sname
        symbolInfos += sym.symbolInfo(symkinds)


  private def registerSymbolSimple(sym: Symbol)(using Context, SDBSymbolNameBuilder): Unit = {
      registerSymbol(sym, Set.empty)
    }

  private def registerDefinition(sym: Symbol, span: SourcePosition, symkinds: Set[SymbolKind], extractor: TastyPositionExtractor)(using Context, SDBSymbolNameBuilder) =
    val sname = sym.SDBname
    registerSymbol(sym, symkinds)

  private def registerOccurrence(symbol: Symbol, range: Option[dotty.tools.dotc.semanticdb.Range], role: SymbolOccurrence.Role)(using Context, SDBSymbolNameBuilder): Unit =
    val name = symbol.SDBname
    val occ = SymbolOccurrence(range, symbol.SDBname, role)
    if !generated.contains(occ) && occ.symbol.nonEmpty && !range.isEmpty then
      occurrences += occ
      generated += occ

  private def registerOccurrence(symbol: Symbol, span: SourcePosition, role: SymbolOccurrence.Role)(using Context, SDBSymbolNameBuilder): Unit =
    val range = if span.isUnknown then None else
      val result = extractor.extract(symbol.name.toString(), span, symbol)
      Some(result)
    registerOccurrence(symbol, range, role)
  
  private def registerOccurrence(symbol: Symbol, otherSymbol: Symbol, otherSymbolSpan: SourcePosition, role: SymbolOccurrence.Role)(using Context, SDBSymbolNameBuilder): Unit =
    val range = if otherSymbolSpan.isUnknown then None else
      val result = extractor.extract(otherSymbol.name.toString(), otherSymbolSpan, otherSymbol)
      Some(result)
    
    registerOccurrence(symbol, range, role)

  private def registerOccurrence(symbol: Symbol, symbolName: String, span: SourcePosition, role: SymbolOccurrence.Role)(using Context, SDBSymbolNameBuilder): Unit =
    val range = if span.isUnknown then None else
      val result = extractor.extract(symbolName, span, symbol)
      Some(result)
    
    val occ = SymbolOccurrence(range, symbolName, role)
    if !generated.contains(occ) && occ.symbol.nonEmpty && !range.isEmpty then
      occurrences += occ
      generated += occ

  val synthetics = new ListBuffer[Synthetic]()
  val symbolInfos = new ListBuffer[SymbolInformation]()
  val occurrences = new ListBuffer[SymbolOccurrence]()
  val localNames = new mutable.HashSet[String]()
  private val generated = new mutable.HashSet[SymbolOccurrence]
  override def traverse(tree: Tree): Unit =

    val pos = tree.pos
    tree match
      case namedTree: (DefTree) =>
        namedTree match
          case defDef: DefDef =>
            
            defDef.paramLists.foreach{namedTree =>
              namedTree match
                case Left(listValDef) =>
                  listValDef.foreach(namedTree =>
                    registerDefinition(namedTree.symbol, namedTree.pos, symbolKinds(namedTree), extractor)
                    if (!defDef.symbol.isConstructor){
                      if !namedTree.pos.isZeroExtent then
                        registerOccurrence(namedTree.symbol, namedTree.pos, dotty.tools.dotc.semanticdb.SymbolOccurrence.Role.DEFINITION)
                    }
                    super.traverse(namedTree)
                  )
                case Right(listTypeParam) => 
                  listTypeParam.foreach(namedTree =>

                    
                    if (!defDef.symbol.isConstructor){
                      registerSymbolSimple(namedTree.symbol)(using ctx, sdbStringBuilder)
                      if !namedTree.pos.isZeroExtent then
                        registerOccurrence(namedTree.symbol, namedTree.pos, dotty.tools.dotc.semanticdb.SymbolOccurrence.Role.DEFINITION)
                    }
                    super.traverse(namedTree)
                  )
                }
            traverse(defDef.resultTpt)
            defDef.rhs match
              case None =>
              case Some(value) => traverse(value)
            registerDefinition(namedTree.symbol, pos, symbolKinds(namedTree), extractor)
            if (defDef.symbol.isConstructor) then
              registerOccurrence(namedTree.symbol, pos, dotty.tools.dotc.semanticdb.SymbolOccurrence.Role.DEFINITION)
            else if !defDef.pos.isZeroExtent then
              defDef.rhs match
                case None => registerOccurrence(namedTree.symbol, pos, dotty.tools.dotc.semanticdb.SymbolOccurrence.Role.DEFINITION)
                case Some(value) => 
                  if value.pos.startColumn != defDef.pos.startColumn then 
                    registerOccurrence(namedTree.symbol, pos, dotty.tools.dotc.semanticdb.SymbolOccurrence.Role.DEFINITION)
              
          case c: ClassDef =>
            def extractOwner(s: Symbol) = 
              s match
                case packageSymbol: PackageSymbol =>
                  val packageRef = packageSymbol.packageTree match
                    case None => 
                    case Some(value) =>
                      if (!value.pos.isZeroExtent && !packageSymbol.isEmptyPackage){
                          registerOccurrence(packageSymbol, value.pos, dotty.tools.dotc.semanticdb.SymbolOccurrence.Role.DEFINITION)
                      }
                  
                case _ =>
            extractOwner(c.symbol.owner)
            registerDefinition(namedTree.symbol, pos, symbolKinds(namedTree), extractor)
            if !pos.isZeroExtent then  
              registerOccurrence(namedTree.symbol, pos, dotty.tools.dotc.semanticdb.SymbolOccurrence.Role.DEFINITION)
            super.traverse(tree)
              
          case _ =>
            registerDefinition(namedTree.symbol, pos, symbolKinds(namedTree), extractor)
            if !pos.isZeroExtent then  
              registerOccurrence(namedTree.symbol, pos, dotty.tools.dotc.semanticdb.SymbolOccurrence.Role.DEFINITION)
            super.traverse(tree)


      case typeIdent: TypeIdent =>
          typeIdent.toType match
            case typeRef: TypeRef =>
              val sym = typeRef.optSymbol.get
              val owner = sym.owner
              val ownerOwner = sym.owner.owner
              sym.owner match
                case termSymbol : TermSymbol =>
                  termSymbol.owner match
                    case classSymbol : ClassSymbol if termSymbol.isConstructor =>
                      val newName = classSymbol.SDBname + sym.name.toString.unescapeUnicode
                      registerOccurrence(classSymbol, newName, pos, dotty.tools.dotc.semanticdb.SymbolOccurrence.Role.REFERENCE)
                    case _ =>
                case _ =>
                  registerOccurrence(sym, pos, dotty.tools.dotc.semanticdb.SymbolOccurrence.Role.REFERENCE)
            case _ =>
              
          super.traverse(tree)
          
      case assign : Assign =>
        assign.lhs match
          case select : Select => 
            val setterName = select.symbol.name.toTermName match
              case simpleName : SimpleName => simpleName.append(str.SETTER_SUFFIX)
              case _ => select.symbol.name.toTermName
            
            val found = select.symbol.owner match
              case classSymbol: ClassSymbol =>
                classSymbol.declarations.find(_.name == setterName).getOrElse(select.symbol)
              case _ => select.symbol

            registerOccurrence(found, select.symbol, select.pos, dotty.tools.dotc.semanticdb.SymbolOccurrence.Role.REFERENCE)
            super.traverse(assign.rhs)
          case _ => super.traverse(tree)
        
        
      case ident: Ident=> 
        registerOccurrence(ident.symbol, ident.pos, dotty.tools.dotc.semanticdb.SymbolOccurrence.Role.REFERENCE)
        super.traverse(tree)
      case select : Select =>
        val qualPos = select.qualifier.pos
        val selectPos = select.pos
        if(!qualPos.isUnknown && !selectPos.isUnknown && qualPos.endOffset != selectPos.endOffset){
          registerOccurrence(select.symbol, select.pos, dotty.tools.dotc.semanticdb.SymbolOccurrence.Role.REFERENCE)
        }
        super.traverse(tree)
      case typeApply: TypeApply =>
        synth.tryFindSynthetic(typeApply).foreach(synthetics.addOne)
        super.traverse(typeApply)

      case apply: Apply =>

        apply.fun match
          case ident: Ident =>
            ident.symbol match
              case termSymbol: TermSymbol => 
                registerOccurrence(termSymbol, ident.pos, dotty.tools.dotc.semanticdb.SymbolOccurrence.Role.REFERENCE)
              case _ =>
            super.traverse(tree)
          case _ => super.traverse(tree)
      case _ =>
          super.traverse(tree)
  
  extension (list: List[Either[List[ValDef], List[TypeParam]]])
    private  inline def isSingleArg = list match
      case _::Nil => true
      case _             => false

  extension (tree: DefDef)
    private def isSetterDef(using Context): Boolean =
      tree.name.isSetterName && tree.paramLists.isSingleArg

  private def symbolKinds(tree: Tree)(using Context): Set[SymbolKind] =
      val symkinds = scala.collection.mutable.HashSet.empty[SymbolKind]
      tree match
        case tree: ValDef =>
          if !tree.symbol.isParam then
            tree.symbol.kind match
              case TermSymbolKind.Val => symkinds += SymbolKind.Val
              case TermSymbolKind.LazyVal => symkinds += SymbolKind.Val
              case TermSymbolKind.Var => symkinds += SymbolKind.Var
              case _ =>
          if tree.rhs.isEmpty && !(tree.symbol.isParamAccessor || tree.symbol.isCaseClassAccessor|| tree.symbol.isParam) then
            symkinds += SymbolKind.Abstract
        case defTree: DefDef =>
          if defTree.rhs.isEmpty then
            symkinds += SymbolKind.Abstract
          if defTree.isSetterDef then
            symkinds += SymbolKind.Setter

        case tree: Bind if (!tree.symbol.isType) =>
          symkinds += SymbolKind.Val
        case tree: Bind if (tree.symbol.isType) =>
          symkinds += SymbolKind.TypeVal
        case _ =>

      symkinds.toSet

class TastyExtractSemanticDB(entry: ClasspathEntry, cp: Classpath, ctx: dotty.tools.dotc.core.Contexts.Context) {
  val ctx3 = tastyquery.Contexts.Context.initialize(cp)
  val symbolList = ctx3.findSymbolsByClasspathEntry(entry).toList
  val originFileList = symbolList.flatMap { x =>
    x.tree.map(tree => (tree.pos.sourceFile, x))
  }.groupMap(_._1)(_._2).toList

  def writeSemanticDB(unitContexts :List[Contexts.Context]): Unit = {
    val sourceRoot = ctx.settings.sourceroot.value(using ctx)
    for ((dottyCtx, i) <- unitContexts.zipWithIndex) do {
      val unit = dottyCtx.compilationUnit
      val outputDir =
        ExtractSemanticDB.semanticdbPath(
          unit.source,
          ExtractSemanticDB.semanticdbOutDir(using dottyCtx),
          sourceRoot
        )
      val sdbStringBuilder = new SDBSymbolNameBuilder()
      val TreeTraverser = new CustomTreeTraverser(unit.source.toString)(using ctx3)(using sdbStringBuilder)
      
      val found = originFileList.filter( (sourceFile, _) =>
        sourceFile.name == unit.source.file.name).head._2
      found.map(x=> x.tree match
          case Some(tree) =>
            TreeTraverser.traverse(tree)
          case None => )
      val definitionList = TreeTraverser.symbolInfos.toList
      val occurrences = TreeTraverser.occurrences.toList
      val synthetics = TreeTraverser.synthetics.toList
      semanticdb.ExtractSemanticDB.write(
        unit.source,
        occurrences,
        definitionList,
        synthetics,
        outputDir,
        sourceRoot,
        false
      )
    }
  }
}