package dotty.tools.dotc

import tastyquery.Contexts.Context
import tastyquery.Types.*
import scala.collection.mutable
import tastyquery.Names.*
import tastyquery.Symbols.TermSymbol
import tastyquery.Symbols.ClassTypeParamSymbol
import tastyquery.Symbols.LocalTypeParamSymbol
import tastyquery.Symbols.TypeMemberSymbol
import tastyquery.Symbols.ClassSymbol
import tastyquery.Symbols.PackageSymbol
import dotty.tools.dotc.semanticdb.Scala3.SemanticSymbol
import dotty.tools.dotc.SDBSymbolNameBuilder
import tastyquery.Constants.*
import tastyquery.Annotations.Annotation
import dotty.tools.dotc.core.SymDenotations.NoDenotation.matches
import dotty.tools.dotc.core.Symbols.TypeSymbol

import dotty.tools.dotc.semanticdb.Scala3.*
import dotty.tools.dotc.semanticdb.LinkMode
import dotty.tools.dotc.semanticdb.Scala3.TastyFakeSymbol
import dotty.tools.dotc.SymbolScopeOps.sscopeOpt
import tastyquery.Modifiers.Variance
import dotty.tools.dotc.SymbolScopeOps.sscope
import dotty.tools.dotc.core.TypeComparer.andType
import tastyquery.Symbols.TermOrTypeSymbol
import dotty.tools.dotc.Extensions.*

object SDBtype:
  
  private val paramRefSymtab = mutable.Map[(LambdaType, Name), TastySemanticSymbol]()
  private val refinementSymtab = mutable.Map[(RefinedType, Name), tastyquery.Symbols.TermOrTypeSymbol]()
  val fakeSymbols = mutable.Set[TastyFakeSymbol]()

  extension (const: Constant)
    def toSemanticConstTasty(using Context): semanticdb.Constant = const.tag match {
      case UnitTag => semanticdb.UnitConstant()
      case BooleanTag => semanticdb.BooleanConstant(const.booleanValue)
      case ByteTag => semanticdb.ByteConstant(const.byteValue)
      case ShortTag => semanticdb.ShortConstant(const.shortValue)
      case CharTag => semanticdb.CharConstant(const.charValue)
      case IntTag => semanticdb.IntConstant(const.intValue)
      case LongTag => semanticdb.LongConstant(const.longValue)
      case FloatTag => semanticdb.FloatConstant(const.floatValue)
      case DoubleTag => semanticdb.DoubleConstant(const.doubleValue)
      case StringTag => semanticdb.StringConstant(const.stringValue)
      case NullTag => semanticdb.NullConstant()
      case _ => throw new Error(s"Constant ${const} can't be converted to Semanticdb Constant.")
    }
  private def registerFakeSymbol(sym: TastyFakeSymbol)(using Context): Unit =
    fakeSymbols.add(sym)

  extension (tpe: TermType)
    def toSemanticSig(using LinkMode, Context, SDBSymbolNameBuilder)(sym: tastyquery.Symbols.TermOrTypeSymbol): semanticdb.Signature = {
      def loop(tpe: TermType): semanticdb.Signature = tpe match{
        
        case mp: MethodicType =>
          def flatten(
            t: TermType,
            paramss: List[List[TastySemanticSymbol]],
            tparams: List[TastySemanticSymbol]
          ): (TermType, List[List[TastySemanticSymbol]], List[TastySemanticSymbol]) = t match {
            case mt: MethodType =>
              val syms: List[TastySemanticSymbol] = mt.paramNames.zip(mt.paramInfos).map {
                (name, info) => 
                  paramRefSymtab.get(mt, name).getOrElse{
                    registerFakeSymbol(TastyTermParamRefSymbol(sym, name, info))
                    TastyTermParamRefSymbol(sym, name, info)
                }
              }
              flatten(mt.resultType, paramss :+ syms, tparams)
            
            case pt: PolyType =>
              val syms: List[TastySemanticSymbol] = pt.paramNames.zip(pt.paramInfos).map {
                (name, info) => 
                  paramRefSymtab.get(pt, name).getOrElse{
                    if sym.isConstructor then 
                        registerFakeSymbol(TastyTypeParamRefSymbol(sym.owner, name, info))
                        TastyTypeParamRefSymbol(sym.owner, name, info)
                    else
                      registerFakeSymbol(TastyTypeParamRefSymbol(sym, name, info))
                      TastyTypeParamRefSymbol(sym, name, info)
                  }

              }
              flatten(pt.resultType, paramss, tparams ++ syms)
            case other =>
              (other, paramss, tparams)
          }
          val (resType, paramss, tparams) = flatten(mp, Nil, Nil)
          val sparamss  = paramss.map(x => {x.sscope})
          val stparams = tparams.sscopeOpt 

          val toReturn = semanticdb.MethodSignature(
            stparams,
            sparamss,
            resType.toSemanticType(sym.SDBname, Some(sym)))
          toReturn
        //as equivalent of ExprType do not have a MethodType, used to catch a ExprType
        case _ if sym.isMethod => 
          semanticdb.ValueSignature(semanticdb.ByNameType(toSemanticType(sym.SDBname, Some(sym))))

        case other => semanticdb.ValueSignature(toSemanticType(sym.SDBname, Some(sym)))
      }
      loop(tpe)
    }
        
    def toSemanticType(using Context, SDBSymbolNameBuilder)(sym: String, symSymbol : Option[TermOrTypeSymbol]): semanticdb.Type =
      val ctx = summon[Context]
      import semanticdb.ConstantOps.*
      def loop(tpe: TermType): semanticdb.Type = tpe match {
        case typeRef: TypeRef => 
          val spre = if tpe.hasTrivialPrefix then semanticdb.Type.Empty else {
            typeRef.prefix match
              case NoPrefix => semanticdb.Type.Empty
              case pre: PackageRef => semanticdb.Type.Empty
              case pre: Type => loop(pre)
          }
          val ssym = typeRef.optSymbol match
            case None => "Error no Symbol"
            case Some(value) =>  
                value.SDBname
          semanticdb.TypeRef(spre, ssym, Seq.empty)

        case termRef: TermRef =>
            val spre = if tpe.hasTrivialPrefix then semanticdb.Type.Empty else {
              termRef.prefix match
                case NoPrefix => semanticdb.Type.Empty
                case pre:PackageRef => loop(pre)
                case pre: Type => loop(pre)
            }
            val ssym = termRef.optSymbol match
              case None => "Error no Symbol"
              case Some(value) =>  
                  value.SDBname
            semanticdb.SingleType(spre, ssym)

        case thisType: ThisType =>
            semanticdb.ThisType(sym)

        case tref: TermParamRef =>
            semanticdb.SingleType(semanticdb.Type.Empty, sym)
        
        case tref: TypeParamRef =>
          semanticdb.TypeRef(semanticdb.Type.Empty, sym, Seq.empty)
        
        case superType: SuperType => 
          val spre = loop(superType.thistpe)
          val ssym = sym
          semanticdb.SuperType(spre, ssym)

        case constantType: ConstantType if constantType.value.tag == core.Constants.ClazzTag =>
          semanticdb.ConstantType(constantType.value.toSemanticConstTasty)
        
        case constantType: ConstantType =>
          semanticdb.ConstantType(constantType.value.toSemanticConstTasty)

        case matchType: MatchType =>
          val scases = matchType.cases.map{
            caseType => 
              caseType match {
                case caseType: MatchTypeCase =>
                  val skey = loop(caseType.pattern)
                  val sbody = loop(caseType.result)
                  semanticdb.MatchType.CaseType(skey, sbody) 
            }}

          val sscrutinee = loop(matchType.scrutinee)
          val sbound = loop(matchType.bound) 
          semanticdb.MatchType(sscrutinee, scases)

        case rec: RecType =>
          loop(rec.parent)
        case repeated: RepeatedType =>
          val stpe = loop(repeated.elemType) 
          semanticdb.RepeatedType(stpe)
        case app : AppliedType =>
          val andSym = ctx.defn.scalaPackage.findDecl(typeName("&"))
          val barSym = ctx.defn.scalaPackage.findDecl(typeName("|"))
          app.tycon match
            case tycon: TypeRef if tycon.optSymbol.contains(barSym) =>
              loop(OrType(app.args.head.highIfWildcard, app.args.tail.head.highIfWildcard))
            case tycon: TypeRef if tycon.optSymbol.contains(andSym) =>
              loop(AndType(app.args.head.highIfWildcard, app.args.tail.head.highIfWildcard))
            case _ => {
              val targs = app.args.map {arg =>
                arg match
                  case arg: WildcardTypeArg => arg.bounds match
                    case bounds:  AbstractTypeBounds => 
                      symSymbol match
                        case None => 
                          val sarg = loop(bounds.high)
                          (None, sarg)
                        case Some(value) =>
                      
                          val wildcardSym = TastyWildcardTypeSymbol(value, bounds)
                          val ssym = wildcardSym.SDBFakeSymName
                          (Some(wildcardSym), semanticdb.TypeRef(semanticdb.Type.Empty, ssym, Seq.empty))
                    case TypeAlias(alias) => 
                      val sarg = loop(alias)
                      (None, sarg)
                  case arg: Type =>  
                    val sarg = loop(arg)
                    (None, sarg)
              }
              
              val wildcardSyms = targs.flatMap(_._1)
              val sargs = targs.map(_._2)
              val applied = loop(app.tycon) match
                case ref @ semanticdb.TypeRef(_, _, targs) =>
                  ref.copy(typeArguments = targs ++ sargs)
                case _ =>
                  semanticdb.Type.Empty
              if (wildcardSyms.isEmpty) applied
              else semanticdb.ExistentialType(
                applied,
                wildcardSyms.sscopeOpt(using LinkMode.HardlinkChildren)
              )}
          
        case and: AndType =>
          def flatten(child: Type): List[Type] = child match
            case andType: AndType=> flatten(andType.first) ++ flatten(andType.second)
            case other => List(other)
          val stpes = flatten(and).map(loop)
          semanticdb.IntersectionType(stpes)

        case or: OrType =>
          def flatten(child: Type): List[Type] = child match
            case orType: OrType => flatten(orType.first) ++ flatten(orType.second)
            case other => List(other)
          val intermediate = flatten(or)
          val stpes = flatten(or).map(loop)
          semanticdb.UnionType(stpes)

        case lambda: TypeLambda =>
          val parameters = Some(semanticdb.Scope(Seq("")))
          val resType = loop(lambda.resultType)
          semanticdb.LambdaType(
            parameters,
            resType)
        
        case _ =>
          semanticdb.Type.Empty
      }
      loop(tpe)
      
          
      

    private def hasTrivialPrefix(using Context): Boolean =
      def checkTrivialPrefix(pre: Prefix, sym: tastyquery.Symbols.Symbol)(using Context): Boolean =
        pre match
          case NoPrefix => false
          case pre : PackageRef => pre.symbol == sym.owner
          case pre : Type => 
            sym.owner match
              case owner: ClassSymbol => pre.isSameType(owner.thisType)
              case _                  => false
        
      tpe match {
        case typeRef : TypeRef =>
          typeRef.optSymbol match
            case None => false
            case Some(value) => checkTrivialPrefix(typeRef.prefix, value)
        case _ => false
        }

            
object SymbolScopeOps:
  import dotty.tools.dotc.semanticdb.Scala3.{_, given}
  extension (syms: List[TastySemanticSymbol])
    def sscope(using linkMode: LinkMode)(using Context, SDBSymbolNameBuilder): semanticdb.Scope =
      linkMode match
        case LinkMode.SymlinkChildren => 
          semanticdb.Scope(symlinks = syms.map(
            x => {
              x.symbolName
            } 
              
            ))
        case LinkMode.HardlinkChildren =>
          semanticdb.Scope(Seq("Hardlink not implemented"))

    def sscopeOpt(using LinkMode, Context, SDBSymbolNameBuilder): Option[semanticdb.Scope] =
      if syms.nonEmpty then Some(syms.sscope) else None
