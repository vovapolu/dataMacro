package dataMacro

import scala.meta._
import scala.collection.immutable.Seq

object DataMacro {
  private[dataMacro] case class DataParam(name: String, tpe: Type, default: Option[Term])

  private[dataMacro] def validateCtor(ctor: Ctor.Primary) {
    if (ctor.paramss.length > 1) {
      abort("Current implementation doesn't support curried class definitions")
    }

    if (ctor.paramss.flatten.exists(param => param.mods.exists(_ != Mod.ValParam()))) {
      abort("Invalid constructor!")
    }
  }

  private[dataMacro] def extractDataParams(ctor: Ctor.Primary): Seq[DataParam] = {
    validateCtor(ctor)
    ctor.paramss.flatten.map {
      case Term.Param(_, name, Some(tpe: Type), default) => DataParam(name.value, tpe, default)
      case _ => abort("Invalid constructor!")
    }
  }

  private[dataMacro] def buildCtorParams(dataParams: Seq[DataParam]): Seq[Term.Param] = {
    dataParams.map(
      param => Term.Param(Seq(Mod.ValParam()), Term.Name(param.name), Some(param.tpe), param.default))
  }

  private[dataMacro] def buildApply(name: Type.Name, dataParams: Seq[DataParam], intern: Boolean): Defn.Def = {
    val args = dataParams.map(param => Term.Name(param.name))
    val params = dataParams.map(param => param"${Term.Name(param.name)}: ${param.tpe}")
    if (intern) {
      q"""def apply(..$params): $name = {
          val newVal = new ${Ctor.Ref.Name(name.value)}(..$args)
          ${Term.Name(name.value)}.intern(newVal)
        }"""
    } else {
      q"""def apply(..$params): $name =
        new ${Ctor.Ref.Name(name.value)}(..$args)"""
    }
  }

  private[dataMacro] def buildUnapply(name: Type.Name, dataParams: Seq[DataParam]): Defn.Def = {
    val clFields = dataParams.map(param => q"that.${Term.Name(param.name)}")
    val tupleTypes = if (dataParams.length > 1) {
      t"(..${dataParams.map(_.tpe)})"
    } else {
      t"${dataParams.head.tpe}"
    }
    val tupleArgs = if (clFields.length > 1) {
      q"(..$clFields)"
    } else {
      clFields.head
    }
    q"def unapply(that: $name): Option[$tupleTypes] = Some($tupleArgs)"
  }

  private[dataMacro] def buildEquals(name: Type.Name, dataParams: Seq[DataParam], intern: Boolean): Defn.Def = {
    val eqs = dataParams.map(param => q"that.${Term.Name(param.name)} == this.${Term.Name(param.name)}")
    val eqsWithAnds = eqs match {
      case Seq(eq1) => eq1
      case Seq(eq1, rest @ _ *) => rest.foldLeft(eq1)((acc, eq) => q"$acc && $eq")
    }

    if (intern) {
      q"override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object])"
    } else {
      q"""override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object]) ||
      (thatAny match {
        case that: $name => $eqsWithAnds
        case _ => false
     })"""
    }
  }

  private[dataMacro] def buildProductMethods(name: Type.Name, dataParams: Seq[DataParam]): Seq[Defn.Def] = {
    val casesForElements = dataParams.zipWithIndex.map {
      case (param, i) => p"case ${Lit(i)} => this.${Term.Name(param.name)}"
    }
    Seq(
      q"def canEqual(that: Any): Boolean = that.isInstanceOf[$name]",
      q"def productArity: Int = ${Lit(dataParams.length)}",
      q"""def productElement(n: Int): Any =
         n match {
            ..case $casesForElements
            case _ => throw new IndexOutOfBoundsException(n.toString())
         }
       """,
      q"override def productPrefix: String = ${Lit(name.value)}",
      q"override def productIterator: Iterator[Any] = scala.runtime.ScalaRunTime.typedProductIterator[Any](this)"
    )
  }

  private[dataMacro] def buildHashCode(name: Type.Name, dataParams: Seq[DataParam]): Defn.Def = {
    q"override def hashCode(): Int = scala.runtime.ScalaRunTime._hashCode(this)"
  }

  private[dataMacro] def buildToString(name: Type.Name, dataParams: Seq[DataParam]): Defn.Def = {
    q"override def toString(): String = scala.runtime.ScalaRunTime._toString(this)"
  }

  private[dataMacro] def buildCopy(name: Type.Name, dataParams: Seq[DataParam]): Defn.Def = {
    val copyParams = dataParams.map(
      param => Term.Param(Seq(), Term.Name(param.name), Some(param.tpe), Some(q"this.${Term.Name(param.name)}")))
    val copyArgs = dataParams.map(param => Term.Name(param.name))
    q"def copy(..$copyParams): $name = new ${Ctor.Ref.Name(name.value)}(..$copyArgs)"
  }

  private[dataMacro] def buildInternMap(name: Type.Name, dataParams: Seq[DataParam]): Seq[Stat] = {
    val wrapperName = Type.fresh(name.value + "Wrapper")
    val eqs = dataParams.map(param => q"that.cl.${Term.Name(param.name)} == this.cl.${Term.Name(param.name)}")
    val eqsWithAnds = eqs match {
      case Seq(eq1) => eq1
      case Seq(eq1, rest @ _ *) => rest.foldLeft(eq1)((acc, eq) => q"$acc && $eq")
    }
    Seq(
      q"""private[$name] class $wrapperName (val cl: $name) {
          override def hashCode(): Int = cl.hashCode
          override def equals(thatAny: Any): Boolean = (this eq thatAny.asInstanceOf[Object]) ||
            (thatAny match {
              case that: $wrapperName => $eqsWithAnds
              case _ => false
            })
       }""",
      q"""private[$name] val internMap: java.util.concurrent.ConcurrentHashMap[$wrapperName, $name] =
         new java.util.concurrent.ConcurrentHashMap()""",
      q"""def intern(cl: $name): $name = {
          val res = internMap.putIfAbsent(new ${Ctor.Ref.Name(wrapperName.value)}(cl), cl)
          if (res == null) cl else res
      }"""
    )
  }

  private[dataMacro] def buildIntern(name: Type.Name): Defn.Def = {
    q"def intern: $name = ${Term.Name(name.value)}.intern(this)"
  }
}

class data(intern: Boolean) extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    println(this.structure)
    val intern = this match {
      case q"new data(intern = ${Lit(b: Boolean)})" => b
      case _ => false
    }
    Term.New

    defn match {
      case Term.Block(
        Seq(cls@Defn.Class(_, name, _, ctor, _),
        companion: Defn.Object)
      ) =>
        abort("@data block")
      case cls@Defn.Class(_, name, _, ctor, _) =>
        val dataParams = DataMacro.extractDataParams(ctor)
        val newClass: Stat = q"""class ${Type.Name(name.value)} (..${DataMacro.buildCtorParams(dataParams)})
                         extends Product with Serializable {
          ${DataMacro.buildEquals(name, dataParams, intern)}
          ${DataMacro.buildToString(name, dataParams)}
          ${DataMacro.buildHashCode(name, dataParams)}
          ${DataMacro.buildCopy(name, dataParams)}
          ..${DataMacro.buildProductMethods(name, dataParams)}
          ..${if (intern) Seq(DataMacro.buildIntern(name)) else Seq()}
        }"""
        val newObject: Stat = q"""object ${Term.Name(name.value)} {
          ${DataMacro.buildApply(name, dataParams, intern)}
          ${DataMacro.buildUnapply(name, dataParams)}
          ..${if (intern) DataMacro.buildInternMap(name, dataParams) else Seq()}
        }"""

        println((newClass, newObject).toString())

        Term.Block(Seq(
          newClass,
          newObject
        ))
      case _ =>
        abort("@data must annotate a class.")
    }
  }
}
