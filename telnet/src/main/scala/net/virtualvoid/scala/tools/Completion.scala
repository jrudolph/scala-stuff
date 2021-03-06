package net.virtualvoid.scala.tools

import jline._
import _root_.scala.tools.nsc
import nsc._

class SymTabCompletor(repl: Interpreter) {
  import repl.compiler
  import compiler._
  
  object Completion extends Completor {
    val Path = java.util.regex.Pattern.compile("""\w+(?:\.\w+)*\.?""")
    
    def isPath(str: String) = Path.matcher(str).matches
    
    def secure[T](f: => List[T]): List[T] =
      try {
        f
      } catch {
        case e: Throwable => 
          e.printStackTrace
          Nil
      }
      
    private def findChild(tpe: Type, ele: String, last: Boolean): List[Symbol] = {
      def withNameFilter(f: (String, String) => Boolean) = 
        tpe.members.filter(sym => !sym.name.toString.contains("$") && 
                                  f(sym.name.toString, ele) && 
                                  (sym.isStable || last))
      
      val exactMatches = withNameFilter(_==_)
      if (!exactMatches.isEmpty)
        exactMatches
      else
        withNameFilter(_.startsWith(_))
    }
    
    def findMatches(tpe: Type, pathEls: List[String], prefix: String): List[String] = {
      println(tpe)
      pathEls match {
        case ""::Nil      => tpe.members.map(prefix+_.name)
        case first::other => println(other==Nil)
          findChild(tpe, first, other == Nil).flatMap(tpe2 => secure { findMatches(tpe2.tpe, other, prefix+tpe2.name+".") })
        case Nil          => List(prefix)
      }
    }
    
    override def complete(buffer: String, cursor: Int, cands: java.util.List[String]):Int = {      
      println(buffer)
      if (!isPath(buffer))
          return cursor
      
      val els = buffer.split('.').toList
      val elements = if (buffer.endsWith(".")) els :+ "" else els
      println(elements)
      findMatches(definitions.RootClass.tpe, elements, "") foreach (cands.add(_))
      0
    }
  }
  def foldSymTab[T](start: T)(f: (T, Symbol) => T): T = {
    def innerFold(cur: T, sym: Symbol): T = {
      val next = f(cur, sym)
      try {
        sym.tpe.members.filter(sym => !sym.name.toString.contains("$")).foldLeft(next)(innerFold)
      } catch {
        case _ => next
      }     
    }
    innerFold(start, definitions.RootClass)
  }
  private def filter(f: Symbol => Boolean): List[Symbol] =
    foldSymTab[List[Symbol]](Nil)((l, sym) => if (f(sym)) sym::l else l)
}
