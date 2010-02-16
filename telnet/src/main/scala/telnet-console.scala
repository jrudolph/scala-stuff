object Telnet {
  import _root_.scala.tools.nsc
  import nsc._
  import nsc.interpreter._

  def repl(reader: Interpreter => InteractiveReader = InteractiveReader.createDefault(_)) {
    val intLoop = new InterpreterLoop
    intLoop.settings = new Settings(Console.println)
    intLoop.settings.classpath.value = "lib/scala-library.jar:lib/scala-compiler.jar"
    intLoop.createInterpreter
    intLoop.in = reader(intLoop.interpreter)
    
    intLoop.repl()
    intLoop.closeInterpreter
  }

  def main(args: Array[String]) {
    repl()
  }
}
