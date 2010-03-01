package net.virtualvoid.scala.tools

object TelnetRepl {
  import _root_.scala.tools.nsc
  import nsc._
  import nsc.interpreter._
  import jline.ConsoleReader
  import _root_.java.io.{OutputStream, PrintWriter}

  def defaultInteractiveReader(interpreter: Interpreter, intLoop: InterpreterLoop, jlineReader: ConsoleReader) =
    InteractiveReader.createDefault(interpreter)

  def repl(jlineReader: ConsoleReader, os: OutputStream, classpath: String)
          (reader: (Interpreter, InterpreterLoop, ConsoleReader) => InteractiveReader = defaultInteractiveReader) {
    val intLoop = new InterpreterLoop(None, new PrintWriter(os))
    intLoop.settings = new Settings(Console.println)
    intLoop.settings.classpath.value = classpath
    intLoop.createInterpreter
    intLoop.in = reader(intLoop.interpreter, intLoop, jlineReader)
    
    intLoop.repl()
    intLoop.closeInterpreter
  }

  def interactiveReader(interpreter: Interpreter, intLoop: InterpreterLoop, jlineReader: ConsoleReader) =
    new InteractiveReader {
        import jline._
        val consoleReader = {
          val history = 
            try new History(new java.io.File(System.getProperty("user.home"), ".scala_history"))
            // do not store history if error
            catch { case _: Exception => new History() }

          val r = jlineReader
          r setHistory history
          r setBellEnabled false
    
          if (interpreter != null) {
            // have to specify all delimiters for completion to work nicely
            val delims = new ArgumentCompletor.AbstractArgumentDelimiter {
              val delimChars = "(){}[],`;'\" \t".toArray
              def isDelimiterChar(s: String, pos: Int) = delimChars contains s.charAt(pos)
            }
            val comp = new ArgumentCompletor(new Completion(interpreter, intLoop), delims)
            comp setStrict false
            r addCompletor comp
            // XXX make this use a setting
            r setAutoprintThreshhold 250
          }

          r
        }
  
      def readOneLine(prompt: String) = consoleReader readLine prompt
      val interactive = true
    }

  def replFromSocket(socket: java.net.Socket, classpath: String){
    val (jlinereader, os) = JLineTelnet.readerFromSocket(socket)
    repl(jlinereader, os, classpath)(interactiveReader)
  }

  def main(args: Array[String]) {
    val server = new java.net.ServerSocket(12124)
    val client = server.accept
    replFromSocket(client, "lib/scala-library.jar:lib/scala-compiler.jar")
    client.close
    server.close
  }
}
