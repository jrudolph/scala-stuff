object TelnetRepl {
  import _root_.scala.tools.nsc
  import nsc._
  import nsc.interpreter._
  import jline.ConsoleReader
  import _root_.java.io.{OutputStream, PrintWriter}

  def defaultInteractiveReader(interpreter: Interpreter, intLoop: InterpreterLoop, jlineReader: ConsoleReader) =
    InteractiveReader.createDefault(interpreter)

  def osWithCR(os: OutputStream): OutputStream = new OutputStream {
    override def write(b: Int) = {
      if (b == '\n')
        os.write('\r')
      
      os.write(b)
    }
  }

  def repl(jlineReader: ConsoleReader, os: OutputStream)
          (reader: (Interpreter, InterpreterLoop, ConsoleReader) => InteractiveReader = defaultInteractiveReader) {
    val intLoop = new InterpreterLoop(None, new PrintWriter(osWithCR(os)))
    intLoop.settings = new Settings(Console.println)
    intLoop.settings.classpath.value = "lib/scala-library.jar:lib/scala-compiler.jar"
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

  def main(args: Array[String]) {
    System.setProperty("line.separator", "\r\n")
    val server = new java.net.ServerSocket(12123)
    val (jlinereader, os) = JLineTelnet.readerFromSocket(server.accept)
    repl(jlinereader, os)(interactiveReader)
    server.close
  }
}
