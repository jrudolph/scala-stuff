package net.virtualvoid.scala.tools

object ClassPath {
  def asFile(url:java.net.URL):java.io.File =
      new java.io.File(url.toURI)

  def findManifest(file: java.io.File): Option[java.util.jar.Manifest] =
    try {
      if (file.isDirectory) {
        val mf = new java.io.File(file, "META-INF/MANIFEST.MF")
        if (mf.exists)
          Some(new java.util.jar.Manifest(new java.io.FileInputStream(mf)))
        else
          None
      }
      else
        Option(new java.util.jar.JarFile(file).getManifest)
    } catch {
      case e: Exception => println(file+" couldn't be opened: "+e.getMessage); None
    }

  def extractClassPath(jarfile: java.io.File): Option[String] =
    findManifest(jarfile).flatMap(mf => Option(mf.getMainAttributes.getValue("Class-Path")))

  def inflateClassPath(jarfile: java.io.File): Seq[java.io.File] =
    jarfile +: extractClassPath(jarfile).map(_.split("\\s+").map(new java.io.File(_))).getOrElse(Array())

  def detectClasspath(): String = getClass.getClassLoader.asInstanceOf[java.net.URLClassLoader].getURLs flatMap(url => inflateClassPath(asFile(url))) map (_.getAbsolutePath) mkString(java.io.File.pathSeparator)
}
