import _root_.scala.Predef.{wrapRefArray  => _}

object Test {
  /*
  import _root_.scala.collection.mutable.{ArrayOps, ArrayLike, WrappedArray, ArrayBuilder}
  import scala.reflect.ClassManifest


  final class ofRef[T <: AnyRef](override val repr: Array[T]) extends ArrayOps[T] with ArrayLike[T, Array[T]] {

    override protected[this] def thisCollection: WrappedArray[T] = new WrappedArray.ofRef[T](repr)
    override protected[this] def toCollection(repr: Array[T]): WrappedArray[T] = new WrappedArray.ofRef[T](repr)
    override protected[this] def newBuilder = new ArrayBuilder.ofRef[T]()(
      ClassManifest.classType[T](repr.getClass.getComponentType))

    def length: Int = repr.length
    def apply(index: Int): T = repr(index)
    def update(index: Int, elem: T) { repr(index) = elem }
  }

  implicit def refArrayOps[T <: AnyRef](xs: Array[T]) = new ofRef[T](xs)
   
  def main(args: Array[String]) {
    if(args.equals("wefwe"))
      refArrayOps(args) foreach println
    else
      println("only stuff")
  }*/

  import MyRanges._
  def whily(i: Int) {
    if (i>0) println("hello") else println("wursti")
    for (x <- 0 to2 10)
      println(x)
  }
}
