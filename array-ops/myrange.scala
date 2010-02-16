
object MyRanges {
  import scala.collection.immutable.Range
  trait RangeBuilding {
    def to2(end: Int): Range
  }
  implicit def rangeBuilding(ostart: Int) = new RangeBuilding {
    def to2(oend: Int): Range = new Range.Inclusive(ostart, oend, 1){
	  override def foreach[U](f: Int => U) {
	    var i = ostart
	    while (i < oend+1) {
	      f(i)
	      i += 1
	    }
	  }
    }
  }
}
