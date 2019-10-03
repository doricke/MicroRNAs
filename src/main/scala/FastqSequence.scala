

// ****************************************************************************
class FastqSequence extends FastaSequence {

  // **************************************************************************
  var quality: String = ""	// sequence quality string
  var qualityValues: Array[Int] = null		// sequence quality values

  // **************************************************************************
  def parseQuality: Array[Int] = {
    qualityValues = new Array[Int]( quality.length )
    for ( i <- 0 until quality.length )
      qualityValues( i ) = quality.toInt - 33 
    qualityValues
  }  // parseQuality

  // **************************************************************************
  override def to_string(): String = {
    "@" + name + " " + description + "\n" + toBlock + "+\n" + toBlock( quality )
  }  // to_string

  // **************************************************************************
}  // class FastqSequence
