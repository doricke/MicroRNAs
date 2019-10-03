
////////////////////////////////////////////////////////////////////////////////
//
//  Author:     Darrell O. Ricke, Ph.D.  (mailto: Darrell.Ricke@ll.mit.edu)
//  Copyright:  Copyright (c) 2013-2019
//  License:    GNU GPL license 2 (http://www.gnu.org/licenses/gpl.html)
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
////////////////////////////////////////////////////////////////////////////////
// DISTRIBUTION STATEMENT A. Approved for public release: distribution unlimited.
//
// This material is based upon work supported under Air Force Contract No.
// FA8721-05-C-0002. Any opinions, findings, conclusions or recommendations
// expressed in this material are those of the author(s) and do not necessarily
// reflect the views of the U.S. Air Force.
//
// © 2013-2019 Massachusetts Institute of Technology.
//
// The software/firmware is provided to you on an As-Is basis
//
// Delivered to the U.S. Government with Unlimited Rights, as defined in DFARS
// Part 252.227-7013 or 7014 (Feb 2014). Notwithstanding any copyright notice,
// U.S. Government rights in this work are defined by DFARS 252.227-7013 or
// DFARS 252.227-7014 as detailed above. Use of this work other than as
// specifically authorized by the U.S. Government may violate any copyrights
// that exist in this work.
////////////////////////////////////////////////////////////////////////////////

import scala.collection.mutable.{Map, Set}

// ****************************************************************************
class WordsSets( targets: Map[String, String], wordSize: Int, mismatches: Int ) {

  private[this] val seqTools = new SeqTools()

  val targetsSeqs = Map[String, String]()
  val targetsWords = Map[String, Set[String]]()
  targets foreach {case (name, seq) =>
    targetsSeqs  += (name -> seq)
    targetsWords += (name -> seqTools.seqSplit( seq, wordSize ) ) 
  }  // foreach

  // **************************************************************************
  def find( sequence: Sequence ): Tuple2[String, Int] = {
    var bestForward = lookup( sequence.seqForward, sequence.forwardWords )
    var bestReverse = lookup( sequence.seqReverse, sequence.reverseWords )
    if ( bestForward._2 > bestReverse._2 ) return bestForward 
    else return bestReverse
  }  // find

  // **************************************************************************
  def lookup( querySeq: String, seqSet: Set[String] ): Tuple2[String, Int] = {
    var bestCount = 0
    var bestName = ""
    var bestSet = Set[String]()
    targetsWords foreach {case (name, microSet) =>
      val commonSet = seqSet & microSet
      val count = commonSet.size
      if ( count > bestCount ) { bestCount = count; bestName = name; bestSet = commonSet }
   }  // foreach

   // Validate the best match
   if ( bestName.length > 0 ) {
     val micro = targetsSeqs( bestName )
     var align = micro.toArray
     bestSet foreach {case (word) =>
       val index = micro.indexOf( word )
       for (i <- 0 until wordSize) {align(index+i) = '|'}
     }  // foreach

     var count = 0
     align foreach { case (base) => if (base == '|') count += 1}
     // println( "lookup: count: " + count + ", micro.length: " + micro.length + ", mismatches: " + mismatches + ", align: " + align.mkString )
     bestCount = count
     // if ( ( count < micro.length - mismatches ) || ( querySeq.length + 3 < micro.length ) ) {
     if (( count < micro.length - mismatches ) && (count < 16)) {
       bestName = ""
       bestCount = 0
     }  // if
     // else
       // println( " >> " + micro + " vs. " + align.mkString )
   }  // if

   return (bestName, bestCount)
  }  // lookup

  // **************************************************************************
}  // class WordsSets
