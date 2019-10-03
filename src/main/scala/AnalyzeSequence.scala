
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

import scala.collection.mutable.Map

// ****************************************************************************
class AnalyzeSequence( parameters: Parameters ) {

  private[this] val align = new Align()
  private[this] val seqTools = new SeqTools()

  // ***************************************************************************
  def analyzeSequence( seqF: String, mid: String ): MicroResult = {
    val sequence = new Sequence( seqF, parameters.wordSize )
    val result = new MicroResult( seqF )
    val seqStart: String = if ( seqF.length > parameters.barcodeWindow ) seqF.substring( 0, parameters.barcodeWindow ) else seqF
    // val seqR = seqTools.reverseCompliment( seqF )

    // Search for a barcode match.
    if ( mid.length > 0 )
      result.barcodesF = new Match( mid, mid, mid, mid.length, 0, '+' )
    else {
      val barcodes = findBest( sequence, parameters.barcodes, parameters.barcodesSet, parameters.mismatches, true )
      if ( barcodes.size > 0 )  result.barcodesF = barcodes.head
    }  // if

    // Search for a primer match.
    result.primers = findBest( sequence, parameters.primersF, parameters.primersFSet, parameters.mismatches, true )
    if ( result.primers.size == 0 )
      result.primers = findBest( sequence, parameters.primersR, parameters.primersRSet, parameters.mismatches, true )

    // Search for a miRNA match.
    val miRnas = findBest( sequence, parameters.microRnas, parameters.microsSet, parameters.mismatches, false )
    if ( miRnas.size > 0 ) {
      result.miRNA = miRnas.head
      // result.miRNA.target_seq = parameters.microRnas( result.microName )
    }  // if
    
    // println( "Result: " + result.to_string )

    result
  }  // analyzeSequence 

  // ***************************************************************************
  def findBest( sequence: Sequence, targetMap: Map[String, String], targetWords: WordsSets, mismatches: Int, submatch: Boolean ): collection.mutable.Set[Match] = {
    val found = collection.mutable.Set[Match]()
    targetMap foreach {case (name, targetSeq) =>
      if ( sequence.seqForward contains targetSeq) {
        found += new Match( name, targetSeq, targetSeq, targetSeq.size, 0, '+' )
      }   
      else if ( sequence.seqReverse contains targetSeq ) {
        found += new Match( name, targetSeq, targetSeq, targetSeq.size, 0, '-' )
      }
      else if ( (submatch || (sequence.seqForward.size >= 16)) && ( targetSeq contains sequence.seqForward ) )  {
        found += new Match( name, targetSeq, sequence.seqForward, sequence.seqForward.size, (targetSeq.size - sequence.seqForward.size), '+' ) 
      }
      else if ( (submatch || (sequence.seqReverse.size >= 16)) && ( targetSeq contains sequence.seqReverse ) ) {
        found += new Match( name, targetSeq, sequence.seqReverse, sequence.seqReverse.size, (targetSeq.size - sequence.seqReverse.size), '-' )
      }  // if
    }  // foreach
    
    val (bestName, bestCount ) = targetWords.find( sequence )
    val bestMatch = if ( bestCount > 0 ) targetMap( bestName ) else ""

    // bestCount will be number of k-mers and will be smaller than target length - check!!!
    if ( ( ( bestCount >= bestMatch.size - mismatches ) || ( bestCount >= 17 ) ) && (bestMatch.size > 0 ) ) {
      val (name, align_match, start, count, strand) = align.check( bestName, sequence.seqForward, sequence.seqReverse, targetMap( bestName ) )
      if ( ( count >= bestMatch.size - mismatches ) || ( count >= 17 ) ) 
        found += new Match( bestName, bestMatch, align_match, count, (bestMatch.size - count), strand )
    }  // if

    found
  }  // findBest

  // **************************************************************************
}  // class AnalyzeSequence

