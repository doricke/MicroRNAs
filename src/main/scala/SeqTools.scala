
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

import scala.collection.mutable.Set

// ****************************************************************************************
class SeqTools {

  // **************************************************************************
  def compliment( base: Char ): Char = {
    base match {
      case 'A' => 'T'
      case 'a' => 't'
      case 'C' => 'G'
      case 'c' => 'g'
      case 'G' => 'C'
      case 'g' => 'c'
      case 'T' => 'A'
      case 't' => 'a'
      case 'N' => 'N'
      case 'n' => 'n'
      case 'R' => 'Y'
      case 'r' => 'y'
      case 'Y' => 'R'
      case 'y' => 'r'
      case '.' => '.'
      case '-' => '-'
      case _ => '?'
    }  // match
  }  // compliment

  // **************************************************************************
  def reverseCompliment( sequence: String): String = {
    var seqReverse = new StringBuilder()
    for ( i <- sequence.length-1 to 0 by -1 )
      seqReverse += compliment( sequence.charAt( i ) )
    seqReverse.toString()
  }  // reverseCompliment

  // **************************************************************************
  def seqSplit( seq: String, wordSize: Int ): collection.mutable.Set[String] = {
    val newSet = collection.mutable.Set[String]()
    for ( i <- 0 until seq.length - (wordSize-1) ) {
      newSet += seq.substring( i, i+wordSize )
    }  // for

    newSet
  }  // seqSplit

  // **************************************************************************************
  def translateCodon( codon: String): Char = {
    // println( "translateCodon: " + codon )

    codon match {
      case "TTT" => 'F'
      case "TTC" => 'F'
      case "TTA" => 'L'
      case "TTG" => 'L'

      case "CTT" => 'L'
      case "CTC" => 'L'
      case "CTA" => 'L'
      case "CTG" => 'L'

      case "ATT" => 'I'
      case "ATC" => 'I'
      case "ATA" => 'I'
      case "ATG" => 'M'

      case "GTT" => 'V'
      case "GTC" => 'V'
      case "GTA" => 'V'
      case "GTG" => 'V'

      case "TCT" => 'S'
      case "TCC" => 'S'
      case "TCA" => 'S'
      case "TCG" => 'S'

      case "CCT" => 'P'
      case "CCC" => 'P'
      case "CCA" => 'P'
      case "CCG" => 'P'

      case "ACT" => 'T'
      case "ACC" => 'T'
      case "ACA" => 'T'
      case "ACG" => 'T'

      case "GCT" => 'A'
      case "GCC" => 'A'
      case "GCA" => 'A'
      case "GCG" => 'A'

      case "TAT" => 'Y'
      case "TAC" => 'Y'
      case "TAA" => '*'
      case "TAG" => '*'

      case "CAT" => 'H'
      case "CAC" => 'H'
      case "CAA" => 'Q'
      case "CAG" => 'Q'

      case "AAT" => 'N'
      case "AAC" => 'N'
      case "AAA" => 'K'
      case "AAG" => 'K'

      case "GAT" => 'D'
      case "GAC" => 'D'
      case "GAA" => 'E'
      case "GAG" => 'E'

      case "TGT" => 'C'
      case "TGC" => 'C'
      case "TGA" => '*'
      case "TGG" => 'W'

      case "CGT" => 'R'
      case "CGC" => 'R'
      case "CGA" => 'R'
      case "CGG" => 'R'

      case "AGT" => 'S'
      case "AGC" => 'S'
      case "AGA" => 'R'
      case "AGG" => 'R'

      case "GGT" => 'G'
      case "GGC" => 'G'
      case "GGA" => 'G'
      case "GGG" => 'G'

      case _ => '.'
    }  // match
  }  // translateCodon

  // **************************************************************************************
  def translate( seq: String ): String = {
    val dnaSeq = seq.toUpperCase
    var protein: StringBuilder = new StringBuilder()

    for ( i: Int <- 0 until seq.length-2 by 3 )
      protein += translateCodon( dnaSeq.substring( i, i+3 ) )

    protein.toString()
  }  // translate

  // **************************************************************************************
}  // class SeqTools

