
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

class SequenceCounts( microReducer: MicroReducer, parameters: Parameters ) {

  // **************************************************************************
  def checkBarcode( filename: String ): String = {
    val tokens = filename.split( "_" )
    if ( filename.contains( "nomatch" ) )  return "noBarcode"
    if ( ( tokens.length > 2 ) && ( tokens(2) == "R" ) )  return ("I" + tokens(1))
    if ( tokens(1).contains( "S" ) )  return tokens(1)
    return ""
  }  // checkBarcode

  // ***************************************************************************
  def countSequences( filename: String, barcode: String ): Counts = {
    // val barcode = checkBarcode( filename )
    // println( "SequenceCounts processing: " + filename + ", barcode: " + barcode )
    val counts = new Counts( barcode, parameters )
    if ( filename.endsWith( ".fasta" ) || filename.endsWith( ".fa" ) ) {
      val fastaIterator = new FastaIterator( filename )
      fastaIterator.nextLine
      while( fastaIterator.endOfFile == false ) 
        counts.add( fastaIterator.nextFasta( barcode ).sequence, 1 )
    } else {
      val fastqIterator = new FastqIterator( filename )
      fastqIterator.nextLine
      while( fastqIterator.endOfFile == false ) 
        counts.add( fastqIterator.nextFastq( barcode ).sequence, 1 )
    }  // if
    counts
  }  // countSequences

  // ***************************************************************************
  def process( filename: String ) {
     microReducer.process( countSequences( filename, filename ) )
  }  // process

  // ***************************************************************************
}  // class SequenceCounts
