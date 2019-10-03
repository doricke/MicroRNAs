
import scala.collection.mutable.Map

////////////////////////////////////////////////////////////////////////////////
//  This is the main class for Align class.
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


// ******************************************************************************
class Align {

  //****************************************************************************
  def same( a: Char, b: Char ) = if ( a == b ) 1 else 0

  //****************************************************************************
  def compare( a: String, b: String ): Int = {
    var count = 0
    for (i <- 0 until a.length)  
      count += (if ( a(i) == b(i) ) 1 else 0 )
      
    // println( "compare: " + a + " : " + b + ", count = " + count ) 
    count
  }  // compare

  //****************************************************************************
  def check( name: String, seq_f: String, seq_r: String, target:String ): Tuple5[String, String, Int, Int, Char] = {
    val for_match = evaluate( name, seq_f, target, '+' )

    if ( seq_r.length > 0 ) {
      val rev_match = evaluate( name, seq_r, target, '-' )
      if ( rev_match._3 > for_match._3 )  return rev_match
    }  // if

    return for_match
  }  // check

  //****************************************************************************
  def evaluate( name: String, text: String, target: String, strand: Char ): Tuple5[String, String, Int, Int, Char] = {
    var best_start = 0
    var best_count = 0
    var best_match = ""

    // Slide off the end by 2 positions to check for truncated matches. 
    for ( start <- 0 until (text.length - target.length + 3) ) {
      val len = if ( start + target.length <= text.length ) start + target.length else text.length
      val token = text.substring( start, len )
      val found = compare( token, target )
      // if ( found > 10 )
         // println( "align:  -> " + text + "\t" + target + "#" + token + " = " + found + ", start = " + start )
      if ( found > best_count ) {
        best_count = found
        best_match = token 
        best_start = start
      }  // if
    }  // for

    return (name, best_match, best_start, best_count, strand)
  }  // evaluate

  //****************************************************************************
  def search_map( seq_f: String, seq_r: String, targets: Map[String, String] ): Tuple5[String, String, Int, Int, Char] = {
    var best_match = Tuple5("", "", 0, 0, ' ')
    targets foreach {case (name, target) =>
      // Evaluate the forward sequence.
      val seq_match = check( name, seq_f, seq_r, target )
      if ( seq_match._3 > best_match._3 ) best_match = seq_match 
    }  // foreach

    // println( "Align: " + seq_f + " # " + best_match)
    return best_match
  }  // search_map

  //****************************************************************************

}  // class Align
