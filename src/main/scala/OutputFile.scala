
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

import java.io._
import scala.io._

// ****************************************************************************
class OutputFile( var fileName: String ) {

  protected[this] var outputFile: FileOutputStream = null
  protected[this] var outputData: PrintStream = null
  try {
    outputFile = new FileOutputStream( fileName )

    outputData = new PrintStream( outputFile )
  }  // try
  catch { 
    case e: Exception => println( "OutputFile error opening file: " + fileName + " message: " + e.toString )
  }  // catch

  // **************************************************************************
  def initialize = { fileName = ""; outputFile = null; outputData = null }

  // **************************************************************************
  def setFileName( value: String ) { fileName = value }

  // **************************************************************************
  def closeFile() = { 
    if ( outputData != null )
    {
      outputData.flush()
      outputData.close()
    }  // if

    try {
      if ( outputFile != null )
        outputFile.close()
    }  // try
    catch {
      case e: Exception => println( "OutputFile error closing file: " + fileName + " message: " + e.toString )
    }  // catch
  }  // closeFile

  // **************************************************************************
  def deleteFile = {
    closeFile
    val deleteFile: File = new File( fileName )
    deleteFile.deleteOnExit()
    outputFile = null
    outputData = null
  }  // deleteFile

  // **************************************************************************
  def openFile: Unit = { 
    try {
      outputFile = new FileOutputStream( fileName )

      outputData = new PrintStream( outputFile )
    }  // try
    catch {
      case e: Exception => println( "OutputFile error in openFile for: " + fileName + " message: " + e.toString )
    }  // catch
  } // openFile  

  // **************************************************************************
  def print( value: String ): Unit = { 
    try { outputData.print( value ) } 
    catch { case e: Exception => println( "OutputFile error in print: " + e.toString() ) }
  }  // print

  // **************************************************************************
  def println( value: String ): Unit = { 
    try { outputData.println( value ) } 
    catch { case e: Exception => println( "OutputFile error in println: " + e.toString() ) }
  }  // println

  // **************************************************************************
  def println: Unit = { 
    try { outputData.println() } 
    catch { case e: Exception => println( "OutputFile error in println: " + e.toString() ) }
  }  // println

  // **************************************************************************
  def write( value: String ): Unit = { 
    try { outputData.print( value ) } 
    catch { case e: Exception => println( "OutputFile error in write: " + e.toString() ) }
  }  // write

  // **************************************************************************

}  // class OutputFile

