
////////////////////////////////////////////////////////////////////////////////
//  This is the main class for MicroRnas application.
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
object MicroRnas {

  //****************************************************************************
  // This method 
  def parseArguments( args: Array[String] ): Arguments = {
    val fastq_name   = if ( args.size > 0 ) args( 0 ) else "test.fq"
    val barcode_name = if ( args.size > 1 ) args( 1 ) else "barcodes.tsv"
    val primer_name  = if ( args.size > 2 ) args( 2 ) else "primers.tsv"
    val ref_name     = if ( args.size > 3 ) args( 3 ) else "mature.fa"
    val hits_name    = if ( args.size > 4 ) args( 4 ) else "empty.csv" 

    new Arguments( fastq_name, barcode_name, primer_name, ref_name, hits_name )
  }  // parseArguments

  //****************************************************************************
  // MicroRnas main method.
  def main(args: Array[String]) {
    val dealer: MicroSetup = new MicroSetup( parseArguments( args ) )
    dealer.process()
  }  // main

}  // object MicroRnas
