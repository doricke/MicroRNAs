
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

class MicroSetup( val arguments: Arguments ) {

  // ***************************************************************************
  def process(): Unit = {
    val host = "localhost" 
    val mismatches = 2
    val wordSize = 6

    // System.out.println( "Fastq name: " + arguments.fastq_name )
    val tokens = arguments.fastq_name.split( '.' )
    val dataset_name = tokens( 0 )

    // Read in the sequence barcodes
    val barcodeReader = new TableReader( arguments.barcode_name )
    // barcodeReader.hash1 foreach {case (name, barcode) => println( "mid: " + name + "\t" + barcode ) }

    // Read in the sequence primers
    val primerReader = new TableReader( arguments.primer_name )  
    // primerReader.hash1 foreach {case (name, primer) => println( "Primer Forward: " + name + "\t" + primer ) }
    // primerReader.hash2 foreach {case (name, primer) => println( "Primer Reverse: " + name + "\t" + primer ) }

    // Read in the list of known alleles with sequence patterns.
    val microReader = new FastaIterator( arguments.ref_name )
    val microRnas = microReader.fastasToMap( "Homo sapiens", "Caenorhabditis elegans miR-39" )
    // val microRnas = microReader.fastasToMap( "Mus musculus", "Caenorhabditis elegans miR-39" )
    
    // Read in the list of Similarity hits.
    val hitReader = new HitIterator( arguments.hits_name )
    val hits = if ( arguments.hits_name.length > 0 ) hitReader.mapHits() else Map[String, Hit]()

    val parameters = new Parameters( barcodeReader.hash1, primerReader.hash1, primerReader.hash2, microRnas, hits, wordSize, mismatches, host, 1, dataset_name )

    // Start the microRNA reducer.
    val microReducer = new MicroReducer( dataset_name + "_miRNAs.csv", parameters )

    val counter = new SequenceCounts( microReducer, parameters )
    counter.process( arguments.fastq_name )
  }  // process

  // ***************************************************************************
}  // class MicroSetup
