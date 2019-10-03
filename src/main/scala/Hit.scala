
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

// ****************************************************************************
class Hit() {
    var query_name: String = ""    // 1
    var query_strand: String = ""
    var query_start: Int = 0
    var query_end: Int = 0
    var query_product: String = ""
    var query_organism: String = ""
    var query_taxonomy: String = ""
    var identities: Int = 0
    var percent: Int = 0
    var gaps: Int = 0                 // 10
    var gap_characters: Int = 0
    var target_name: String = ""
    var target_strand: String = ""
    var target_start: Int = 0
    var target_end: Int = 0
    var target_organism: String = ""
    var target_product: String = ""
    var target_taxonomy: String = ""
    var query_sequence: String = ""
    var target_sequence: String = ""  // 20
    var program_name: String = ""
    var database_name: String = ""
    var target_length: Int = 0
    var query_length: Int = 0
    var expect: String = ""
    var target_gene: String = ""
    var locus_tag: String = ""
    var target_note: String = ""
    var target_protein_id: String = ""
    var target_taxon_id: String = ""  // 30
    var target_accession: String = ""
    var target_source: String = ""
    var target_strain: String = ""

  // **************************************************************************
  def parseLine( line: String ) {
    val tokens = line.filterNot(_ == '"' ).split( "," )
    query_name = tokens(0)
    query_strand = tokens(1)
    query_start = tokens(2).toInt
    query_end = tokens(3).toInt
    query_product = tokens(4)
    query_organism = tokens(5)
    query_taxonomy = tokens(6)
    identities = tokens(7).toInt
    percent = tokens(8).toInt
    gaps = tokens(9).toInt             // 10
    gap_characters = tokens(10).toInt
    target_name = tokens(11)
    target_strand = tokens(12)
    target_start = tokens(13).toInt
    target_end = tokens(14).toInt
    target_organism = tokens(15)
    target_product = tokens(16)
    target_taxonomy = tokens(17)
    query_sequence = tokens(18)
    target_sequence = tokens(19)       // 20
    program_name = tokens(20)
    database_name = tokens(21)
    target_length = tokens(22).toInt
    query_length = tokens(23).toInt
    if ( tokens.size >= 25 ) expect = tokens(24)
    if ( tokens.size >= 26 ) target_gene = tokens(25)
    if ( tokens.size >= 27 ) locus_tag = tokens(26)
    if ( tokens.size >= 28 ) target_note = tokens(27)
    if ( tokens.size >= 29 ) target_protein_id = tokens(28)
    if ( tokens.size >= 30 ) target_taxon_id = tokens(29)       // 30
    if ( tokens.size >= 31 ) target_accession = tokens(30)
    if ( tokens.size >= 32 ) target_source = tokens(31)
    if ( tokens.size >= 33 ) target_strain = tokens(32)
  }  // parseLine

  // **************************************************************************
  def toStr( del: String ): String = {
        query_name + del + 
        query_strand + del + 
        query_start + del + 
        query_end + del +
        query_product + del + 
        query_organism + del + 
        query_taxonomy + del +
        identities + del + 
        percent + del + gaps + del + 
        gap_characters + del +
        target_name + del + 
        target_strand + del + 
        target_start + del +
        target_end  + del +
        target_organism  + del +
        target_product  + del +
        target_taxonomy  + del +
        query_sequence  + del +
        target_sequence  + del +
        program_name  + del +
        database_name  + del +
        target_length  + del +
        query_length  + del +
        expect  + del +
        target_gene  + del +
        locus_tag  + del +
        target_note  + del +
        target_protein_id  + del +
        target_taxon_id  + del +
        target_accession  + del +
        target_source  + del +
        target_strain
  }  // toStr

  // **************************************************************************
}  // class Hit 
