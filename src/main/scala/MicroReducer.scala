
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

import scala.collection.mutable.{Map, ArrayBuffer}

class MicroReducer( fileName: String, parameters: Parameters ) {

  private[this] val outFile: OutputFile = new OutputFile( fileName )
  private[this] val microFile: OutputFile = new OutputFile( parameters.dataset_name + "_Micros.csv" )
  private[this] val uniqFile: OutputFile = new OutputFile( parameters.dataset_name + "_Unique.csv" )
  private[this] val unknownsFile: OutputFile = new OutputFile( parameters.dataset_name + "_Unknowns" + ".fa" )
  private[this] val jsonFile: OutputFile = new OutputFile( parameters.dataset_name + ".json" )

  private[this] val totalCounts: Counts = new Counts( "All", parameters )
  private[this] val allCounts: Map[String, Counts] = Map[String, Counts]()

  private[this] val microCounts: Map[String, Map[String, Int]] = Map[String, Map[String, Int]]()
  private[this] val microTotalCounts: Map[String, Int] = Map[String, Int]()

  private[this] val uniqCounts: Map[String, Map[String, Int]] = Map[String, Map[String, Int]]()
  private[this] val uniqNames: Map[String, String] = Map[String, String]()
  private[this] val uniqTotalCounts: Map[String, Int] = Map[String, Int]()

  private[this] var seqTotal: Double = 0.0
  private[this] var seqLengths: Double = 0.0
  private[this] var alignedReads: Long = 0L

  // ***************************************************************************
  def addCounts( counts: Counts ) {
    val mid: String = counts.mid
  
    // Add these sequences to the totals
    counts.seen foreach {case(seq, cnt) => 
      // Tally the counts by barcode.
      addSequence( seq, cnt, counts.results( seq ), mid )
    }  // foreach
  }  // addCounts

  // ***************************************************************************
  def addSequence( seq: String, count: Int, result: MicroResult, mid: String ) {

    if ( allCounts contains mid )
      allCounts( mid ).addResult( seq, count, result )
    else {
      val counts = new Counts( mid, parameters )
      counts.addResult( seq, count, result )
      allCounts += (mid -> counts)
    }  // if

    // Tally for calculating mean trimmed length
    seqTotal += count * 1.0
    seqLengths += seq.length() * count * 1.0

    // Check for a known miRNA
    if ( result.miRNA.name.length > 0 ) {
      // Count aligned reads
      alignedReads += count

      // Add to the counts for this MicroRNA
      if ( ( microCounts contains mid ) == false )
        microCounts += (mid -> Map[String, Int]())
      if ( microCounts( mid ) contains result.miRNA.name)
        microCounts( mid )( result.miRNA.name ) += count 
      else
        microCounts( mid ) += (result.miRNA.name -> count )

      if ( microTotalCounts contains result.miRNA.name )
        microTotalCounts( result.miRNA.name ) += count 
      else
        microTotalCounts += (result.miRNA.name -> count )
 
      // Count the unique sequence occurances of this miRNA
      if ( ( uniqCounts contains mid ) == false )
        uniqCounts += (mid -> Map[String, Int]())
      if ( uniqCounts( mid ) contains result.miRNA.target_seq )
        uniqCounts( mid )( result.miRNA.target_seq ) += count 
      else
        uniqCounts( mid ) += (result.miRNA.target_seq -> count )

      if ( ( uniqNames contains result.miRNA.target_seq) == false )
        uniqNames += (result.miRNA.target_seq -> result.miRNA.name)

      if ( uniqTotalCounts contains result.miRNA.target_seq )
        uniqTotalCounts( result.miRNA.target_seq ) += count
      else
        uniqTotalCounts += (result.miRNA.target_seq -> count )
    }  // if
  }  // addSequence

  // ***************************************************************************
  def addResult( result: MicroResult ) {
    totalCounts.add( result.seq, 1 )
    val barcode: String = if ( result.barcodesF.target_seq.length > 0 ) result.barcodesF.name else "No_barcode"
    if ( (allCounts contains barcode) == false )  allCounts += (barcode -> new Counts( barcode, parameters ))
    allCounts( barcode ).add( result.seq, 1 )
  }  // addResult

  // ***************************************************************************
  def scoreHigh( metric: Double, level1: Double, level2: Double ): Double = {
    if ( metric >= level2 ) 
      2.0
    else if ( metric >= level1 )
      1.0
    else
      0.0
  }  // scoreHigh

  // ***************************************************************************
  def scoreLow( metric: Double, level1: Double, level2: Double ): Double = {
    if ( metric <= level2 )
      2.0
    else if ( metric <= level1 )
      1.0
    else
      0.0
  }  // scoreLow

  // ***************************************************************************
  def writeJson() {
    jsonFile.write( "{\n" )
    jsonFile.write( "  \"Dataset assay\": \"miRNA-seq\",\n" )
    jsonFile.write( "  \"Dataset name\": \"" + parameters.dataset_name + "\",\n" )
    jsonFile.write( "  \"Dataset size\": " + seqTotal.toInt + ",\n" )
    val mean_length = seqLengths / seqTotal
    jsonFile.write( "  \"Mean trimmed sequence length\": " + mean_length + ",\n" )
    jsonFile.write( "  \"Aligned reads\": " + alignedReads + ",\n" )
    val depth_score = scoreHigh( seqTotal, 4.0e6, 8.0e6 )
    val length_score = scoreHigh( mean_length, 17.0, 1.0e6 )
    val aligned_score = scoreHigh( alignedReads * 1.0, 2.0e6, 9.9e99 )
    val final_score = if ( depth_score == 0.0 || length_score == 0.0 || aligned_score == 0.0 ) 0.0 else
      (depth_score + length_score + aligned_score) / 3.0
    jsonFile.write( "  \"QC score\": " + final_score+ ",\n" )
    if ( final_score > 0.0 )
      jsonFile.write( "  \"QC status\": \"passed pending replication\"\n" )
    else
      jsonFile.write( "  \"QC status\": \"not passed\"\n" )
    jsonFile.write( "}\n" )
  }  // writeJson

  // ***************************************************************************
  def writeReport1() {
    // Write out the report header line.
    outFile.write( "Sequence\tTotal\tmicroRNA\tmicroMatch\tmicroSeq\tPrimer\tOrganism\tMatch\t" )
    allCounts.toList.sortBy{_._1} foreach {case (mid, counts) => outFile.write( mid + "\t" )}
    outFile.write( "\n" )
 
    // Write out the observed sequence counts. 
    totalCounts.seen.toList.sortBy{_._2}.reverse foreach {case (seq, totalCount) =>
      // println( "Report1: seq = " + seq + ", totalCount = " + totalCount )
      val result = totalCounts.results( seq )

      // if ( ( totalCount >= 10 ) || ( result.miRNA.name.length > 0 ) ) {
      if ( ( totalCount >= 1 ) || ( result.miRNA.name.length > 0 ) ) {
        var mod_seq = seq
        if ( result.miRNA.name.size > 0 )
          mod_seq = seq.replace( result.miRNA.target_seq, "|" + result.miRNA.target_seq + "|" )
        val longest_primer = result.longestPrimer()
        if ( longest_primer.target_seq.size > 0 )
          mod_seq = mod_seq.replace( longest_primer.target_seq, "[" + longest_primer.target_seq + "]" )
        outFile.write( mod_seq + "\t" )
        outFile.write( totalCount + "\t" )
        outFile.write( result.miRNA.name + "\t" + result.miRNA.target_seq + "\t" + result.miRNA.match_seq + "\t" )
        outFile.write( longest_primer.name + "\t" )
        if ( parameters.hits.contains( seq ) ) {
          outFile.write( parameters.hits( seq ).target_organism )
          outFile.write( "\t" )
          outFile.write( parameters.hits( seq ).target_strand + ":" + parameters.hits( seq ).target_sequence )
        }
        else
          outFile.write( "\t" )
        outFile.write( "\t" )
        allCounts.toList.sortBy{_._1} foreach {case (mid, counts) =>
          if ( counts.seen.contains( seq ) )
            outFile.write( counts.seen( seq ) + "\t" )
          else
            outFile.write( "0\t" )
        }  // foreach
        outFile.write( "\n" )

        if ( totalCount >= 100 )  
          unknownsFile.write( ">" + seq + "\tcount: " + totalCount + "\n" + seq + "\n" )
      }  // if
    }  // foreach
    println( "report1 complete" )
  }  // writeReport1

  // ***************************************************************************
  def writeReport2() {
    // Write out the report header line.
    microFile.write( "microRNA\tmiRNA\t" )
    microCounts.toList.sortBy{_._1} foreach {case (mid, counts) => microFile.write( mid + "\t" )}
    microFile.write( "Total\n" )
 
    // Write out the observed sequence counts. 
    microTotalCounts.toList.sortBy{_._2}.reverse foreach {case (micro, totalCount) =>
      microFile.write( micro + "\t" + parameters.microRnas( micro ) + "\t" )
      microCounts.toList.sortBy{_._1} foreach {case (mid, tally) =>
        if ( tally.contains( micro ) ) 
          microFile.write( tally( micro ) + "\t" )
        else
          microFile.write( "0\t" )
      }  // foreach
      microFile.write( totalCount + "\n" )
    }  // foreach
    println( "report2 complete" )
  }  // writeReport2

  // ***************************************************************************
  def writeReport3() {
    // Write out the report header line.
    uniqFile.write( "microRNA\tmiRNA\tobserved\t" )
    uniqCounts.toList.sortBy{_._1} foreach {case (mid, counts) => uniqFile.write( mid + "\t" )}
    uniqFile.write( "Total\n" )
 
    // Write out the observed sequence counts. 
    uniqTotalCounts.toList.sortBy{_._2}.reverse foreach {case (micro, totalCount) =>
      val mirnaName = uniqNames( micro )
      uniqFile.write( mirnaName + "\t" + parameters.microRnas( mirnaName ) + "\t" + micro + "\t" )
      uniqCounts.toList.sortBy{_._1} foreach {case (mid, tally) =>
        if ( tally.contains( micro ) ) 
          uniqFile.write( tally( micro ) + "\t" )
        else
          uniqFile.write( "0\t" )
      }  // foreach
      uniqFile.write( totalCount + "\n" )
    }  // foreach
    println( "report3 complete" )
  }  // writeReport3

  // ***************************************************************************
  def writeUnknowns() {

    // Write out the Unknown sequences and counts. 
    totalCounts.seen.toList.sortBy{_._2}.reverse foreach {case (seq, totalCount) =>
      val result = totalCounts.results( seq )

      if ( ( totalCount >= 10 ) && ( result.miRNA.name == "" ) ) {
        outFile.write( ">" + seq + "\tcount: " + totalCount + "\n" )
        outFile.write( seq + "\n" )
      }  // if
    }  // foreach
    println( "Unknowns file complete" )  
  }  // writeUnknowns
  
  // ***************************************************************************
  def process( counts: Counts ) {
    addCounts( counts ) 
    writeReport1()
    writeReport2()
    writeReport3()
    writeJson()
    // writeUnknowns()
  }  // process
}  // class MicroReducer
