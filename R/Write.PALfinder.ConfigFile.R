#' Write.PALfinder.ConfigFile: Function to write a configuration file for PAL finder
#'
#' This function returns writes a series of information into the PALfinder configuration file set by the user
#' @param path.ConfigFile Path to output configuration file
#' @param numeric.FindPrimers Numeric answer (0 = no, 1 = yes)
#' @param string.Platform String for sequencing platform ("Illumina" or "454")
#' @param string.InputFormat String for input format ("fasta" or "fastq")
#' @param numeric.PE Path to fasta genome file
#' @param path.inputreadfile Path to fastq file read1 (Illumina)
#' @param path.inputPEreadfile Path to fastq file read2 (Illumina)
#' @param path.MicrosateSumout Path to output microsatellite summary statistics
#' @param path.PALsummaryOut Path to output PAL summary statistics
#' @param path.Primer3 Path to Primer3_core executable
#' @keywords comparative genomics, repeat elements, genomics, evolution
#' @export
#' @examples
#' library(ape)
#' library(RepeatLandscapeR)
#' 
#' 
#' Write.PALfinder.ConfigFile(path.ConfigFile = '~/Copy/AmphibianGenomics/Methods&Materials/Step2_PAL_Finder/Step0_Examples/PALFinderAnalysis/config.txt', 
#' numeric.FindPrimers = 0, 
#' string.Platform = "454", 
#' string.InputFormat = "fasta", 
#' numeric.PE = 0, 
#' path.454readfile = 'SAMPLED_RMB2503_Rhyacotriton-kezeri.fasta', 
#' path.MicrosateSumout = 'test_microsat_summary.txt', 
#' path.PALsummaryOut = 'test_PAL_summary.txt', 
#' path.Primer3 = '/Applications/primer3-2.4.0/src/primer3_core')







###################################################################################
# Write.PALfinder.ConfigFile: Function to 									#
###################################################################################



Write.PALfinder.ConfigFile <- function(path.ConfigFile, numeric.FindPrimers, string.Platform, string.InputFormat, numeric.PE, path.454readfile, path.inputreadfile, path.inputPEreadfile, path.MicrosateSumout, path.PALsummaryOut, path.Primer3){
  ######################################################
  # Step 0: Delete any old config files with same path #
  ######################################################
  unlink(path.ConfigFile, recursive = T)
  
  #######################
  # Step 1: Start block #
  #######################
  string.StartBlock <- '# config.txt
# configuration file for pal_finder_v0.02.04.pl
#
# This file determines the configuration and input parameters for
# pal_finder (version 0.02.03). All lines in this file that start
# with "#" are comments and are not considered input to the
# program. All other lines containing text are considered input
# parameters and need to follow the format of:
# 
# <variable_name> <variable_vale>
# 
# Changing variable names will cause the script to run incorrectly
# or not run at all. You should chane the variable values to suit
# your experimental needs and input data type. Descriptions of
# variables and acceptable variable values are described below.  

#----------------------- Primer Design -------------------------#
#
# Enter 1 to design primers or 0 if you just want to calculate the
# microsatellite statistics (faster).\n'
  
  write(x = string.StartBlock, file = path.ConfigFile, append = T)
  string.FindPrimers <- gsub(pattern = "XXX", replacement = numeric.FindPrimers, x = "findPrimers XXX")
  write(x = string.FindPrimers, file = path.ConfigFile, append = T)
  
  #########################
  # Step 2 Platform block #
  #########################
  string.PlatformBlock <- '
#------------------ Platform Input Parameters ------------------#
#
# The possible input values for the platform variable
# are "454" or "Illumina" (without quotes, case-sensitive).\n'
  write(x = string.PlatformBlock, file = path.ConfigFile, append = T)
  string.PlatformOption <- gsub(pattern = "XXX", replacement = string.Platform, x = "platform XXX")
  write(x = string.PlatformOption, file = path.ConfigFile, append = T)
  
  ######################
  # Step 3 Input block #
  ######################
  string.InputBlock <- '
#------------------- Input Format Parameters -------------------#
#
# The possible input values for the platform variable are
# "fasta", "fastq" or "scarf" (without quotes, case-sensitive).
# Currently fasta input is required for 454 single end reads and
# fastq or scarf is required for Illumina paired end data.\n'
  write(x = string.InputBlock, file = path.ConfigFile, append = T)
  string.InputOption <- gsub(pattern = "XXX", replacement = string.InputFormat, x = "inputFormat XXX")
  write(x = string.InputOption, file = path.ConfigFile, append = T)
  
  ###################
  # Step 3 PE block #
  ###################
  string.peBlock <- '
#---------------------- Paired-End Reads -----------------------#
#
# Enter 1 if your reads are paired-end or 0 if not. Currently the 
# script only takes Illumina paired-end reads and 454 single-end
# reads. Paired-end reads should have 2 separate read files.\n'
  
  write(x = string.peBlock, file = path.ConfigFile, append = T)
  string.peOption <- gsub(pattern = "XXX", replacement = numeric.PE, x = "pairedEnd  XXX")
  write(x = string.peOption, file = path.ConfigFile, append = T)
  
  
  ############################
  # Step 4: Read file blocks #
  ############################
  if (string.Platform == "454"){
    string.ReadBlock <- '
#----------------------- 454 Read Files ------------------------#
#
# If you are running 454 data enter the file name of the raw 454
# reads. This should be in the fasta format, not fastq. If this
# file is not in the same directory as the one you are running
# the primer_designer script from, then you must include the
# full file path.
#
# If you are running Illumina data you may disregard this section
# (but do not delete).\n'
    write(x = string.ReadBlock, file = path.ConfigFile, append = T)
    string.Path454Reads <- gsub(pattern = "XXX", replacement = path.454readfile, x = "input454reads  XXX")
    write(x = string.Path454Reads, file = path.ConfigFile, append = T)
    
    string.WrongBlock <- '
#------------------- Illumina PE Read Files --------------------#
#
# If you are running Illumina data enter the two paired-end
# files, one for each variable name below. The file format should
# be a raw Illumina scarf format, not fastq. If these file are
# not in the same directory as the primer_designer script then
# you must include the full file path.
#
# If you are running 454 data you may disregard this section (but
# do not delete).

inputReadFile  DUMMY.fq
pairedReadFile  DUMMY.fq'
    write(x = string.WrongBlock, file = path.ConfigFile, append = T)
  }
  
  if (string.Platform == "Illumina"){
    string.ReadBlock <- '
#----------------------- 454 Read Files ------------------------#
#
# If you are running 454 data enter the file name of the raw 454
# reads. This should be in the fasta format, not fastq. If this
# file is not in the same directory as the one you are running
# the primer_designer script from, then you must include the
# full file path.
#
# If you are running Illumina data you may disregard this section
# (but do not delete).

input454reads  DUMMY.fna

#------------------- Illumina PE Read Files --------------------#
#
# If you are running Illumina data enter the two paired-end
# files, one for each variable name below. The file format should
# be a raw Illumina scarf format, not fastq. If these file are
# not in the same directory as the primer_designer script then
# you must include the full file path.
#
# If you are running 454 data you may disregard this section (but
# do not delete).\n'
    
    write(x = string.ReadBlock, file = path.ConfigFile, append = T)
    string.inputREADS <- gsub(pattern = "XXX", replacement = path.inputreadfile, x = "inputReadFile  XXX")
    write(x = string.inputREADS, file = path.ConfigFile, append = T)
    string.inputPEreads <- gsub(pattern = "XXX", replacement = path.inputPEreadfile, x = "pairedReadFile  XXX")
    write(x = string.inputPEreads, file = path.ConfigFile, append = T)
    
  }
  
  #########################
  # Step 5: Output blocks #
  #########################
  string.OutputBlock <- '
#------------------------ Output Files -------------------------#
#
# Enter the names of output files. Include the full path names to
# put the files in directories other than the directory you run
# the script from. The MicrosatSumOut file gives a summary of the
# types of microsatellites found in the data. The PALsummaryOut
# file lists all the read IDs with microsatellites and gives
# information about primer pairs if found and associated repeats.\n'
  write(x = string.OutputBlock, file = path.ConfigFile, append = T)
  string.MSumOut <- gsub(pattern = "XXX", replacement = path.MicrosateSumout, x = "MicrosatSumOut  XXX")
  string.PALSumOut <- gsub(pattern = "XXX", replacement = path.PALsummaryOut, x = "PALsummaryOut  XXX")
  write(x = string.MSumOut, file = path.ConfigFile, append = T)
  write(x = string.PALSumOut, file = path.ConfigFile, append = T)
  
  
  #########################
  # Step 6: Major Block 2 #
  #########################
  string.MajorBlock2 <- '

#----------------------- Microsat Info -------------------------#
#
# Define the minimum number of n-mer repeat units you want to
# detect for each n-mer length. Put 0 if you do not want to
# consider a particular n-mer unit size. For instance, a value of
# 6 for the 2merMinReps would mean pal_finder will record all
# 2mer repeated at least 6 times in a row (at least 12 bases;
# example: ATATATATATAT)

2merMinReps 	6
3merMinReps 	4
4merMinReps 	3
5merMinReps 	3
6merMinReps 	3


#----------------------- primer3 Files -------------------------#
#
# The primer3input and primer3output variables hold the names of
# primer3-specific files. These are internal, temporary files
# that hold the raw input and output for the primer3 program.
# These are not files you need to provide. They may, however be
# useful for debugging purposes or if you want more specific
# information about the primer metrics from primer3. You can
# specify the name and location of these files by the values for
# these variables. You can also retain these files by setting
# keepPrimer3files to 1. Setting to 0 causes these files to be
# deleted after the script finishes.

primer3input  test/output/pr3in.txt
primer3output  test/output/pr3out.txt
keepPrimer3files  0

# The primer3executable variable sets the location of the
# primer3 executable. If the location of the primer3 executable
# has been added to your systems $PATH variable, all you need
# to put is the binary executable name (usually "primer3_core").
# The primer3 version required for this script is 2.0.0. If you
# have multiple versions of primer3, make sure you put the
# location of the correct version here. The correct version of
# primer3 can be downloaded from here:
# http://primer3.sourceforge.net/releases.php'
 
  write(x = string.MajorBlock2, file = path.ConfigFile, append = T)
  string.Primer3 <- gsub(pattern = "XXX", replacement = path.Primer3, x = "primer3executable  /Applications/primer3-2.4.0/src/primer3_core")
  write(x = string.Primer3, file = path.ConfigFile, append = T)
  
  #######################
  # Step 7: Final block #
  #######################
  string.FinalBlock <- '

#---------------------- primer prefix ------------------------#
#
# This value will be added onto the beginning of all primer
# names. The rest of the primer name is an id #. For instance,
# if the value is "test_" primer names will look like
# "test_123" or "test_" followed by a unique number identifier. 

prNamePrefix  test_


#-------------------- primer3 Parameters ---------------------#
#
# These settings are passed on to the primer3 program and can
# be adjusted to manipulate the primer pair requirements. For
# further details on these parameters consult the primer3 
# (version 2.0.0) documentation.

PRIMER_TASK  pick_pcr_primers

PRIMER_OPT_SIZE  20

PRIMER_MIN_SIZE  18

PRIMER_MAX_SIZE  30

PRIMER_MAX_NS_ACCEPTED  0

# The following two values are adjusted by the script for paired ends reads
# however for 454 reads they can be adjusted by the user

pr3ProductSizeRangeMinVal  60
pr3ProductSizeRangeMaxVal  500

PRIMER_OPT_SIZE  20

PRIMER_MIN_GC  30.0

PRIMER_MAX_GC  80.0

PRIMER_GC_CLAMP  2

PRIMER_MAX_END_GC  5

PRIMER_MIN_TM  58.0

PRIMER_MAX_TM  65.0

PRIMER_OPT_TM  62.0

PRIMER_PAIR_MAX_DIFF_TM  2.0

PRIMER_TM_FORMULA  0

PRIMER_MAX_SELF_ANY  8.00

PRIMER_PAIR_MAX_COMPL_ANY  8.00

PRIMER_MAX_SELF_END  3.00

PRIMER_PAIR_MAX_COMPL_END  3.00

PRIMER_MAX_POLY_X  4

PRIMER_LOWERCASE_MASKING  0 

PRIMER_NUM_RETURN  1

PRIMER_MISPRIMING_LIBRARY  simple.ref

PRIMER_MAX_LIBRARY_MISPRIMING  10.00

PRIMER_LIB_AMBIGUITY_CODES_CONSENSUS   0\n'
  write(x = string.FinalBlock, file = path.ConfigFile, append = T) 
  
}