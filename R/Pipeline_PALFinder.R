#' Pipeline_PALFinder.R: Function to conduct PAL finder analyses on a directory of genome fasta files
#'
#' This function returns a directory containg the results of a PAL finder analysis on a set of genome fasta files
#' @param dir.Parent Path to parent directory, subdirectory for all analyses will be created here
#' @param path.GenomeFastaDir Path to directory containing the fasta genome files
#' @param path.Primer3 Path to Primer3_core executable (used by Palfinder)
#' @param path.PAL_finder Path to pal_finder executable
#' @keywords comparative genomics, repeat elements, genomics, evolution
#' @export
#' @examples
#' library(ape)
#' library(RepeatLandscapeR)
#' 
#' Pipeline_PALFinder(dir.Parent = '~/Copy/AmphibianGenomics/Methods&Materials/Step2_PAL_Finder/Step0_Examples/PALFinderAnalysis/', 
#'   path.GenomeFastaDir = '~/Copy/AmphibianGenomics/Methods&Materials/Step2_PAL_Finder/Step0_Examples/PALFinderAnalysis/Genomes_FastaFiles/', 
#'   path.Primer3 = '/Applications/primer3-2.4.0/src/primer3_core', 
#'   path.PAL_finder = '/Applications/pal_finder_v0.02.04.pl')


#####################################################################################################
# Pipeline_PALFinder: Function to conduct PAL finder analyses on a set of genomes 									#
#####################################################################################################



Pipeline_PALFinder <- function(dir.Parent, path.GenomeFastaDir, path.Primer3, path.PAL_finder){
  ######################################################
  # Step 1: Create subdirectory for PALfinder analyses #
  ######################################################
  string.ChildDir0 = paste(dir.Parent, '/PALfinder_Analyses_', Sys.Date(), sep = "")
  unlink(string.ChildDir0, recursive = T)
  dir.create(string.ChildDir0, showWarnings = T, recursive = T)
  
  #########################################################
  # Step 2: Extract info from genome dir and make subdirs #
  #########################################################
  vector.GenomeFiles1 <- list.files(path= path.GenomeFastaDir, full.names = F)
  vector.GenomeFiles2 <- list.files(path= path.GenomeFastaDir, full.names = T)
  numeric.NumberOfGenomes <- length(vector.GenomeFiles1)
  
  ######################################################
  # Step 3: Loop through genomes and conduct PALfinder #
  ######################################################
  
  for (i in 1:numeric.NumberOfGenomes){
    
    #################################################
    # Step 3.1: Extract info from genome fasta file #
    #################################################
    path.GenomeFastaFile <- string.GenomeFastaFile_Name <- NULL
    path.GenomeFastaFile <- vector.GenomeFiles2[i]
    string.GenomeFastaFile_Name <- vector.GenomeFiles1[i]
    
    #####################################
    # Step 3.2: Make Sub-dir for genome #
    #####################################
    string.Genome_SubDir = paste(string.ChildDir0, '/', string.GenomeFastaFile_Name, sep = "")
    unlink(string.Genome_SubDir, recursive = T)
    dir.create(string.Genome_SubDir, showWarnings = T, recursive = T)
    
    #######################################
    # Step 3.3: Copy genome to new subdir #
    #######################################
    string.PathNewGenome <- paste(string.Genome_SubDir, '/', string.GenomeFastaFile_Name, sep = "")
    unlink(string.PathNewGenome, recursive = T)
    file.copy(from = path.GenomeFastaFile, to = string.PathNewGenome)
    
    ######################################################
    # Step 3.4: Copy pal finder executable to new subdir #
    ######################################################
    string.PathNewPALfinder <- paste(string.Genome_SubDir, '/pal_finder_v0.02.04.pl', sep = "")
    unlink(string.PathNewPALfinder, recursive = T)
    file.copy(from = path.PAL_finder, to = string.PathNewPALfinder)
    
    ########################################
    # Step 3.5: Specify configuration file #
    ########################################
    string.ConfigFile <- paste("ConfigurationFile_", string.GenomeFastaFile_Name, '.txt', sep = "")
    string.PathConfigFile <- paste(string.Genome_SubDir, '/', string.ConfigFile, sep = "")
    string.PathMsatSummary <- paste("_MicrosatSummary_", string.GenomeFastaFile_Name, '.txt', sep = "")
    string.PathPALSummary <- paste("_PALSummary_", string.GenomeFastaFile_Name, '.txt', sep = "")
  
    Write.PALfinder.ConfigFile(path.ConfigFile = string.PathConfigFile, 
          numeric.FindPrimers = 0, 
          string.Platform = "454", 
          string.InputFormat = "fasta", 
          numeric.PE = 0, 
          path.454readfile = string.GenomeFastaFile_Name, 
          path.MicrosateSumout = string.PathMsatSummary, 
          path.PALsummaryOut = string.PathPALSummary, 
          path.Primer3 = path.Primer3)
    
    ###########################
    # Step 3.6: Run PALFINDER #
    ###########################
    setwd(string.Genome_SubDir)
    string.PALcommand <- 'perl ./pal_finder_v0.02.04.pl XXX'
    string.PALcommand <- gsub(pattern = "XXX", replacement = string.ConfigFile, x = string.PALcommand)
    system(string.PALcommand)
    
    
  }
}