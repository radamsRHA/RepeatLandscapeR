#' Pipeline_ConcateProcessedReads.R: Function to X
#'
#' This function returns XXX
#' @param X X
#' @keywords comparative genomics, repeat elements, genomics, evolution
#' @export
#' @examples
#' library(ape)
#' library(RepeatLandscapeR)
#' 
#' Pipeline_ConcateProcessedReads(dir.Parent = '~/Copy/AmphibianGenomics/Methods&Materials/Step0_ProcessReads/Step0.2_ConcateProcessedReads_PIPELINE/', 
#'   path.GenomeFastqDir = '~/Copy/AmphibianGenomics/Methods&Materials/Step0_ProcessReads/Step0.2_ConcateProcessedReads_PIPELINE/GenomeFastqFiles/')



#####################################################################################################
# Pipeline_ConcateProcessedReads.R: Function to conduct PAL finder analyses on a set of genomes 									#
#####################################################################################################



Pipeline_ConcateProcessedReads <- function(dir.Parent, path.GenomeFastqDir){
  ######################################################
  # Step 1: Create subdirectory for PALfinder analyses #
  ######################################################
  string.ChildDir0 = paste(dir.Parent, '/ConcateProcessedReads_Analyses_', Sys.Date(), sep = "")
  unlink(string.ChildDir0, recursive = T)
  dir.create(string.ChildDir0, showWarnings = T, recursive = T)
  
  #########################################################
  # Step 2: Extract info from genome dir and make subdirs #
  #########################################################
  vector.GenomeFiles1 <- list.files(path= path.GenomeFastqDir, full.names = F, pattern = "SE.fastq.gz")
  vector.GenomeFiles2 <-list.files(path= path.GenomeFastqDir, full.names = T, pattern = "SE.fastq.gz")
  numeric.NumberOfGenomes <- length(vector.GenomeFiles1)
  
  ######################################################
  # Step 3: Loop through genomes and conduct PALfinder #
  ######################################################
  
  ######################################################
  # Step 3: Loop through genomes and conduct PALfinder #
  ######################################################
  
  for (i in 1:numeric.NumberOfGenomes){
    
    #################################################
    # Step 3.1: Extract info from genome fasta file #
    #################################################
    string.GenomeFastqFile_Name <- string.SplitName <- NULL
    string.GenomeFastqFile_Name <- vector.GenomeFiles1[i]
    string.SplitName <- strsplit(x = string.GenomeFastqFile_Name, split = ".SE.fastq.gz")[[1]][1]
    
    #####################################
    # Step 3.2: Make Sub-dir for genome #
    #####################################
    string.Genome_SubDir = paste(string.ChildDir0, '/', string.SplitName, sep = "")
    unlink(string.Genome_SubDir, recursive = T)
    dir.create(string.Genome_SubDir, showWarnings = T, recursive = T)
    
    #############################################
    # Step 3.3: Copy genome files to new subdir #
    #############################################
    string.SE_file <- paste(string.SplitName, '.SE.fastq.gz', sep = "")
    string.P1_file <- paste(string.SplitName, '.P1.fastq.gz', sep = "")
    string.P2_file <- paste(string.SplitName, '.P2.fastq.gz', sep = "")
    path.OriginalSE <- paste(path.GenomeFastqDir, string.SE_file, sep = "")
    path.OriginalP1 <- paste(path.GenomeFastqDir, string.P1_file, sep = "")
    path.OriginalP2 <- paste(path.GenomeFastqDir, string.P2_file, sep = "")
    path.NewSE <- paste(string.Genome_SubDir, '/', string.SE_file, sep = "")
    path.NewP1 <- paste(string.Genome_SubDir, '/', string.P1_file, sep = "")
    path.NewP2 <- paste(string.Genome_SubDir, '/', string.P2_file, sep = "")
    unlink(path.NewSE, recursive = T)
    unlink(path.NewP1, recursive = T)
    unlink(path.NewP2, recursive = T)
    file.copy(from = path.OriginalSE, to = path.NewSE)
    file.copy(from = path.OriginalP1, to = path.NewP1)
    file.copy(from = path.OriginalP2, to = path.NewP2)
    
    ##############################
    # Step 3.4: gunzip new files #
    ##############################
    string.ConcateFile <- paste(string.SplitName, "_P1_P2_SE_Concatenated.fastq", sep = "")
    setwd(string.Genome_SubDir)
    system(command = "gunzip *.gz")
    system(command = gsub(pattern = "XXX", replacement = string.ConcateFile, x = "cat *.fastq > XXX"))
    
    
  }
}