#' Check.Nexus.Names: Function to check the overlap in taxnames between a vector (provided from a file usually) and a nexus file
#'
#' This function returns a list containing (1) a vector of names found in nexus file and (2) a vector of names that are not found in the nexus file
#' @param vector.TaxaNames Vector containing the taxa names to be evaluated
#' @param handle.NexusFile Handle object of nexus file obtained from (read.nexus.data from APE) 
#' @keywords comparative genomics, repeat elements, genomics, evolution
#' @export
#' @examples
#' library(ape)
#' library(RepeatLandscapeR)
#' 
#' vector.TaxaNames <- as.vector(read.table(file = '~/Copy/AmphibianGenomics/Methods&Materials/Step1_PhylogeneticInference/Step1.4_ReNameNexus/TaxaNameMapSheet.txt', header = T)[,2])
#' handle.NexusFile <- read.nexus.data(file = '~/Copy//AmphibianGenomics/Methods&Materials/Step1_PhylogeneticInference/Step1.4_ReNameNexus/Pyron2011_Amphib.nexus')
#' 
#' CheckName.Results <- Check.Nexus.Names(vector.TaxaNames = vector.TaxaNames, handle.NexusFile = handle.NexusFile)


###################################################################################
# Function to check the Taxa names in a vector with a nexus file									#
###################################################################################



Check.Nexus.Names <- function(vector.TaxaNames, handle.NexusFile){
  #####################################################
  # Step 1: Extract basic info from vector and handle #
  #####################################################
  numeric.NumberTaxa <- length(vector.TaxaNames)
  vector.NexusNames <- names(handle.NexusFile)
  numeric.NumberNexusNames <- length(vector.NexusNames)
  vector.FoundTaxa <- vector()
  vector.NotFoundTaxa <- vector()
  
  ######################################################
  # Step 2: Loop through taxa names and find matches  #
  ######################################################
  
  for (i in 1:numeric.NumberTaxa){
    string.TaxaName <- vector.TaxaNames[i]
    
    if (length(grep(pattern = string.TaxaName, x = vector.NexusNames))>=1){
      vector.FoundTaxa <- c(vector.FoundTaxa, string.TaxaName)
    } 
    if (length(grep(pattern = string.TaxaName, x = vector.NexusNames))==0){
      vector.NotFoundTaxa <- c(vector.NotFoundTaxa, string.TaxaName)
    }
    if (length(grep(pattern = string.TaxaName, x = vector.NexusNames))>1){
      print(string.TaxaName)
    }
    
    
  }
  return(list(vector.FoundTaxa = vector.FoundTaxa, vector.NotFoundTaxa = vector.NotFoundTaxa))
}