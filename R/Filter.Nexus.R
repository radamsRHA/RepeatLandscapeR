#' Filter.Nexus: Function to filter a large nexus file to only contain sequences from a given vector of names
#'
#' This function writes a new nexus file with only sequences from taxa contained in the vectort.TaxaNames
#' @param vector.TaxaNames Vector containing the taxa names to be evaluated
#' @param handle.NexusFile Handle object of nexus file obtained from (read.nexus.data from APE) 
#' @keywords comparative genomics, repeat elements, genomics, evolution
#' @export
#' @examples
#' library(ape)
#' library(RepeatLandscapeR)
#' 
#' vector.TaxaNames <- as.vector(read.table(file = '~/Copy/AmphibianGenomics/Methods&Materials/Step1_PhylogeneticInference/Step1.5_FilterNexus/TaxaNameMapSheet.txt', header = T)[,2])
#' handle.NexusFile <- read.nexus.data(file = '~/Copy//AmphibianGenomics/Methods&Materials/Step1_PhylogeneticInference/Step1.5_FilterNexus/Pyron2011_Amphib.nexus')
#' Filter.Nexus(vector.TaxaNames = vector.TaxaNames, handle.NexusFile = handle.NexusFile, path.OutNexus = '~/Copy/AmphibianGenomics/Methods&Materials/Step1_PhylogeneticInference/Step1.5_FilterNexus/Filtered.nexus')




###################################################################################
# Filter.Nexus: Function to check the Taxa names in a vector with a nexus file									#
###################################################################################



Filter.Nexus <- function(vector.TaxaNames, handle.NexusFile, path.OutNexus){
  #####################################################
  # Step 1: Extract basic info from vector and handle #
  #####################################################
  numeric.NumberTaxa <- length(vector.TaxaNames)
  vector.NexusNames <- names(handle.NexusFile)
  numeric.NumberNexusNames <- length(vector.NexusNames)
  numeric.NumberSites <- length(handle.NexusFile[[1]])

  
  ################################################################
  # Step 2: Check taxaname overlap between vector and nexus file #
  ################################################################
  CheckName.Results <- Check.Nexus.Names(vector.TaxaNames = vector.TaxaNames, handle.NexusFile = handle.NexusFile)
  vector.FoundNames <- CheckName.Results$vector.FoundTaxa
  numeric.NumberFoundNames <- length(vector.FoundNames)
  
  ##########################################
  # Step 3: Init matrix for filtered nexus #
  ##########################################
  matrix.FilteredNexus <- matrix(nrow = numeric.NumberFoundNames, ncol = numeric.NumberSites)
  rownames(matrix.FilteredNexus) <- vector.FoundNames
  
  ########################################################
  # Step 4: Loop through names/taxa and append to matrix #
  ########################################################
  
  for (i in 1:numeric.NumberFoundNames){
    string.TaxaName <- vector.FoundNames[i]
    list.Sequence <- handle.NexusFile[[string.TaxaName]]
    matrix.FilteredNexus[string.TaxaName,] <- list.Sequence
  }

  write.nexus.data(x = matrix.FilteredNexus, file = path.OutNexus, interleaved = F)
}