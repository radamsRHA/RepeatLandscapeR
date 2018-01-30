#' Check.Tree.Names: Function to check the overlap in taxnames between a vector (provided from a file usually) and a newick tree
#'
#' This function returns a list containing (1) a vector of names found in newick tree and (2) a vector of names that are not found in the newick tree
#' @param vector.TaxaNames Vector containing the taxa names to be evaluated
#' @param handle.Tree Handle object of newick tree obtained from (read.tree from APE) 
#' @keywords comparative genomics, repeat elements, genomics, evolution
#' @export
#' @examples
#' library(ape)
#' library(RepeatLandscapeR)
#' 


###################################################################################
# Function to check the Taxa names in a vector with a nexus file									#
###################################################################################



Check.Tree.Names <- function(vector.TaxaNames, handle.Tree){
  #####################################################
  # Step 1: Extract basic info from vector and handle #
  #####################################################
  numeric.NumberTaxa <- length(vector.TaxaNames)
  vector.TreeNames <- handle.Tree$tip.label
  numeric.NumberNexusNames <- length(vector.TreeNames)
  vector.FoundTaxa <- vector()
  vector.NotFoundTaxa <- vector()
  
  ######################################################
  # Step 2: Loop through taxa names and find matches  #
  ######################################################
  
  for (i in 1:numeric.NumberTaxa){
    string.TaxaName <- vector.TaxaNames[i]
    
    if (length(grep(pattern = string.TaxaName, x = vector.TreeNames))>=1){
      vector.FoundTaxa <- c(vector.FoundTaxa, string.TaxaName)
    } 
    if (length(grep(pattern = string.TaxaName, x = vector.TreeNames))==0){
      vector.NotFoundTaxa <- c(vector.NotFoundTaxa, string.TaxaName)
    }
    if (length(grep(pattern = string.TaxaName, x = vector.TreeNames))>1){
      print(string.TaxaName)
    }
    
    
  }
  return(list(vector.FoundTaxa = vector.FoundTaxa, vector.NotFoundTaxa = vector.NotFoundTaxa))
}