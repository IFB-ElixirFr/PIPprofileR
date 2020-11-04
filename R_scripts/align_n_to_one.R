#' @title Perform pairwise alignments between a set of query sequence and a single reference sequence
#' @author Jacques.van-Helden@france-bioinformatique.fr
#' @param refSequence reference sequence. Must be an object of class Biostrings::XStringSet
#' @param querySequences query sequences. Must be an object of class Biostrings::XStringSet
#' @param type="global-local" alignment type, passed to Biostrings::pairwiseAlignment()
#' @param sortByPIP=TRUE sort resulting alignments by decreasing mean PIP
#' @param outfile=NULL if specified, the alignments are savec in the speficied file
#' @param IDsuffix=NULL suffix to append to sequence names in the fasta file
#' @param ... other arguments are passed to  Biostrings::pairwiseAlignment()
#' @export
alignNtoOne <- function(refSequence, 
                        querySequences, 
                        type = "global-local",
                        sortByPIP = TRUE,
                        outfile = NULL, 
                        IDsuffix = NULL,
                        seqType, 
                        ...) {
  ## Prepare a table for the matching statistics
  stats <-  c("pid", "nchar", "insertNb", "insertLen", "delNb", "delLen",  "score")
  alignmentStats <- data.frame(matrix(nrow = length(querySequences), ncol = length(stats)))
  rownames(alignmentStats) <- names(querySequences)
  colnames(alignmentStats) <- stats
  
  ## Prepare a list for the pairwise alignments
  alignments <- list()
  
  ## Get parameters of the analysis
  seqNb <- length(querySequences)
  seqNames <- names(querySequences)
  
  withProgress(message = 'Alignment in progress', value = 0, {
                 
                 i <- 1
                 for (i in 1:seqNb) {
                   
                   subjectName <- seqNames[i]
                   
                   incProgress(1/seqNb, 
                               detail = subjectName)
                   message("\t\tAligning sequence ", i, "/", seqNb,  "\t", subjectName)
                   
                   alignment <- pairwiseAlignment(pattern = refSequence, 
                                                  subject = querySequences[i],
                                                  type = type) 
                   
                   alignmentStats[subjectName, "pid"] <- pid(alignment)
                   alignmentStats[subjectName, "nchar"] <- nchar(alignment)
                   alignmentStats[subjectName, "insertNb"] <- insertion(nindel(alignment))[1]
                   alignmentStats[subjectName, "insertLen"] <- insertion(nindel(alignment))[2]
                   alignmentStats[subjectName, "delNb"] <- deletion(nindel(alignment))[1]
                   alignmentStats[subjectName, "delLen"] <- deletion(nindel(alignment))[2]
                   alignmentStats[subjectName, "score"] <- score(alignment)
                   alignments[[subjectName]] <- alignment
                 }
               })
  
  if (sortByPIP) {
    PIPorder <- order(alignmentStats$pid, decreasing = TRUE)
    alignmentStats <- alignmentStats[PIPorder, ]
    alignments <- alignments[PIPorder]
  }
  
  ## Prepare the result object
  result <- list(alignments = alignments,
                 stats = alignmentStats, 
                 reference = refSequence,
                 seqType = seqType)
  
  #### Export sequences ####
  if (!is.null(outfile)) {
    message("\talignNtoOne()",
            "\tExporting multiple alignments to file\n\t\t", outfile)
  }
  
  ## Extract the matching sequences
  i <- 1
  nbAlignments <- length(alignments)
  result$sequences <- BStringSet() ## Empty list to store the desalined sequences
  for (i in 1:nbAlignments) {
    alignment <- alignments[[i]]
    sequenceName <- names(alignments)[i]
    subject <- subject(alignment)
    
    ## Suppress the dashes from the alignment to get the raw sequence
    sequence <- as.character(subject)
    sequenceDesaligned <- gsub(pattern = "-", replacement = "", x = sequence)
    seqStringSet <- BStringSet(x = sequenceDesaligned)
    
    
    ## Define a sequence ID for the fasta header
    sequenceID <- sequenceName
    if (!is.null(IDsuffix)) {
      sequenceID <- paste0(sequenceID, IDsuffix)
    } 
    sequenceID <- paste0(sequenceID, "_", start(subject), "-", end(subject))
    names(seqStringSet) <- sequenceID
    
    result$sequences <- append(result$sequences, seqStringSet)
    
    ## Append the sequence to the file
    if (!is.null(outfile)) {
      message("\tAppending sequence ", i, "/", nbAlignments, "\t", sequenceID)
      writeXStringSet(seqStringSet,
                      filepath = outfile, format = "fasta", append = i > 1)
    }    
  }
  message("\tExported alignments to\t", outfile)
  
  return(result)
}

