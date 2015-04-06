######     This has a bug

#################################################
#
#   is.valid.numeric
#
#   input is a single object
#   the input can be a vector or matrix
#
#   returns false if:
#       object is not numeric mode
#       length of object is 0
#       nan.valid is FALSE and NaN values are detected
#       na.valid  is FALSE and NA  values are detected
#       inf.valid is FALSE and Inf or -Inf values are found.
#     
#   otherwise return true
#
#  Note is.na returns TRUE if the object has an NaN value
#       is.nan returns false if the object has an NA value
#
###################################################
is.valid.numeric <- function(x,na.valid = FALSE, nan.valid = FALSE, Inf.valid = FALSE)
{ # is.valid.numeric
  if ( !is.numeric(x) ) { cat("mode is not numeric\n"); return(FALSE) }
  if ( length(x)== 0 )  { cat("length equals 0\n"); return(FALSE) }
  if ( nan.valid == FALSE && any(is.nan(x)) ) { cat("NaN value(s) detected\n"); return(FALSE) }
  if ( na.valid  == FALSE && any(is.na(x)) )  { cat("NA value(s) detected\n"); return(FALSE) }
  if ( Inf.valid == TRUE ) { return(TRUE) }

#            If x has an NA value, then any(x == Inf) returns NA.
#            This implies that:  if ( any(x == Inf) ) { cat("Inf value(s) detected\n"); return(FALSE) }
#            will not work. We will get an error saying we need a TRUE/FALSE value in the if condition
#            Identical considerations apply to testing for -Inf. 
  if ( length(which(is.na(x))) > 0 )
    {
      x.rm.na = x[-which(is.na(x))]  # removes NA and NaN
    }
  else
    {
      x.rm.na = x
    }
  if ( any( x.rm.na == Inf) ) { cat("Inf value(s) detected\n"); return(FALSE) }
  if ( any( x.rm.na == -Inf) ) { cat("-Inf value(s) detected\n"); return(FALSE) }
  return(TRUE)
} # is.valid.numeric

