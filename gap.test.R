##########################################
#
#    gap test for random number generator
#
#
#    x is the vector to be tested for randomness
#    a and b are numbers in the support of the pdf from which
#      x is generated
#    p is the P(a <= x <= b) for each element of x
#   count runs of x NOT in the interval [a,b]
#   if x is random uniform over (0,1), then use
#        the default for p
#
gap.test <- function(x,a,b,p = b - a)
{ # begin gap.test

  stopifnot( is.numeric(x) & is.vector(x) )
  stopifnot( is.numeric(a) & length(a) == 1 )
  stopifnot( is.numeric(b) & length(b) == 1 )
  stopifnot( b > a )
  stopifnot( is.numeric(p) & (length(p) == 1) & (p > 0) & (p < 1) )

#   compute number of entries for counting gaps.
#   the vector gaps will hold the gaps count.
#   gap[i] = N means there were N sequences with a gap
#   of (i-1). Recall, gaps of length 0 cannot be stored in
#   gaps[0]; they must be in gaps[1]
#   We aggregate gaps greater than (g-1)
#   browser()
  s = 1:20  # max number of gap counts is 20
  t = (1-p)^s
  t = t[t > 0.05] # first entry chopped off is less than 0.05
                  # NOTE: should I make 0.05 a parameter?
  g = length(t) 
  gaps = numeric(g)

  in.gap = which( (x >=a) & (x <= b) )
  in.gap = c(0,in.gap)  
  m      = length(in.gap) 
  runs   = in.gap[2:m] - in.gap[1:(m-1)]
  for ( i in 1:(g-1))
    {
      gaps[i] = length(runs[runs == i])      
    }
  gaps[g] = length(runs[ runs >= g])

#      if last uncompleted gap is long, count it
#      in the last gaps vector entry
#      Set n to the number of
  if ( length(x) - in.gap[m] > (g-1) )
    { 
       gaps[g] = gaps[g] + 1
       n       = m - 1
    }
  else
    {
       n = m - 2
    }       

#  do the chi squared test. We have n random variables
#  which are geomtrically distributed under the null hypothesis

#          find the expected number of the geomtric random variables

  expected = numeric(g)
  for ( i in 1:(g-1) )
    {
      p.c = p*(1-p)^(i-1)
      expected[i] = n*p.c
    }
  expected[g] = n*(1-p)^(g-1)
  print("minimum expected value is")
  print(min(expected))
#                                         browser()
  ts = (gaps - expected)^2/expected
  ts = sum(ts)
  return(1 - pchisq(ts,g-1))
  
} # end gap.test