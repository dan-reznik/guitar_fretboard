semitone <- 2^(1/12)
middleC <- 440/(2^(9/12))

getRoots <- function(n,jump) accumulate(0:(n-1),~(.x+jump-1)%%n,.init=0)+1

# create a series 1, f0^1, f0^2, ... f0^n ,such that all terms lie within (1,2)
getFreqSeries <- function(n,f0)
  accumulate(1:n,
             ~{f<-.x*f0;if(f>2.01) f/2 else f},
             .init=1)

getScale <- function(root,notes,scaleIdx) {
  scale <- notes[(root+which(scaleIdx)[-8]-2)%%12+1]
  str_c(scale,collapse=",")
}

# these are extracting in the order they appear on the scale,
# not in the order sharps (or flats) are introduced by 5ths (or 4ths)
clean_chr0 <- function(s) if(length(s)==0) "" else s
getBasic <- function(s,regex) s %>%
  str_extract_all(regex)%>%
  map(clean_chr0)%>%
  map_chr(str_c,collapse=",")
getSharps <- function(s) s%>%getBasic(".#")
getFlats <- function(s) s%>%getBasic(".b")