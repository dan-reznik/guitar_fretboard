# (C) 2017 Dan S. Reznik
library(tidyverse)
library(stringr)

semitone <- 2^(1/12)
middleC <- 440/(2^(9/12))

getRoots <- function(n,jump) accumulate(0:(n-1),~(.x+jump-1)%%n,.init=0)+1

# create a series 1, f0^1, f0^2, ... f0^n ,such that all terms lie within (1,2)
getFreqSeries <- function(n,f0)
  accumulate(1:n,
             function(x,y) {
               f<-x*f0
               if_else(f>2.01,f/2,f)
             },.init=1)

getScale <- function(root,notes,scaleIdx) {
  scale <- notes[(root+which(scaleIdx)[-8]-2)%%12+1]
  paste0(scale,collapse=",")
}

# these are extracting in the order they appear on the scale,
# not in the order sharps (or flats) are introduced by 5ths (or 4ths)
getSharps <- function(scale) paste0(unlist(str_extract_all(scale,".#")),collapse=",")
getFlats <- function(scale) paste0(unlist(str_extract_all(scale,".b")),collapse=",")

# Creates a "circle of fifths" data frame

tones12 <- tibble(degree=1:13,
                  notes=c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B","C2"),
                  notesF=c("C","Db","D","Eb","E","F","Gb","G","Ab","A","Bb","B","C2"),
                  majorIdx=c(T,F,T,F,T,T,F,T,F,T,F,T,T),
                  minorIdx=c(T,F,T,T,F,T,F,T,T,F,T,F,T),
                  freqMult=accumulate(degree[-13],~.x*semitone,.init=1),
                  freq=middleC*freqMult,
                  # 5ths
                  roots5ths=getRoots(12,8), # P5 is 8th note
                  notes5ths=notes[roots5ths],
                  freq5ths=getFreqSeries(12,freqMult[8]),
                  majorScale5ths=map_chr(roots5ths,~getScale(.x,notes,majorIdx)),
                  majorSharps5ths=map_chr(majorScale5ths,~getSharps(.x)),
                  minorScale5ths=map_chr(roots5ths,~getScale(.x,notes,minorIdx)),   
                  minorSharps5ths=map_chr(minorScale5ths,~getSharps(.x)),
                  # 4ths
                  roots4ths=getRoots(12,6), # P4 is 6th note
                  notes4ths=notesF[roots4ths],
                  freq4ths=getFreqSeries(12,freqMult[6]),
                  majorScale4ths=map_chr(roots4ths,~getScale(.x,notesF,majorIdx)),
                  majorFlats4ths=map_chr(majorScale4ths,~getFlats(.x)),
                  minorScale4ths=map_chr(roots4ths,~getScale(.x,notesF,minorIdx)),
                  minorFlats4ths=map_chr(minorScale4ths,~getFlats(.x))
)

# Draw fingerboard

fretDist <- function(fretN) 1 / (2^(fretN/12))
fretDists <- fretDist(0:24)
fw2 <- .1
stringYs <- seq(fw2,-fw2,-2*fw2/5)
fretBoard <-
  tibble(stringYs=stringYs) %>%
  ggplot() +
  # fingerboard (light wood)
  geom_rect(xmin=0,xmax=1,
            ymin=-1.1*fw2,ymax=1.1*fw2,
            color="#663300",fill="#663300") +
  # bridge (dark wood)
  geom_rect(xmin=-.02,xmax=0.02,
            ymin=-1.2*fw2,ymax=1.2*fw2,
            color="#330000",fill="#330000") +
  # nut (motherpearl)
  geom_rect(xmin=1,xmax=1.02,
            ymin=-1.2*fw2,ymax=1.2*fw2,
            color="#888888",fill="#888888") +
  # frets (black)
  geom_segment(aes(x=fretDists,xend=fretDists,
                   y=-1.1*fw2,yend=1.1*fw2),
               data=tibble(fretDists=fretDists)) +
  # marker dots (white)
  geom_point(aes(x=.5*(dotx+dotxN),y=0),
             data=tibble(dotx=fretDists[c(3,5,7,9)],
                         dotxN=fretDists[c(4,6,8,10)]),
             size=2,color="white") +
  geom_point(aes(x=dotx,y=doty),
             data=tibble(dotx=rep(.5*(fretDists[12]+fretDists[13]),2),
                         doty=c(-fw2/12,fw2/12)),
             size=2,color="white") +
  # strings
  geom_segment(aes(y=stringYs,yend=stringYs,
                   x=0,xend=1.02),color="grey") +
  # bridge pegs (blue)
  geom_point(aes(x=0,y=stringYs),size=4,color="blue") +
  coord_fixed(xlim=c(0,1.05),ylim=1.1*c(-fw2,fw2)) +
  labs(x="",y="",title="")

# Where are the naturals

tuning6 <- c("E","A","D","G","B","E")
# where in the notes vector
tuning6idx <- map_int(tuning6,~match(.x,tones12$notes[-13]))
naturalfretsCroot <- unlist(map((0:3)*12,~.x+which(tones12$majorIdx[-13])))-1

naturalfingerPos <-
  map(tuning6idx-1,~as.integer(keep(naturalfretsCroot-.x,
                                    function(y)y>0&y<=24)))
naturalfingerRel <-
  map2(tuning6idx,naturalfingerPos,~(.x+.y-1)%%12+1)
naturalfingerNotes <- map(naturalfingerRel,~tones12$notes[.x])

fretDistsMid <- map2_dbl(fretDists[-length(fretDists)],
                         fretDists[-1],~.5*(.x+.y))
drawNotes <- function(p,s) {
  nfp <- naturalfingerPos[[s]]
  nfn <- naturalfingerNotes[[s]]
  df <- data_frame(nfp=nfp,
                   x=fretDistsMid[nfp],
                   y=stringYs[s],
                   nfn=nfn)
  p+
    geom_point(aes(x=x,y=y),size=7,color="black",data=df) +
    geom_text(aes(x=x,y=y,label=nfn),color="white",
              size=4,data=df)
}

# open notes
df0 <- data_frame(x=1.05,y=stringYs,nfn=tuning6)
# draw entire fretboard.
# loop doesn't work because drawNotes requires previous plot
reduce(1:6,~drawNotes(.x,.y),.init=fretBoard) +
  geom_point(aes(x=x,y=y),size=7,color="grey",data=df0) +
  geom_text(aes(x=x,y=y,label=nfn),color="black",
            size=4,data=df0)
