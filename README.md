Guitar Fretboard
================

``` r
library(tidyverse)
library(purrr)
source("scale_util.R")
```

### Creates a "circle of fifths" data frame

``` r
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
                  majorScale5ths=map_chr(roots5ths,getScale,notes,majorIdx),
                  majorSharps5ths=map_chr(majorScale5ths,getSharps),
                  minorScale5ths=map_chr(roots5ths,getScale,notes,minorIdx),   
                  minorSharps5ths=map_chr(minorScale5ths,getSharps),
                  # 4ths
                  roots4ths=getRoots(12,6), # P4 is 6th note
                  notes4ths=notesF[roots4ths],
                  freq4ths=getFreqSeries(12,freqMult[6]),
                  majorScale4ths=map_chr(roots4ths,getScale,notesF,majorIdx),
                  majorFlats4ths=map_chr(majorScale4ths,getFlats),
                  minorScale4ths=map_chr(roots4ths,getScale,notesF,minorIdx),
                  minorFlats4ths=map_chr(minorScale4ths,getFlats)
)
```

### Show: Circle of 5ths' scales

``` r
tones12 %>%
  select(degree,
         Scale=notes5ths,
         majorScale5ths,majorSharps5ths,
         minorScale5ths,minorSharps5ths) %>%
  knitr::kable()
```

|  degree| Scale | majorScale5ths          | majorSharps5ths     | minorScale5ths          | minorSharps5ths     |
|-------:|:------|:------------------------|:--------------------|:------------------------|:--------------------|
|       1| C     | C,D,E,F,G,A,B           |                     | C,D,D\#,F,G,G\#,A\#     | D\#,G\#,A\#         |
|       2| G     | G,A,B,C,D,E,F\#         | F\#                 | G,A,A\#,C,D,D\#,F       | A\#,D\#             |
|       3| D     | D,E,F\#,G,A,B,C\#       | F\#,C\#             | D,E,F,G,A,A\#,C         | A\#                 |
|       4| A     | A,B,C\#,D,E,F\#,G\#     | C\#,F\#,G\#         | A,B,C,D,E,F,G           |                     |
|       5| E     | E,F\#,G\#,A,B,C\#,D\#   | F\#,G\#,C\#,D\#     | E,F\#,G,A,B,C,D         | F\#                 |
|       6| B     | B,C\#,D\#,E,F\#,G\#,A\# | C\#,D\#,F\#,G\#,A\# | B,C\#,D,E,F\#,G,A       | C\#,F\#             |
|       7| F\#   | F\#,G\#,A\#,B,C\#,D\#,F | F\#,G\#,A\#,C\#,D\# | F\#,G\#,A,B,C\#,D,E     | F\#,G\#,C\#         |
|       8| C\#   | C\#,D\#,F,F\#,G\#,A\#,C | C\#,D\#,F\#,G\#,A\# | C\#,D\#,E,F\#,G\#,A,B   | C\#,D\#,F\#,G\#     |
|       9| G\#   | G\#,A\#,C,C\#,D\#,F,G   | G\#,A\#,C\#,D\#     | G\#,A\#,B,C\#,D\#,E,F\# | G\#,A\#,C\#,D\#,F\# |
|      10| D\#   | D\#,F,G,G\#,A\#,C,D     | D\#,G\#,A\#         | D\#,F,F\#,G\#,A\#,B,C\# | D\#,F\#,G\#,A\#,C\# |
|      11| A\#   | A\#,C,D,D\#,F,G,A       | A\#,D\#             | A\#,C,C\#,D\#,F,F\#,G\# | A\#,C\#,D\#,F\#,G\# |
|      12| F     | F,G,A,A\#,C,D,E         | A\#                 | F,G,G\#,A\#,C,C\#,D\#   | G\#,A\#,C\#,D\#     |
|      13| C     | C,D,E,F,G,A,B           |                     | C,D,D\#,F,G,G\#,A\#     | D\#,G\#,A\#         |

### Show: circle of 4ths' scales

``` r
tones12 %>%
  select(degree,
         Scale=notes4ths,
         majorScale4ths,majorFlats4ths,
         minorScale4ths,minorFlats4ths) %>%
  knitr::kable()
```

|  degree| Scale | majorScale4ths     | majorFlats4ths | minorScale4ths     | minorFlats4ths |
|-------:|:------|:-------------------|:---------------|:-------------------|:---------------|
|       1| C     | C,D,E,F,G,A,B      |                | C,D,Eb,F,G,Ab,Bb   | Eb,Ab,Bb       |
|       2| F     | F,G,A,Bb,C,D,E     | Bb             | F,G,Ab,Bb,C,Db,Eb  | Ab,Bb,Db,Eb    |
|       3| Bb    | Bb,C,D,Eb,F,G,A    | Bb,Eb          | Bb,C,Db,Eb,F,Gb,Ab | Bb,Db,Eb,Gb,Ab |
|       4| Eb    | Eb,F,G,Ab,Bb,C,D   | Eb,Ab,Bb       | Eb,F,Gb,Ab,Bb,B,Db | Eb,Gb,Ab,Bb,Db |
|       5| Ab    | Ab,Bb,C,Db,Eb,F,G  | Ab,Bb,Db,Eb    | Ab,Bb,B,Db,Eb,E,Gb | Ab,Bb,Db,Eb,Gb |
|       6| Db    | Db,Eb,F,Gb,Ab,Bb,C | Db,Eb,Gb,Ab,Bb | Db,Eb,E,Gb,Ab,A,B  | Db,Eb,Gb,Ab    |
|       7| Gb    | Gb,Ab,Bb,B,Db,Eb,F | Gb,Ab,Bb,Db,Eb | Gb,Ab,A,B,Db,D,E   | Gb,Ab,Db       |
|       8| B     | B,Db,Eb,E,Gb,Ab,Bb | Db,Eb,Gb,Ab,Bb | B,Db,D,E,Gb,G,A    | Db,Gb          |
|       9| E     | E,Gb,Ab,A,B,Db,Eb  | Gb,Ab,Db,Eb    | E,Gb,G,A,B,C,D     | Gb             |
|      10| A     | A,B,Db,D,E,Gb,Ab   | Db,Gb,Ab       | A,B,C,D,E,F,G      |                |
|      11| D     | D,E,Gb,G,A,B,Db    | Gb,Db          | D,E,F,G,A,Bb,C     | Bb             |
|      12| G     | G,A,B,C,D,E,Gb     | Gb             | G,A,Bb,C,D,Eb,F    | Bb,Eb          |
|      13| C     | C,D,E,F,G,A,B      |                | C,D,Eb,F,G,Ab,Bb   | Eb,Ab,Bb       |

### Draw fretboard

``` r
fretDist <- function(fretN) 1 / (2^(fretN/12))
draw_fretboard <- function(tones12,fw2=.1) {
  fretDists <- fretDist(0:24)
  stringYs <- seq(fw2,-fw2,-2*fw2/5)
  tibble(stringYs=stringYs) %>%
    ggplot() +
    # fretboard (light wood)
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
    labs(x="",y="",title="") +
    theme(axis.text=element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm"),
          panel.spacing=unit(c(0,0,0,0), "cm"))
}

draw_fretboard(tones12_5ths_and_4ths)
```

![](README_files/figure-markdown_github/unnamed-chunk-6-1.png)

### Draw fretboard w/ naturals

``` r
draw_naturals <- function(tones12,fw2=.1) {
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
  fretDists <- fretDist(0:24)
  stringYs <- seq(fw2,-fw2,-2*fw2/5)
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
      geom_point(aes(x=x,y=y),size=6,color="black",data=df) +
      geom_text(aes(x=x,y=y,label=nfn),color="white",
                size=3,data=df)
  }

  fretboard <- draw_fretboard(tones12,fw2)
#open notes
  df0 <- data_frame(x=1.05,y=stringYs,nfn=tuning6)
  # draw entire fretboard.
  # loop doesn't work because drawNotes requires previous plot
  reduce(1:6,~drawNotes(.x,.y),.init=fretboard) +
    geom_point(aes(x=x,y=y),size=4,color="grey",data=df0) +
    geom_text(aes(x=x,y=y,label=nfn),color="black",
              size=1,data=df0) +
    theme(axis.text=element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          plot.margin=unit(c(0,0,0,0), "cm"),
          panel.spacing=unit(c(0,0,0,0), "cm"))
}
```

``` r
draw_naturals(tones12)
#> Warning: `data_frame()` is deprecated, use `tibble()`.
#> This warning is displayed once per session.
```

![](README_files/figure-markdown_github/unnamed-chunk-8-1.png)