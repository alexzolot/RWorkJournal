RWorkJournal
============

R Work Journal

R code for a large project often contains many tasks  amd may have 500-3000 lines of code that make difficult navigation through the code. Some tasks in a project could have long execution time that makes difficult also usual  literate programming methods (as `sweave`, `knitr`).

We wrote R function `Code2RWorkJournal()` that

1.  Transforms  `.R` file into  self-documented  `.html` file, containing all R code with output pics, headers, table of contents and gallery. 
2.  The titles in body and contents are clickable to navigate from contents to body and back. 
3.  The pics are clickable to resize. 
4.  The html file has partly R syntax highlighted.  It is possible to do the full R syntax highlighting in resulting html, but the result file becomes almost twice heavier. 
5.  Parts of the result html file could be folded. 
6.  If in a browser you “select all”, copy and paste from browser to a text editor,   you get the pure original R file. 
7.  If we modify `.R` code,   recreate `.html` is fast. 


## In rel.02

1. Full syntax highlight
2. Added line numbers
3. Use knitr::knit in the middle
4. Added support for d3 js plots
5. In addition to png - pdf plots, open on dblclk
6. Dropped not-standard md #===, only standard  ####
