#! /usr/bin/Rscript 
# Project  : newProjTemplName
# File     : newProjTemplName/newProjTemplName.r
# Author   : Alex Zolotoviski
# Created  : 00-00-00
# License  : GPL-2
###############################################################################

{ #== init ===
	rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)
	
	options(help='html', digits=4, width=2160, scipen=5, editor='C:\\Program Files (x86)\\Notepad++\\notepad++.exe')
	options(error= NULL)  # options(error= recover) options(error=dump) 

	onWin= Sys.getenv('R_PLATFORM')==''
	if(onWin){root= 'm:'; 	memory.limit(size=24000)}  else root= '/home/zolotovi/work'   # memory.limit()
	
	library(HaLaP)
	library(WorkJournal)
	
	if(fromSrc <- 1) source('m:/50_HLP/out/packages/HaLaP/inst/rcode/HLP.r') # T:/work/UseR-2013/lib/zBase0.r
	if(fromSrc <- 1) source('m:/50_HLP/out/packages/RWorkJournal/inst/rcode/RWJ.r') # T:/work/UseR-2013/lib/zCode.r

	
	#source(file.path(root, 'lib/zBase0.r'))  
	source('T:/work/UseR-2013/lib/zBase1.r')   # xxx: hard coded path to zBase.r
	source('T:/work/UseR-2013/lib/zCode.r')  
	source('T:/work/UseR-2013/lib/zStats.r')  
	source('M:/newProjTemplName/newProjTemplName.fun.r')  
	
	libra(plyr) 
	libra(data.table)
	libra(SOAR); srm= Store # Ls(); Attach(); Search(); loo(); vignette("SOAR") 
	#libra(R2HTML) 
	#libra(XLConnect)
	
	libra(RColorBrewer)  # display.brewer.all()
	palette(c(adjustcolor(cn('grey50 blue3 red2'), alpha.f = .6), brewer.pal(8,"Dark2")))  # plot(1:19, pch=16, col=1:19, cex=3)
	
	#libra(Defaults)
	#	setDefaults(legend, bty='n')
	#	setDefaults(symbols, inc=.15)
	#libra(randomForest)
	#libra(plotrix)  # addtable2plot
	
	#libra(ggplot2)
	#libra(scales)
	#libra(RJDBC)     # for hive_conn
	
	proot= fp(root, 'newProjTemplName')  # project root
	sw(fp(proot, 'out'))
	
	#libra(Cairo);  
		
	theFile= fp(proot, 'newProjTemplName.r')
	
	sg.bak= sg    # ; sg=dummy ; # sa= sa.bak   # to rerun w/o change images
	sgg.bak= sgg  # ; sgg=dummy; # sa= sa.bak   # to rerun w/o change images
	sa.bak= sa    # ; sa=dummy ; # sa= sa.bak   # to rerun w/o change images
	saa.bak= saa  # ; saa=dummy; # sa= sa.bak   # to rerun w/o change images

	# rmDF(); gff('saved'); loo(); lo(); lsDF(); dett(); gw(); tables(); loo('xx'); loo('evs')
} #--
##########################################################
'
#   newProjTemplName  
'

if(0){#==  Data Inventory  ==
	gw() 
	dir() # expl() 
}

if(0){#==  Data Exploration  ==
}

if(0){#==  Predictive Modeling  ==
}

if(0){#==  Reports  ==
}


if(0){#=== Misc
	theFile= fp(proot, 'newProjTemplName.r')
	theFile= 'm:/newProjTemplName/newProjTemplName.r'
	gff('saved', theFile)
	gff('sa\\(|===', theFile)
	
	CreateNewProj(newProj.name= '___zzz', Templ.dir= 'M:/50_HLP/out/packages/RWorkJournal/inst/newProjTemplName', root='m:')
} #--
