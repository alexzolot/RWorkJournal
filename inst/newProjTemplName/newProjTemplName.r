#! /usr/bin/Rscript
# Project  : newProjTemplName
# File     : newProjTemplName/newProjTemplName.r
# Author   : Alex Zolotoviski, alex@zolot.us
# Created  : 00-00-00
# License  : GPL-2
###############################################################################

{ #== init ===
	rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)
	
	options(help='html', digits=4, width=2160, scipen=5, editor='C:\\Program Files (x86)\\Notepad++\\notepad++.exe')
	options(error= NULL) # options(error= recover) options(error=dump)
	options(datatable.print.nrows=200)


	onWin= Sys.getenv('R_PLATFORM')==''
	if(onWin){root= 'M:'; 	memory.limit(size=9000)}  else root= '/home/azolotovitski/work'   # memory.limit()
	
	library(HaLaP)  # it loads libras SOAR, data.table
	library(RWorkJournal)
	
	#source(file.path(root, 'lib/zBase0.r'))  
	source('T:/work/UseR-2013/lib/zBase0.r')  # or hard coded paths to source files
	source('T:/work/UseR-2013/lib/zBase1.r')  
	source('T:/work/UseR-2013/lib/zCode.r')  
	source('T:/work/UseR-2013/lib/zStats.r')  
	source('T:/work/UseR-2013/newProjTemplName/newProjTemplName.fun.r')  
	
	libra(plyr) 
	#libra(R2HTML) 
	#libra(XLConnect)
	
	libra(RColorBrewer)  # display.brewer.all()
	palette(c(adjustcolor(cn('grey50 green3 red2'), alpha.f = .6), brewer.pal(8,"Dark2")))  ##ex: plot(1:19, pch=16, col=1:19, cex=3)
	
	#libra(Defaults)
	#	setDefaults(legend, bty='n')
	#	setDefaults(symbols, inc=.15)
	#libra(randomForest)
	#libra(plotrix)  # addtable2plot
	#libra(Cairo);  

	
	#libra(ggplot2)
	#libra(scales)
	#libra(RJDBC)     # for hive_conn
	
	proot= fp(root, 'newProjTemplName')  # project root
	sw(fp(proot, 'out'))
	
		
	theFile= fp(proot, 'newProjTemplName.r')
	
        options(theFile= theFile)
	sg.bak= sg    # ; sg=dummy ; # sa= sa.bak   # to rerun w/o change images
	sgg.bak= sgg  # ; sgg=dummy; # sa= sa.bak   # to rerun w/o change images
	sa.bak= sa    # ; sa=dummy ; # sa= sa.bak   # to rerun w/o change images
	saa.bak= saa  # ; saa=dummy; # sa= sa.bak   # to rerun w/o change images
					
	# rmDF(); gff('saved'); loo(); lo(); lsDF(); dett(); gw(); taa(); loo()
} #--
##########################################################

#   newProjTemplName  

if(0){###  Data Inventory  ==
	gw() 
	dir() # expl() 
}

if(0){###  Data Exploration   ==
}

if(0){###  Predictive Modeling  ==
}

if(0){###  Reports  ==
}


if(0){###  Misc
	theFile= 'm:/newProjTemplName/newProjTemplName.r'
	gff('saved', theFile)
	gff('sa\\(|===', theFile)
	
	theFile= fp(proot, 'newProjTemplName.r')
	ccc= function()code2HTML(theFile)
	
	CreateNewProj(newProj.name= 'zzz', Templ.dir= 'M:/lib/newProjTemplNam e', root='M:')  # Fix "Nam e" !!!
} #--
