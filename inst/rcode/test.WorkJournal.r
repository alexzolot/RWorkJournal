
options(datatable.print.nrows=100)
libra(data.table); dtt= data.table; ad= as.IDate; taa= tables	# vignette("datatable-faq")
libra(knitr)
libra(markdown)

'
#view=Fit does not work in Chrome:
<br/>
<object data="img/Fig_9.pdf#view=Fit" type="application/pdf" width="300" height="200">
  alt : <a href="img/Fig_9.pdf">Fig_9.pdf</a>
</object>'


if (0) {
	
#' side effect: creates .rmd and .kn.htm  files
	code2spin= function(.file= get.theFile(), s= readLines(.file, warn= F), toTempDir=F, show=T) { catf('\ncode2rmd("%s"): \n', .file)  #, withLineNum=T
		#	code2rmd= function(.file= get.theFile(), s= readLines(.file, warn= F), toTempDir=T, show=T) { catf('\ncode2rmd("%s"): \n', .file)  #, withLineNum=T
		stopifnot(require(knitr))
		stopifnot(require(markdown))
		
		sing.quo= regexpr('^\\s*([\'"`])\\s*$', s)
		q1= gre2('^\\s*[\'"`]', '^\\s*([\'"`]).*\\1', s, v=F)  # line start quote
		q2= gre2('[\'"`]\\s*$', "(['\"`]).*\\1\\s*$|#'\\s*$", s, v=F)  # line end quote, not roxygen
		
		ich= cumsum(q2 | q1)  # chunk index
		iich= ich - ifelse(ich%%2, 0, c(0, diff(ich)))  # chunk index, from 0; 0 is.code
		is.code= 1- iich%%2
		
		ss= dtt(i= 1:le(s), q1, q2, ich, iich, is.code, s, s2=s)  #, q2a, q2b
		
		dout= if(toTempDir) tempdir() else dirname(theFile)
		fout= fp(dout, basename(theFile))
		
		#' s2 - code for .rwd
		if(0){
			ss[(q1 | q2) & ich%%2==0,  s2:=  sf("%s\n\n```{r %s}", s, i)]
			ss[(q1 | q2) & ich%%2==1,  s2:=  sf("```\n%s",  s)]
			ss[i==1      & ich%%2==0,  s2:=  sf("\n```{r 1}\n%s", s)]
			ss[i==le(s)  & ich%%2==0,  s2:=  sf("%s\n```",  s)]
			ss[,  s2:=  gsub('(^|[^x"])@', '\\1\\\\', gsub('@@', '\\\\\\\\', s2))]  # escapes for LaTeX @ -> \
			#wl(ss$s2, .file %+% '.rmd')
		}
		
		rmd= ss$s2
		
		#if(toTempDir) {dout=tempdir();   .file= fp(dout, basename(theFile))} else dout= dirname(theFile)
		
		
		wl(rmd,  fout %+% '.rmd', F)
		
		#' s3 - code for .rwd with line numbers comments in r chunks 
		# insert comments with line numbers in non-empty R chunks
		ss[, s3:=s2]
		ss[is.code & grepl('\\S', s), s3:= s3 %+% sf(" # line %s#", i)]  
		
		wl(ss$s3,  fout %+% '.1.htm',   sep='</br>')
		
		
		#' insert roxygen comments in md chunks
		ss[is.code==0, s3:= "#' " %+% s3 ]  
		
		
		wl(ss$s3,  fout %+% '.2.htm',   sep='</br>')
		
		
		#' transf headers to roxygen comments in r chunks
		ss[is.code==1 ,  s3:= sub('(.*#)(=+)', "\\1' \\2", s3)]  
		
		ss[is.code==1 ,  s3:= sub('(.*?#)(#+)', "\\1' \\2", s3)]  
		ss[is.code==1 ,  s3:= sub('(.*#)( j?Fig)', "\\1' \\2", s3)]  
		
		rmd= forspin= ss$s3
		wl(forspin, fout %+% '.spin.r')
		
		
		opts_chunk$set(eval=F)
		spi= spin(, text= rmd) 
		wl(spi, fout %+% '.spi.htm')
		catn(spi)
		
		kn= knit(, text= spi) 
		#			markdownToHTML(file, output, text,
		#					options = getOption("markdown.HTML.options"),
		#					extensions = getOption("markdown.extensions"), title = "",
		#					stylesheet = getOption("markdown.HTML.stylesheet"),
		#					header = getOption("markdown.HTML.header"),
		#					template = getOption("markdown.HTML.template"), fragment.only = FALSE,
		#					encoding = getOption("encoding"))
		#			expl(getOption("markdown.HTML.header"))
		#			expl(getOption("markdown.HTML.template"))
		knit2html(,'.spikn.htm', text=spi, options = cn('mathjax highlight_code toc'))
		expl('.spikn.htm')
		
		
		opts_chunk$set(eval=F)
		kn= knit(, text= rmd) 
		#			markdownToHTML(file, output, text,
		#					options = getOption("markdown.HTML.options"),
		#					extensions = getOption("markdown.extensions"), title = "",
		#					stylesheet = getOption("markdown.HTML.stylesheet"),
		#					header = getOption("markdown.HTML.header"),
		#					template = getOption("markdown.HTML.template"), fragment.only = FALSE,
		#					encoding = getOption("encoding"))
		#			expl(getOption("markdown.HTML.header"))
		#			expl(getOption("markdown.HTML.template"))
		
		#knit.htm= markdownToHTML(fout %+% outSuffix, text= kn, fragment.only=F, out=fout %+% '.kn.htm')
		knit.htm= markdownToHTML(fout %+% '.rmd', text= kn, fragment.only=F, out=fout %+% '.kn.htm'
				, options = cn('mathjax highlight_code toc'))  #, extensions= cn('no_intra_emphasis latex_math')  'base64_images'
		expl(fout %+% '.kn.htm')
		
		
		if (1) { #' with line numbers - bad for \] LaTeX
			# grep('\\\\', '\\')
			#s3= ifelse(grepl('(^\\s*$|```|^\\s*\\\\|\\\\\\[|\\\\\\]|= *\\\\)', ss$s2), sf('%s <!--%s-->', ss$s2, 1:le(ss$s2)), sf('%s #zz#%s#', ss$s2, 1:le(ss$s2)))
			s3= ifelse(grepl('(^\\s*$|```|^\\s*\\\\|\\\\\\[|\\\\\\]|= *\\\\)', ss$s2), ss$s2, sf('%s #zz#%s#', ss$s2, 1:le(ss$s2)))
			s3= ss$s3	# prr(ss$s3)
			rmd.nu= c(s3)  #; strr(rmd.nu); prr(rmd.nu)
			wl(rmd.nu,  fout %+% '.nu.rmd', F)
			kn.nu= knit(, text= rmd.nu) 
			kn.nu.htm= markdownToHTML(fout %+% '.nu.rmd', text= kn.nu, fragment.only=F, out=fout %+% '.kn.nu.htm'
					, options = cn('mathjax highlight_code'))  #, extensions= cn('no_intra_emphasis latex_math')
			expl(fout %+% '.kn.nu.htm')
		}
		
		if(show) expl(dout)
		
		# out.kn.htm= sub('$', '.kn.htm', fout)
		invisible(list(file= .file, dout= dout, fout= fout, rmd=rmd, lines=ss, out.rmd= fout %+% '.rmd'
						, out.kn.htm= fout %+% '.kn.htm', out.kn.nu.htm= fout %+% '.kn.nu.htm'))
	}
#e rmd= code2spin()
#e rmd= code2rmd(toTempDir =F)
	
	
}


nope= function() { 
	
	
	
	main.r2htm= function(s1, img='img') { catf('\nmain.r2htm: %s\n', le(s1)); strr(s1)
		#if(s1=='') brr()
		s1= gsub('<(\\s)', '&lt;\\1', s1)  # we suppose no blanks after "<" in <tag    in the input R code
		s1= replaceTagsOutSq(s1)  # we suppose  <tag>  only in `` in the  R code
		s1= gsub('@@', '\\\\\\\\', s1)  # drop escapes for LaTeX
		s1= gsub('(^|[^x"])@', '\\1\\\\', s1)  # drop escapes for LaTeX
		
		figss= list()
		
		#==  Set id  ==
		for(i in 1:le(s1)){
			if(grepl('^\\s*#\\s* (Pic|Fig)_\\d+', s1[i])) figss[[ch(i)]]= s1[i]
			
			s1[i]= gsub('^([^\\~]*# *"?)(jFig_\\d+)(.*)'
					, '\n\n <iframe src="img/\\2.htm" width="100%" height="600px"></iframe>
							\n\n \\1\\2\\3' , s1[i])
			
			
			s1[i]= gsub('^([^\\~]*# *"?)(Pic_\\d+)(.*)'
					, sf('<br/><img id="%s" class="fig" src="%s/\\2.png" width=700 
									onClick="resize(%1$s);" ondblclick="ShowImg(\'\\2\', \'\\3\');" 
									name="\\2\\3"/>   
									<br/><span class="capt" onClick="goto(\'tnTOC%1$s\');" 
									ondblclick="ShowImg(\'\\2\', \'\\3\')">\\1\\2\\3</span><br/>'
							,  i, img), s1[i])
			s1[i]= gsub('^([^\\~]*# *"?)(Fig_\\d+)(.*)'
					, sf('<br/><img id="%s" class="fig" src="%s/\\2.png" width=700 
									onClick="resize(%1$s);" ondblclick="ShowImg(\'\\2\', \'\\3\');" 
									name="\\2\\3"/>   
									<br/><span class="capt" onClick="goto(\'tnTOC%1$s\');" ondblclick="ShowImg(\'\\2\', \'\\3\')">\\1\\2\\3</span><br/>',  i, img), s1[i])
			#		s1[i]= gsub('(.*\\s*#==== )(.*)( =+.*)$', sf('</pre>\\1<span class="H5" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
#		s1[i]= gsub('(.*\\s*#=== )(.*)( =+.*)$',  sf('</pre>\\1<span class="H4" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
#		s1[i]= gsub('(.*\\s*#== )(.*)( =+.*)$' ,  sf('</pre>\\1<span class="H3" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
#		s1[i]= gsub('(.*\\s*#= )(.*)( =+.*)$'  ,  sf('</pre>\\1<span class="H2" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
#		s1[i]= gsub('(.*\\s*#=+)([^=]*)$'      ,  sf('</pre>\\1<span class="H1" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
			
			s1[i]= gsub('(.*\\s*#===== )(.*)( =+.*)$', sf('</pre>\\1<span class="H5" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
			s1[i]= gsub('(.*\\s*#==== )(.*)( =+.*)$',  sf('</pre>\\1<span class="H4" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
			s1[i]= gsub('(.*\\s*#=== )(.*)( =+.*)$' ,  sf('</pre>\\1<span class="H3" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
			s1[i]= gsub('(.*\\s*#== )(.*)( =+.*)$'  ,  sf('</pre>\\1<span class="H2" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
			s1[i]= gsub('(.*\\s*#= )(.*)( =+.*)?$'  ,  sf('</pre>\\1<span class="H1" id="%s" onClick="goto(\'tn%s\');">\\2</span> <span class="comment2">\\3</span><pre>',i,i),  s1[i])
			
			s1[i]= gsub('(^[^#A-a0-9\\.\\,]*###### )(.*)', sf('</pre><span class="comment2">\\1</span><span class="H5" id="%s" onClick="goto(\'tn%1$s\');">\\2</span> <br/><pre>',i),  s1[i])
			s1[i]= gsub('(^[^#A-a0-9\\.\\,]*##### )(.*)' , sf('</pre><span class="comment2">\\1</span><span class="H4" id="%s" onClick="goto(\'tn%1$s\');">\\2</span> <br/><pre>',i),  s1[i])
			s1[i]= gsub('(^[^#A-a0-9\\.\\,]*#### )(.*)'  , sf('</pre><span class="comment2">\\1</span><span class="H3" id="%s" onClick="goto(\'tn%1$s\');">\\2</span> <br/><pre>',i),  s1[i])
			s1[i]= gsub('(^[^#A-a0-9\\.\\,]*### )(.*)'   , sf('</pre><span class="comment2">\\1</span><span class="H2" id="%s" onClick="goto(\'tn%1$s\');">\\2</span> <br/><pre>',i),  s1[i])
			s1[i]= gsub('(^[^#A-a0-9\\.\\,]*## )(.*)'    , sf('</pre><span class="comment2">\\1</span><span class="H1" id="%s" onClick="goto(\'tn%1$s\');">\\2</span> <br/><pre>',i),  s1[i])
			
			s1[i]= gsub('id="(\\d+)" ', 'id="\\1" title="line \\1" ',  s1[i])
		}	
		
		#= div for code folding ===
		depth= countDepth2(s1)
		imgFold= '<img src=\'https://ui.netbeans.org/docs/ui/code_folding/cf_minus.gif\' alt="-"/>'
		
		s1= ifelse(depth!= 0, gsub('^([^#]*\\{[^\\{\\}]*)', sf('\\1 <a href="javascript:ToggleFold(\'D-fold\')" class="aD">%s</a><div class="D-fold">', imgFold), s1), s1)
		
		s1= gsub('^(.* class="H(\\d)".* id="(\\d+).*)D-fold(.*) class="D-fold"' , '\\1D\\3\\4 class="D\\2" id="D\\3"', s1)
		s1= ifelse(depth!= 0, gsub('([^#\\{\\}]*)\\}', '\\1</div><b>}</b></pre><pre>', s1), s1)
		
		#attr(s1,".theFile")=  .theFile
		s1= subSingleQuote2div(quote='^[^\\`]*`[^\\`]*$', pattNeg='\'\\`\'|\"\\`\"', repl=c('<span class="sq">', '</span>'), s1)  # starts from `, single ` in line
		s1= gsub(" \\$ (.+) \\$ ",' &nbsp;&nbsp; $ \\1 $ &nbsp;&nbsp; ', s1)    #  inline math formula
		
		
		#==  comments, TODO and xxx  ==
		s1= gsub("^(\\s*#\')(.*)$",'<span class="comment2">\\1</span><span class="text">\\2</span>', s1)  
		s1= gsub('(#[^\'=-].*?)(<|$)','<span class="comment">\\1</span>\\2', s1)  # code2HTML3()
		s1= gsub('^(.*)(#=+)(.*)$','\\1<span class="comment2">\\2</span>\\3', s1)  # code2HTML3()
		s1= gsub('^(.*)(#\\-\\-)(.*)$','\\1<span class="comment2">\\2</span>\\3', s1)  # code2HTML()
		s1= gsub('^(.*# ?.*)((xxx|TODO):.*)(</span.*)$','\\1<font color="red">\\2</font>\\4', s1)
		c(s1, '</pre>')
	}
	
	cccc= rmd2htm.main= function(.theFile= get.theFile(), img='img', FullSyntaxHighlight= FALSE
			, classicHeaders=TRUE, show=TRUE, toSave=TRUE, outSuffix='.htm', wchunks=T) {
		s1= readLines(.theFile, warn=F)
		
		if (wchunks) {
			#debug(parse.terminal.line.quotes2); undebug(parse.terminal.line.quotes2)
			u= parse.terminal.line.quotes2(s=s1, verb=F, exec=T)
			strr(u)
			
			ut= dtt(u$lines)
			
			# options()
			#	$markdown.extensions
			#	[1] "no_intra_emphasis" "tables"            "fenced_code"       "autolink"          "strikethrough"     "lax_spacing"       "space_headers"     "superscript"       "latex_math"       
			#	
			#	$markdown.HTML.options
			#	[1] "use_xhtml"      "smartypants"    "base64_images"  "mathjax"        "highlight_code"
			#	
			#	$markdown.HTML.stylesheet
			#	[1] "D:/R/R-3.1.0/library/markdown/resources/markdown.css"
			
			#		options(markdown.HTML.options = markdownHTMLOptions())
			#		options(markdown.extensions = "latex_math")
			
			htm= ut[, list(is.code= is.code[1], htm= if(is.code[1]==1) main.r2htm(text) 
									else {
										# if(any(grepl('LaTeX', text))) brr()  # prr(text)
										s1= gsub('@@', '\\\\\\\\', text)  # drop escapes for LaTeX
										s1= gsub('(^|[^x"])@', '\\1\\\\', s1)  # drop escapes for LaTeX
										#u= markdownToHTML(text= s1, fragment.only=T) 
										opts_chunk$set(eval=F)
										s2= knit(,text= s1) 
										u= markdownToHTML(text= s2, fragment.only=(iich >1)
												, extensions = c("latex_math", "no_intra_emphasis")) 
									} )
					, by= 'iich']
			
			code.rmd={
				rmd= ut[, list(is.code= is.code[1], rmd= if(is.code[1]==1) c("```r\n", text, "```\n")
										else {
											# if(any(grepl('LaTeX', text))) brr()  # prr(text)
											s1= gsub('@@', '\\\\\\\\', text)  # drop escapes for LaTeX
											s1= gsub('(^|[^x"])@', '\\1\\\\', s1)  # drop escapes for LaTeX
											c("'", s1, "'")
										} )
						, by= 'iich']
				strr(rmd)
				opts_chunk$set(eval=F)
				s2= knit(text= rmd$rmd, output= .theFile %+% '.md') 
				#html= markdownToHTML(text= s2, fragment.only=F) 
				#writeLines(html, .theFile %+% '.html')
				md=markdownToHTML(.theFile %+% '.md', fragment.only=F, output= .theFile %+% '.html') 
				expl("file://" %+% .theFile %+% '.html')
				md
			} 
			
		}	
		
		out= c(header.jQuery,  menu.line,'<!-- pre --><div class="main"><pre>', htm$htm
				, '</div><!-- /pre --><br>##   The HTML output of ', .theFile, ' was created on ', DT()
				, '; <a href="http://www.mathjax.org/demos/scaling-math/">test MathJax </a>'
				, footer)
		
		if(toSave){
			catn(.theFile, outSuffix, .theFile %+% outSuffix)  # xxx del
			writeLines(out, .theFile %+% outSuffix)
			catf('expl("file://%s")', .theFile %+% outSuffix)
			if(show) expl("file://" %+% .theFile %+% outSuffix)
		}
		invisible(list(theFile=.theFile, header=header.jQuery, main=htm$htm, footer=footer, out=out
						, u=u, code.rmd=code.rmd))
	}
	if(0){
		rc= cccc(wchunks=T)
		showInOpera(sf("file://%s.htm", theFile))
		strr(rc$code.rmd)
		strr(rc)
		
		cc(theFile %+% '.html')
		expl(theFile %+% '.html')
		
	} 
	
	
	
#' really it used for backticks rather than single quotes : 
	subSingleQuote2div= function(quote='^\\s*`\\s*$', pattNeg='\'\\`\'|\"\\`\"', repl=c('<div class="sq">', '</div>'), s1= readLines(theFile, warn=F)){
		x= grepl(patt=quote, s1) & !grepl(patt=pattNeg, s1)
		if(sum(x) %% 2 != 0) warning(sf('odd quotes in %s: %s', attr(s1,"theFile"), pas(which(x))))
		
		s1[x]= repl %+%  s1[x]  # %+% repl
		s1
	}
	
	
	
# get status for single, double quotes, backticks - single symbol on the line
# s2= getQuoteCommentStatus(s= c('1abs#bb\n2#a"cc"\n3aa"bb#cc\n4`aa"b`b', '5aa"bb#cc\n6a\'aa#"bb')); prr(s2)
	if (0) {
		s= readLines(textConnection(' x=1
								
								"bb **cc** *dd*
								
								## bhbhbh
								
								"
								y=0
								z=3
								"
								# aa
								# bb"
								"abc"
								
								'))
	}
	
	ccc= parse.single.line.quotes= function(file= get.theFile(), s=readLines(file), verb=F, exec=T) {
		#sing.quo= gregexpr('^\\s*([\'"`])\\s*$', s)
		sing.quo= regexpr('^\\s*([\'"`])\\s*$', s)
		if (0) {
			state02=ifelse(sing.quo==1, substr(s, attr(sing.quo,"match.length"), attr(sing.quo,"match.length")), '')
			state.text= (sing.quo==1)
			state.text= 0+ grepl('^\\s*([\'"`])\\s*$', s)
			state.text= cumsum(sing.quo==1)%%2
			
			s= gsub('\t','    ', s)
			ident= regexpr('\\S', s) - 1 
			
			
			s3= s %+% ifelse(sing.quo>0, ifelse(ichunk %%2, '</code></pre>\n\n', '\n<pre><code>\n'), ''); prr(s3)
			
			s3=c( '<html>\n<pre><code>\n', s %+% ifelse(sing.quo>0, ifelse(ichunk %%2, '</code></pre>\n\n', '\n<pre><code>\n'), ''), '</code></pre></html>\n\n')
			s3=c( '\n<pre><code>\n', s %+% ifelse(sing.quo>0, ifelse(ichunk %%2, '</code></pre>\n\n', '\n<pre><code>\n'), '')
					, '</code></pre>\n\n')
			
			gw()
			htm= markdownToHTML(text= s3, fragment.only=F)  ; prr(htm)
			markdownToHTML(output='zz.htm', text= s3, fragment.only=F)
			cat(htm, file='zz.htm')
			expl('zz.htm')
			file.remove('zz.htm')		
		}
		s= gsub('\t','    ', s)
		
		ichunk= cumsum(grepl('^\\s*([\'"`])\\s*$', s))
		u= df(sing.quo, ichunk, style=ifelse(ichunk %%2, 'md', 'r')
				, ident= regexpr('\\S', s) - 1
				, ident1= attr(sing.quo,"match.length")-1, s)
		
		
		s2= split(s, ichunk)
		u2= split(u, ichunk)
		
		s4= list()
		for(i in 1:le(s2)){ # i=3
			if(i%%2==1){#  R code
				#s4[[i]]= c('\n<pre><code>\n', s2[[i]]) # r code
				s4[[i]]= markdownToHTML(text= c('', '    ' %+% s2[[i]], ''),  fragment.only=F) # r code
			} else{ 
				minide=max(0, min(u2[[i]]$ident))
				txt= substr(s2[[i]][-1], minide+1, 999)
				ides= pas(rep(' ', minide))
				htm= ides %+% markdownToHTML(text= txt, fragment.only=T)
				s4[[i]]= c(s2[[i]][1], '\n</code></pre>\n\n', htm) 
			}
		}
		
		#s4= markdownToHTML(text= s %+% '\n', fragment.only=F) #xxx !!!
		s5= c('<html>', unlist(s4) %+%  '\n', '</html>'); if( verb) prr(s5)
		if (exec) {
			cat(s5, file='zz.htm')
			expl('zz.htm')
			#file.remove('zz.htm')
		}
		attr(s5, 'detailes')= u
		invisible(s5)
	}
# u= parse.single.line.quotes(file= get.theFile(), verb=F, exec=T)
# gw()
	
	
	
	
		#' for Code2HTML: prevent < and > in R code render as HTML tags. 
		# s= replaceTagsOutSq(readLines('M:/85_Otto/85_Otto.r', warn=F)); prr(tail(s))
		#e s= replaceTagsOutSq(c(" <aa '<bb'   `<ee`  <cc", " <aa '<bb'   `<ee`  <cc")); prr(s)
			replaceTagsOutSq= function(s= readLines('M:/85_Otto/85_Otto.r', warn=F)){
		#	comme = ifelse(grepl('#', s), sub('.*?(#.*)', '\\1', s), '')
		#	s= no.comme = sub('#.*', '', s)
		
		#s3= unlist(strsplit('~#~' %+% s,''))  # now # is \n
		s3= strsplit(pas(s,'~#~','~#~'),'')[[1]]  # now # is \n
		inbt= cumprod((-1)^ (s3=='`'))== -1
		insq= cumprod((-1)^ (s3=="'"))== -1
		s3[!insq & !inbt]= sub('>', '&gt;', sub('<', '&lt;', s3[!insq & !inbt]))
		s4= strsplit(pas(s3, '', ''), '~#~') [[1]] #[-1]
		#s4= s4 %+% comme
		s4
	}
	
	
	
	
#' split code to chunks by single first or last quote: ' ' `
	ccc2= parse.terminal.line.quotes2= function(file= get.theFile(), s= readLines(file), verb=F, exec=T) {
		#s= readLines('m:/50_HLP/out/HLP_demo/HLP_demo.r', warn =0)
		
		#brr()
		
		#sing.quo= gregexpr('^\\s*([\'"`])\\s*$', s)
		s= gsub('\t','    ', s)  # prr(s)
		sing.quo= regexpr('^\\s*([\'"`])\\s*$', s)
		#q1= gre2('^\\s*[\'"`]', '^\\s*([\'"`]).*\\1\\S', s, v=F)  # line start quote
		#q2= gre2('[\'"`]\\s*$', '\\S([\'"`]).*\\1\\s*$', s, v=F)  # line end quote
		q1= gre2('^\\s*[\'"`]', '^\\s*([\'"`]).*\\1', s, v=F)  # line start quote
		#q2= gre2('[\'"`]\\s*$', '([\'"`]).*\\1\\s*$', s, v=F)  # line end quote
		q2= gre2('[\'"`]\\s*$', '\\1.*([\'"`])\\s*$', s, v=F)  # line end quote
		if (0) {
			s='#  == Code in top and bottom of the R file.  - "cache", "parking lot"'
			gre2('[\'"`]\\s*$', '([\'"`]).*\\1\\s*$', u$lines$s[180], v=F)
			gre2('[\'"`]\\s*$', '\\1.*([\'"`])\\s*$', u$lines$s[180], v=F)
			grepl('[\'"`]\\s*$', u$lines$s[180])
			grepl('([\'"`]).*\\1\\s*$', u$lines$s[180])
			regexpr('([\'"`]).*\\1\\s*$', u$lines$s[180])
			regexpr('([\'"`])', u$lines$s[180])
			regexpr('([\'"`])\\s*$', u$lines$s[180])
			s[q1]
			prr(s[q2])
			
		}
		ich= cumsum(q2 | q1)
		iich= ich - ifelse(ich%%2, 0, c(0, diff(ich)))  # chunk index, from 0; 0 is.code
		text= ifelse(q1, sub('^\\s*[\'"`]', '', s), s)
		text= ifelse(q2, sub('[\'"`]\\s*$', '', text), text)  # prr(text)
		
		
		u= df(line=1:le(s), s, iich= ich - ifelse(ich%%2, 0, c(0, diff(ich))), ich, is.code=1- iich%%2, text,q1,q2, stringsAsFactors = F)
		chunks= dlply(u, .(iich, is.code), I)  ; strr(chunks)
		
		require(markdown)
#	sh= unlist(sapply(chunks, function(cha){if(cha$is.code[1])c("\n<pre><code>", cha$text, "</code></pre>\n" ) 
#						else markdownToHTML(text=cha$text, fragment.only=T)}))
		
		sh= unname(unlist(sapply(chunks, function(chu){
									if(chu$is.code[1])c(sf('\n<pre><code chu="%s" line="%s">', chu$iich[1], chu$line[1])
												, chu$text, sf('</code chu="%s" line="%s"></pre>\n', chu$iich[1], last(chu$line)) ) 
									else chu$text}))) ; strr(sh) # prr(sh)
		s3a=sh # %+%  '\n'   ; strr(s3a)# prr(s3)	
		s3= gsub('^ *#', '#', s3a)
		#s4= markdownToHTML(text= s3, fragment.only=F, options = "highlight_code")  
		
		strr(s3)
		
		s4= markdownToHTML(text= s3, fragment.only=F, options = cn("mathjax highlight_code"))  # toc 
		strr(s4)
		s5= markdownToHTML(text= s3, fragment.only=T, options = cn("mathjax highlight_code"))  
		
		strr(s5)
		tf= fp(tempdir(), 's.htm'); cat(s4, file=tf); expl(tf)
		tf0= fp(tempdir(), 's0.htm'); cat(s5, file=tf0); expl(tf0)
		return(list(lines=u, chunks= chunks, s.md= s3, s.htm= s4, s0.htm= s5, tf=tf, tf0=tf0))
#'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''	
		
		if (0) {
			#df(start.q, end.q, ich=cumsum(start.q + end.q), s)
			#s2= ifelse(start.q, "</code>",'') %+% s %+% ifelse(end.q, "<code>",'')
			#s2= ifelse(start.q, "```\n",'') %+% ifelse(ich%%2, '', '    ') %+%  s %+% ifelse(end.q & !start.q, "\n```",'')
			#s2= ifelse(start.q, "`\n",'') %+% ifelse(ich%%2, '', '\t') %+%  s %+% ifelse(end.q & !start.q, "\n`",'')
			s2= ifelse(q1, "</code></pre>\n",'') %+% ifelse(ich%%2, '', '\t') %+%  s %+% ifelse(q2 & !q1, "\n<pre><code>", '') #c('a','b') %+% 1:2
			#s2= ifelse(ich%%2, '', '\t') %+%  s 
			#s3= c("`\n", s2, "`\n\n") %+% '\n'
			s3= c("<pre><code>\n", s2, "</code></pre>\n\n") %+% '\n'
			#s3= s2 #c("```\n", s2, "\n```\n") %+% '\n'
			gw()
			# gw: sw("");  expl()
			
			
			sh= unname(unlist(sapply(chunks, function(chu){if(chu$is.code[1])c("\n'''r\n", chu$text, "\n'''\n" ) 
										else chu$text})))  # prr(sh)
			s3=sh  %+% '\n'   # prr(s3)
			s2= ifelse(q1, "\n'''\n",'') %+% ifelse(ich%%2, '', '\t') %+%  s %+% ifelse(q2 & !q1, "\n'''r\n", '') #c('a','b') %+% 1:2
			s3= c("'''r\n", s2, "\n'''\n\n") %+% '\n'
			s3= gsub('^ *#', '#', s3)
			
			
			sw('m:/62_MM_dispos/out')
			cat(s3, file='m:/62_MM_dispos/out/zz.md', sep='')
			prr(markdownHTMLOptions())
			expl(system.file('resources', 'markdown.html', package = 'markdown') )
			markdownToHTML('m:/62_MM_dispos/out/zz.md', 'm:/62_MM_dispos/out/zz.htm', fragment.only=F, options = "highlight_code")  # prr('zz.md')
			cc('m:/62_MM_dispos/out/zz.htm')
			expl('m:/62_MM_dispos/out/zz.htm')
			expl('m:/62_MM_dispos/out/')
			
			file.remove('zz.htm')
			
			zzz
			
			execf('d:/z/arc/MultiMarkdown-Windows-Portable-4.3.1/multimarkdown.exe "m:/62_MM_dispos/out/zz.md" >> m:/62_MM_dispos/out/zz.htm')
			expl('zz.htm')
			
		}
		
	}
	if (0) {
		u= parse.terminal.line.quotes2(s=readLines('m:/50_HLP/out/HLP_demo/HLP_demo.r', warn =0), verb=F, exec=T)
		strr(u)
		u$lines[1:5, ]
		u$lines[81:90, ]
		u$lines[111:115, ]
		u$lines[175:180, ]
		u$lines[192:195, ]
		
	}
	
	
	
}


if(0){   #== Misc
	theFile= 'R:/work/R-svn-ass/00_commonR/zCodeTools.fun.r'
	listFuncUsage(theFile, stoplist='^(c|if|for|function|with|within|exp|log|list|legend|lm|ifelse|invisible|ecdf|lines|plot|min|max|na\\.omit|f|expression|options|rgamma|runif|round|str|unique)$')
	listFuncUsage(theFile, stoplist='^(c)$')
	
	gff('saved', theFile)
	gff('sa\\(|===', theFile)

	theFile='M:/50_HLP/out/packages/WorkJournal/inst/rcode/WorkJournal.r'
	gff('pic', theFile)
	gff('Pic', theFile)
	gff('getQuoteCommentStatus', theFile)
	gff('theFile', theFile)
	
	
	u= listFuncDef(theFile= 'M:/50_HLP/out/packages/WorkJournal/inst/rcode/test.WorkJournal.r')
	
	theFile= fp(proot, '80_ChurnSim.r')+ dummy(x)
	#ccc= function()code2HTML(theFile)
	#cc= function()code2HTMLjQuery(theFile) #  cc()
	cc()
	
	shell('start explorer file:\\\\m:\\80_ChurnSim\\80_ChurnSim.r.jQ.htm')
	CreateNewProj(newProj.name= 'HLP_demo', Templ.dir= 'T:/work/UseR-2013/lib/newProjTemplName', root='m:/50_HLP/out')
} #--

