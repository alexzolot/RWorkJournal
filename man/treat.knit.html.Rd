\name{treat.knit.html}
\alias{treat.knit.html}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Function treat.knit.html..
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
treat.knit.html(theFile, rFile = theFile, dout = dirname(rFile))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theFile}{
%%     ~~Describe \code{theFile} here~~
}
  \item{rFile}{
%%     ~~Describe \code{rFile} here~~
}
  \item{dout}{
%%     ~~Describe \code{dout} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (theFile, rFile = theFile, dout = dirname(rFile)) 
{
    catf("\n:\n")
    "we have 2 knitr produced files: code2rmd() ->  .kn.htm  and cccc= rmd2htm.main -> .html"
    js1 = "\t<!-- script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js\"></script-->\n\t\t\t\t\n\t\t\t\t<script src=\"http://code.jquery.com/jquery-2.1.1.js\"></script>\n\t\t\t\t\n\t\t\t\t<script>\n\t\t\t\t\n\t\t\t\tvar d= document;  \n\t\t\t\tvar imgFold= \"<img src='data:image/jpeg;base64, R0lGODlhDQAMAJEAAAAAAP///2NlY////yH5BAEAAAMALAAAAAANAAwAAAIYnI+pK+2OBIvzSHWNfJtin2SDaEUctCwFADs=' alt='-'/>\";\n\t\t\t\tvar srcMinus=\"data:image/jpeg;base64, R0lGODlhDQAMAJEAAAAAAP///2NlY////yH5BAEAAAMALAAAAAANAAwAAAIYnI+pK+2OBIvzSHWNfJtin2SDaEUctCwFADs=\";\n\t\t\t\t\n\t\t\t\t$(document).ready(function(){\n\t\t\t\t\n\t\t\t\t\n\t\t\t\t$(\".fold\").src= srcMinus;\n\t\t\t\t\n\t\t\t\t$(\".figz\").each(function(){   //copy src from gallery to TOC and main - for base64 nOK!\n\t\t\t\tcl(this.id)\n\t\t\t\tthis.src= $(\"#tn\" + this.id).src\n\t\t\t\t$(\"#TOC\" + this.id).attr(\"src\",  this.src)\n\t\t\t\t})\n\t\t\t\t\n\t\t\t\t//http://stackoverflow.com/questions/5540561/jquery-click-event-not-firing\n\t\t\t\t$(\".fig\").click(function(e) {e.preventDefault(); \n\t\t\t\tfor(k in e) if(e[k] != null && k.match(/width|css|style/)) cl(\"fig.e[\" + k + \"]= \" + e[k]); \n\t\t\t\tw=this.width; \n\t\t\t\t$(this).css({width: (w==700)? 1000 :  (w==1000)? 1200 : (w==1200)? \"100\%\" : 700})\n\t\t\t\t})\n\t\t\t\t.dblclick(function(e) {e.preventDefault(); ShowImg(this.src, this.alt, e.ctrlKey) });\n\t\t\t\t\n\t\t\t\t$(\"img.imgGal, img.TOCfig\")\n\t\t\t\t.mouseover(function() {this.title= this.alt; show(this) })\n\t\t\t\t.mouseout(function() { show(0) })\n\t\t\t\t.click(function() {gotoo(this.id.replace(\"tn\", \"\").replace(\"TOC\", \"\")) })\n\t\t\t\t.dblclick(function(e) { e.preventDefault(); ShowImg(this.src, this.alt, e.ctrlKey) });\n\t\t\t\t\n\t\t\t\t\n\t\t\t\t$(\"span.TOC\").click(function() {gotoo(this.id.replace(\"TOC\", \"\")) })\n\t\t\t\t\n\t\t\t\t\n\t\t\t\t$(\".aToggle.DD1\").click(function() {toggleD($(this), $(\".H1 + + .aD\"), $(\".D1\")) })\n\t\t\t\t$(\".aToggle.DD2\").click(function() {toggleD($(this), $(\".H2 + + .aD\"), $(\".D2\")) })\n\t\t\t\t$(\".aToggle.DD3\").click(function() {toggleD($(this), $(\".H3 + + .aD\"), $(\".D3\")) })\n\t\t\t\t$(\".aToggle.DD4\").click(function() {toggleD($(this), $(\".H4 + + .aD\"), $(\".D4\")) })\n\t\t\t\t$(\".aToggle.TOContents\").click(function() {toggleD($(this), $(\".zz\"), $(\"div.TOContents\")) })\n\t\t\t\t$(\".aToggle.Gallery\").click(function() {toggleD($(this), $(\".zz\"), $(\"div.Gallery\")) })\n\t\t\t\t$(\".aToggleAllFig\").click(function()   {toggleD($(this), $(\".zz\"), $(\"img, iframe\"))}) //xxx: ; toggleD($(this), $(\".zz\"), $(\"iframe\")) \n\t\t\t\t//$(\".linu\").click(function() { $(\"body\").scrollTop(0) })\n\t\t\t\t\n\t\t\t\t$(\"#aToggleAll\").click(function() {th= $(this)\n\t\t\t\tif( th.text() ==\"(+)\") {\n\t\t\t\t$(\"div\").show(); th.html(imgFold); $(\"a:not(.comments)\").html(imgFold)   \n\t\t\t\t} else { $(\"div\").hide(); th.html(\"(+)\"); $(\"a:not(.comments)\").html(\"(+)\")}\n\t\t\t\t})\n\t\t\t\t\n\t\t\t\t\n\t\t\t\t$(\".aToggleComments\").click(function() {ToggleComments2();})\n\t\t\t\t\n\t\t\t\t// $(\".aD\").click(function() //zzz does not work inside <code> can be explicitly call <a href=\"javascript:alert();\" class=\"aD\">\n\t\t\t\t\n\t\t\t\t})\n\t\t\t\t</script>\n\t\t\t\t<script>\n\t\t\t\t\n\t\t\t\tfunction cl(x){console.log(x)}\n\t\t\t\t\n\t\t\t\tfunction gotoo(i) {w= d.location= \"#\" + i;}\n\t\t\t\tfunction gotop() {d.body.scrollTop = d.documentElement.scrollTop = 0}\n\t\t\t\t\n\t\t\t\tfunction linutoggle() {$(\"span.linu\").toggle()}\n\t\t\t\t\n\t\t\t\tfunction ToggleFold(d){ \n\t\t\t\t$(\"#D\" + d).animate({ height:\"toggle\"}, 600); th= $(\"#asp\" + d); \n\t\t\t\tth.html(th.html()== \"...}\"  ? imgFold : \"...}\")\n\t\t\t\t}\n\t\t\t\t\n\t\t\t\tfunction resize0(e) { w= e.width;\n\t\t\t\te.style.width= (w==700)? 1000 :  (w==1000)? 1200 : (w==1200)? \"100\%\" : 700;\n\t\t\t\t}\n\t\t\t\t\n\t\t\t\tfunction resize(i) {e=d.getElementById(i); w= e.width;\n\t\t\t\te.style.width= (w==700)? 1000 :  (w==1000)? 1200 : (w==1200)? \"100\%\" : 700;\n\t\t\t\t}\n\t\t\t\t\n\t\t\t\t\n\t\t\t\tfunction ShowImg(src, capt, ctrl){ \n\t\t\t\tif(ctrl) {src= src.replace(\"png\", \"pdf\")};\n\t\t\t\tw= window.open(src, capt, \"type=fullWindow, fullscreen, location='', height=\"+ screen.height+ \", width=\"+screen.width)\n\t\t\t\tif(ctrl) { w.document.location= src} else {w.document.write(\"<html><title>\" +  capt + \"</title>\" + capt + \"<br/><img src='\"  + src + \"' /> </html>\")};\n\t\t\t\tw.title= capt;\n\t\t\t\tw.document.title= capt;\n\t\t\t\tw.focus()\n\t\t\t\t}\n\t\t\t\t\n\t\t\t\t\n\t\t\t\tfunction show(e){\n\t\t\t\t$(\"#show\").css({display : (e==0)? \"none\" : \"block\", border: (e==0)? \"0\" : \"2px solid blue\"})\n\t\t\t\t\n\t\t\t\tif(e != 0){\n\t\t\t\tx= Math.min(e.offsetLeft+ e.width + 10, 500) //x= e.x+ e.width + 10;// ; if(x > 500) x= 500;\n\t\t\t\ty= e.offsetTop+ e.height + 10               //y= e.y+ e.height + 10\n\ncl(\"e.src= \" + e.src);\n\t\t\t\t\n\t\t\t\t$(\"#show\").css({left: x, top: y})\n\t\t\t\t$(\"#showFig\").attr(\"src\", e.src)\n\t\t\t\t$(\"#showTxt\").html(\"<br/>\" +  e.alt)  //  +  e.alt)\n\t\t\t\t\n\t\t\t\t/* show properties of e \n\t\t\t\tfor(k in e) if(1 || e[k] > 10 && e[k]+\" \" < \"a\") cl(\"e[\" + k + \"]= \" + e[k]);\n\t\t\t\t//for(k in e) if(e[k] != null && k.match(/^src$|^id$/)) {cl(\"e[\" + k + \"]= \" + e[k])}; \n\t\t\t\t*/\n\t\t\t\t}\n\t\t\t\t}\n\t\t\t\t\n\t\t\t\t</script>"
    js.toggle = "<script>\t\n\t\t\t\tfunction toggleD(a, a2, di){ if( a.text() == \"(+)\") {\n\t\t\t\tdi.show(); a.html(imgFold); a2.html(imgFold)  \n\t\t\t\t} else { di.hide(); a.html(\"(+)\"); a2.html(\"...\")}\n\t\t\t\t}\n\t\t\t\t\n\t\t\t\tfunction ToggleComments2(){ \n\t\t\t\tvar tt = document.getElementsByClassName(\"comment2\"); \n\t\t\t\tvar t0= tt[0].style.fontSize;\n\t\t\t\tdi= (t0 == \"3px\" || t0 == \"\") ? \"12px\" : ((t0 == \"12px\")? \"1px\" : \"3px\") ;\n\t\t\t\tfc=  di == \"1px\" ? \"white\" : \"green\";\n\t\t\t\tfor (var i = 0; i < tt.length; i++) {tt[i].style.fontSize = di; tt[i].style.color=fc;}\n\t\t\t\t}\n\t\t\t\t</script>\n\t\t\t\t"
    css = "\t<style scoped>  /*  www.w3schools.com/cssref/css_colornames.asp  www.tizag.com/cssT/border.php */\n\t\t\t\tbody {max-width: 95\%; font-size: 100\%; line-height: 105\%;}\n\t\t\t\tdiv.Gallery {background-color:rgb(255, 248, 248); }\t\n\t\t\t\tdiv.TOC {background-color:rgb(248, 248, 255); }\t\n\t\t\t\tdiv.main, .r {font-family: monospace; white-space: pre; max-width: 1000px}\n\t\t\t\tp{margin-bottom:2\%; margin-top:2\%;margin-before: 2\%; margin-after: 2\%;}\n\t\t\t\tcode {border: 0px}\n\t\t\t\tpre  {border: 1px solid}\n\t\t\t\t\n\t\t\t\t.D1, .D2, .D3, .D4, .D5  {background-color:rgba(255, 240, 240, .2); border-style:ridge; margin:5px; padding:15px;-moz-border-radius:5px; border-radius:5px} /*div {; opacity: 0.3; background-color:GhostWhite; border-left-style:ridge;}  */\n\t\t\t\t// code, pre  {background-color:rgba(240, 240, 255, .2); border-style:ridge; margin:5px; padding:15px;-moz-border-radius:5px; border-radius:5px} \n\t\t\t\tdiv.D5 {font-size:8px;} \n\t\t\t\t//.sq {font-size:12px; font-family: Arial; color:DodgerBlue } \n\t\t\t\tcenter, .capt {font-size:12px; font-weight:bold; font-family: Arial; margin:auto; text-align:center;}\n\t\t\t\t.captTOC {font-size:100\%; font-weight:bold; font-family: Arial; align:left}\n\t\t\t\tH1,  H2,  H3,  H4,  H5 {color:blue; font-family: Arial; color:teal;}\n\t\t\t\t.H1, .H2, .H3, .H4, .H5 {color:teal; font-family: Arial; font-weight:bold;  display:inline-block; display:-moz-inline-box;}\n\t\t\t\tH1, .H1 {font-size:300\%;  margin-top:36px;  margin-bottom:8px;  margin-left:0px; } \n\t\t\t\tH2, .H2 {font-size:250\%;  margin-top:28px;  margin-bottom:5px;  margin-left:20px;} \n\t\t\t\t.TOC.H1, H3, .H3 {font-size:200\%;  margin-top:15px;  margin-bottom:3px;  margin-left:40px;}\n\t\t\t\t.TOC.H2, H4, .H4 {font-size:150\%;  margin-top:12px;  margin-bottom:2px;  margin-left:60px;}\n\t\t\t\t.TOC.H3, H5, .H5 {font-size:120\%;  margin-top:10px;  margin-bottom:1px;  margin-left:80px;}\n\t\t\t\timg.fig, .captTOC, img.tnTOC {margin-top:0px;   margin-bottom:0px;  margin-left:10px;} \n\t\t\t\t.TOC.H1, .TOC.H2, .TOC.H3, .TOC.H4, .TOC.H5, span.captTOC, img.tnTOC { display:block;}\n\t\t\t\t.D77 { display:inline; }\n\t\t\t\timg, .aD, iframe {border-style:none; border:0}\n\t\t\t\t/* .dimg{height:90px;  display:inline-block;} /* position:fixed;  position:absolute;*/ \n\t\t\t\t.imgGal{margin-top:1px;   margin-bottom:1px;  margin-left:1px; height:100px} /*height:100\%; max-width:100\%; max-height:100\%; }/* position:absolute; fixed;*/ \n\t\t\t\t\n\t\t\t\t.fun {color:indigo; font-weight:bold;}\n\t\t\t\t.comment {color:green; align:left}\n\t\t\t\t.comment2 {color: rgb(100, 200, 100); font-size:3px; -webkit-text-size-adjust: none;}\n\t\t\t\t.text, .sq {color: teal; font-size:12px;}\n\t\t\t\t</style>\n\t\t\t\t"
    menu.line = "# <a href=\"#\" id= \"aToggleAll\">Fold</a> All |\n\t\t\t\t<a href=\"#\" class= \"aToggleAllFig\">Toggle</a> all figs | \n\t\t\t\t<a href=\"#\" class= \"aToggle Gallery\">Fold</a> Gallery |\n\t\t\t\t<a href=\"#\" class= \"aToggle TOContents\">Fold</a> TOC |\n\t\t\t\t<a href=\"#\" class= \"aToggle DD2\">Fold</a> H2 |\n\t\t\t\t<a href=\"#\" class= \"aToggle DD3\">Fold</a> H3 |\n\t\t\t\t<a href=\"#\" class= \"aToggle DD4\">Fold</a> H4 |\n\t\t\t\t<a href=\"#\" class= \"aToggleComments\">Toggle</a> Comments |\n\t\t\t\t<a href=\"javascript:linutoggle()\" >Toggle</a> Line Numbers <span id=\"out\"></span></br>"
    footer = "<div id= \"show\" style= \"position: absolute; left:-1000px; top:1px; border:solid blue; padding:5; display:none; background-color:white; -moz-border-radius: 10px; border-radius: 10px;\">\n\t\t\t\t<span id=\"showTxt\"></span><br/>\n\t\t\t\t<img id=\"showFig\" src=\"#\" height=\"401\" style=\"left:0px\" alt=\"\"/>\n\t\t\t\t</div>\n</body>\n</html>"
    sk3 = readLines(theFile \%+\% ".kn.nu.htm")
    sk.tit = grep("<title>", sk3)
    sk3[sk.tit] = sf("<title>\%s - RWJ</title>", basename(theFile))
    synhi = readLines(system.file("resources", "r_highlight.html", 
        package = "markdown"))
    sk.3 = append(sk3, c(synhi, css), after = sk.tit)
    bd = grep("body>", sk.3)
    in.body = (bd[1] + 1):(bd[2] - 1)
    s = sk.3[in.body]
    s = gsub("(^.*#.*) \\$([^ ])(.*)([^ ])\\$ ", "\\1 \\\\\\(\\2\\3\\4\\\\\\) ", 
        s)
    s = gsub("^(<pre><code class=\"r \\d*\">)?(.*) # line (\\d+)#", 
        "\\1<span id=\"sp\\3\" class=\"linu\" onclick=\"gotop()\">\\3  </span>\\2", 
        s)
    s = gsub("^(.+/span>\\s*#')(.*)$", "<span class=\"comment2\">\\1</span><span class=\"text\">\\2</span>", 
        s)
    s = gsub("( #[^'=-].*?)(<|$)", "<span class=\"comment\">\\1</span>\\2", 
        s)
    s = gsub("^(.+/span>.*?)( *#=+ *)(.*)$", "\\1<span class=\"comment2\">\\2</span>\\3", 
        s)
    s = gsub("^(.+/span>.*)(#\\-\\-)(.*)$", "\\1<span class=\"comment2\">\\2</span>\\3", 
        s)
    s = gsub("^(.+/span>.*# ?.*)((xxx|TODO):.*)(</span.*)$", 
        "\\1<font color=\"red\">\\2</font>\\4", s)
    s = gsub("^(.*id=\"sp(\\d+)\".*# )(Fig_\\d+)(.*?)(</span.*)", 
        "<br/></code><code class=\"r\"><img id=\"fig\\2\" class=\"fig\" src=\"img/\\3.png\" width=700 alt=\"\\3\\4\"/>   \n\t\t\t\t\t\t<br/><span class=\"capt\">\\3\\4</span><br/>", 
        s)
    s = gsub("^(.*id=\"sp(\\d+)\".*# )(jFig_\\d+)(.*?)(</span.*)", 
        "<br/></code><code class=\"r\"><iframe id=\"fig\\2\" class=\"jfig\" src=\"img/\\3.htm\" width=\"100\%\"  height=\"600px\" name=\"\\3\\4\"></iframe>  \n\t\t\t\t\t\t<br/><span class=\"capt\">\\3\\4</span><br/>", 
        s)
    s = gsub("^(.*id=\"sp(\\d+)\".*# )(Pic_\\d+)(.*?)(</span.*)", 
        "<br/></code><code class=\"r\"><img id=\"fig\\2\" class=\"fig\" src=\"img/\\3.png\" width=700 alt=\"\\3\\4\"/>   \n\t\t\t\t\t\t<br/><span class=\"capt\">\\3\\4</span><br/>", 
        s)
    s = gsub("^(.*id=\"sp(\\d+)\".*# )(jPic_\\d+)(.*?)(</span.*)", 
        "<br/></code><code class=\"r\"><iframe id=\"fig\\2\" class=\"jfig\" src=\"img/\\3.htm\" width=\"100\%\"  height=\"600px\" name=\"\\3\\4\"></iframe>  \n\t\t\t\t\t\t<br/><span class=\"capt\">\\3\\4</span><br/>", 
        s)
    get.r.headers = function(s, find = "(<span id=\"sp(\\d+)\".*\\s*#=+ )(.*)( =+.*)$", 
        patt.le = "#=+ ", replace = "\\1<span class=\"H\%s\" id=\"\\2\" title=\"\\2\">\\3</span> <span class=\"comment2\">\\4</span>") {
        h = regexpr(find, s)
        i = which(h > 0)
        hh = dtt(i, le = attr(regexpr(patt.le, s[i]), "match.length"), 
            s = s[i])
        hh[, `:=`(s2, sub(find, sf(replace, ch(le - 2)), s)), 
            by = i]
        hh
    }
    hh1 = get.r.headers(s, find = "(<span id=\"sp(\\d+)\".*#=+ +</span>)(.*?)( =+.*)?$", 
        patt.le = "#=+ ", replace = "\\1<span class=\"H\%s\" id=\"\\2\" title=\"\\2\">\\3</span> <span class=\"comment2\">\\4</span>")
    s[hh1$i] = hh1$s2
    prr(s[hh1$i])
    hh = get.r.headers(s, find = "(<span id=\"sp(\\d+)\".*\\s*##+ )(.*)( =+.*)?$", 
        patt.le = "##+ ", replace = "\\1<span class=\"H\%s\" id=\"\\2\" title=\"\\2\">\\3</span> <span class=\"comment2\">\\4</span>")
    s[hh$i] = hh$s2
    s1 = s
    depth = countDepth2(s1)
    imgFold = "<img src=\"\" alt=\"-\" class=\"fold\" />"
    s1 = ifelse(depth != 0, gsub("^(<span id=.sp(\\d+)[^#]*\\{[^\\{\\}]*)", 
        "\\1 <a href=\"javascript:ToggleFold(\\2)\" id=\"asp\\2\"><img src=\"\" alt=\"-\" class=\"fold\" /></a>\n\t\t\t\t\t\t\t\t</code></pre><div class=\"D-fold\" id=\"D\\2\"><pre><code class=\"r div\">", 
        s1), s1)
    s1 = gsub("^(.* class=\"H(\\d)\".* id=\"(\\d+).*)D-fold(.*) class=\"D-fold\"", 
        "\\1D\\3\\4 class=\"D\\2\" id=\"D\\3\"", s1)
    s1 = ifelse(depth != 0, gsub("([^#\\{\\}]*)\\}", "\\1<b>}</b></code></pre></div><pre><code class=\"r fold\">", 
        s1), s1)
    figs = gre2("class=\"j?fig\"", , s1)
    figs = sub(".*(<img id=\".*?>).*", "\\1", figs)
    figs = sub(".*(<iframe id=\".*?iframe>).*", "\\1", figs)
    figs = sub("class=\"jfig\"", "class=\"imgGal\"", sub("width=\"100\%\"  height=\"600px\"", 
        "width=300 height=140 style=\"-ms-zoom: 0.25\"", figs))
    figs = sub("img id=\"", "img id=\"tn", figs)
    figs = sub("class=\"fig\"", "class=\"imgGal\"", sub("width=700", 
        "height=140", figs))
    s1 = ifelse(grepl("<h\\d+", s1), sf("<!-- l#\%s -->\%s", 1:le(s1), 
        s1), s1)
    s1 = sub("<!-- l#(\\d+) -->.*<h(\\d)>(.*)</h.>", "<span class=\"H\\2\" id=\"md\\1\">\\3</span>", 
        s1)
    toc = grep("img id=|iframe id=|<H\\d+|<h\\d+|\"H[1-5]", s1, 
        value = TRUE)
    toc = gsub(".*(<span class=\"H.*?)=* *</span>.*", "<br/>\\1</span>", 
        toc)
    toc = sub(".*(<img id=\".* alt=\"(.*?)\").*", "\\1 title=\"\\2\"/>", 
        toc)
    toc = sub(".*(<iframe id=\".*) name=\"(.*?)\"(.*iframe>).*", 
        "\\1 title=\"\\2\"\\3", toc)
    toc = sub("width=\"100\%\"  height=\"600px\"", "width=300 height=140 style=\"-ms-zoom: 0.25\"", 
        toc)
    toc = sub(" id=\"", " id=\"TOC", toc)
    toc = gsub("\"capt\"", "\"captTOC\"", toc)
    toc = gsub("class=\"fig\"", "class=\"TOCfig\"", toc)
    toc = gsub("class=\"H", "class=\"TOC H", toc)
    toc = gsub("700", "250", toc)
    toc = gsub("resize", "goto", toc)
    toc = gsub("id=\"(\\d+)\"", "id=\"tnTOC\\1\"", toc)
    toc = gsub("goto\\('tn(\\d+)'\\)", "goto('\\1')", toc)
    toc = gsub("\\{?( *<H\\d.*</H\\d>).*", "\\1", toc)
    toc = gsub("(.*)\\{", "<br/>\\1", toc)
    toc = gsub("<a.*$", "", toc)
    toc = gsub("(.* class= *\"H.\".*)", "<br/>\\1", toc)
    toc = gsub("<br/>", "", toc)
    css2 = "<style scoped>  /*  www.w3schools.com/cssref/css_colornames.asp  www.tizag.com/cssT/border.php */\n\t\t\t\t.TOC.fig, .imgGal{margin-top:1px;   margin-bottom:1px;  margin-left:1px; height:130px}\n\t\t\t\t.TOC.H1, .TOC.H2, .TOC.H3 {display:block}\n\t\t\t\t</style> <!-- ================================================================== -->"
    main = s1
    out = c(sk.3[1:bd[1]], css, css2, menu.line, "<div class=\"Gallery\"> <H3>Gallery for  ", 
        rFile, " </H3>", figs, "\n\n<hr/></div> <div class=\"TOContents\" id=\"0\"> <H2>Table of Contents</H2>", 
        toc, "<hr/></div><br/>\n\t\t\t\t\t\t<!-- pre --><div class=\"main\">", 
        main, "</div><!-- /pre --><br>##   The HTML output of ", 
        rFile, " was created on ", DT(), "; <a href=\"http://www.mathjax.org/demos/scaling-math/\">test MathJax </a>", 
        js1, js.toggle, footer)
    wl(out, theFile \%+\% ".6.kn.htm", show = FALSE)
    file.copy(theFile \%+\% ".6.kn.htm", fp(dout, basename(rFile) \%+\% 
        ".htm"), overwrite = TRUE)
    expl(fp(dout, basename(rFile) \%+\% ".htm"))
    invisible(list(s.htm = out, fout.htm = fp(dout, basename(rFile) \%+\% 
        ".htm")))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
