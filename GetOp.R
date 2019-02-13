# A FUNCTION THAT RETURNS BANKS FEEDBACK ON A PIECE OF LEGISLATION 
# AS NOTED IN A 10-k REPORT
# GIVEN
#  cik.no - cik code for the firm , i.e for jpm 19617
#  year - year of filing 
#  name of related policy as it would show up in text (i.e "Volcker Rule")
#  n - number of sentences you want to extract after the sentence in which the "word"
#      is mentioned; = 0 by default

GetOp = function (cik.no, year, n = 0, word){
  require(XML)
  require(edgarWebR)
  require(RCurl)
  
  cik.no = as.character(cik.no)
  form="10-K"
  p=company_filings(cik.no, type=form, before=Sys.Date())
  pp=filing_documents(p$href[format(p$accepted_date, "%Y")==year])
  html = getURL(pp$href[pp$type == "10-K"], followlocation = TRUE)
  doc = htmlParse(html, asText=TRUE)
  ######################
  plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  text <- paste(plain.text, collapse = "\n")
  dtext <- iconv(text, 'utf-8', 'ascii', sub='')
  dtext = gsub("\r?\n|\r", " ", dtext)
  dtext=strsplit(paste(dtext, collapse = " "), split = "(?<=\\.)\\s(?=[A-Z])",perl = TRUE)[[1]]
  dtext=tolower(dtext)
  occurence=sum(grepl(tolower(word), as.vector(dtext)))
  if (occurence > 0){
    flag=which(grepl(tolower(word), dtext))
    flags=vector()
     for (i in 1:occurence){
      f=c(flag[i]:(flag[i]+n))
     flags=c(flags, f)
    }
     flags=flags[duplicated(flags)==FALSE]
     dtext=dtext[flags] # need next
     fulltext=dtext
  
    coname = tolower(company_details(cik.no)$information$name)
    stopwords=c("of", "bank", "corp", "inc", "& co")
     for (i in 1:length(stopwords)){
       coname=gsub(stopwords[i], "", coname)
     }
      vec=c("the company", "the bank", "the firm", " we ", " us ", " our ", "the corporation", unlist(strsplit(coname, " ")))
      nams=paste0(vec, collapse="|")
      opinion=trimws(dtext[grepl(nams, dtext)==TRUE])
      return(list(occurence = occurence, opinion = opinion, fulltext = fulltext))
  } else { 
    return(list(occurence = occurence, opinion = paste("No occurences of ", word, " has been found", sep=""), fulltext = paste("No occurences of ", word, " has been found", sep="")))
 
  }   
}
