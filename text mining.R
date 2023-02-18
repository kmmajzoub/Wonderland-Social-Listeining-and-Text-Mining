library(tm)
library(arabicStemR)
library(wordcloud2)


text = readLines("comments.txt" , encoding = "UTF-8", warn = F)
text
text <- text[text!=""]


text1 = removePunctuation(text)
text1 = removeNumbers(text)
text1 = removeNewlineChars(text)
text1 = stripWhitespace(text)
 
text1
text

mytext <- data.frame(text1,stringsAsFactors = F)
mytext$doc_id <- c(1:nrow(mytext))
mytext$text <- mytext$text1
mytext

mytext <-  mytext[,c(2,1,3)]

mytext$text <- transliterate(mytext$text)

str(mytext)


arabic_corpus <- Corpus(DataframeSource(mytext))

## myextract <- data.frame(text = sapply(arabic_corpus,as.character, stringAsfactor  )) something 

arabic_tdm <- TermDocumentMatrix(arabic_corpus)

arabic_m <- as.matrix(arabic_tdm)

arabic_v <- sort(rowSums(arabic_m), decreasing = TRUE)

output_ar <- cbind(arabic_v)
head(output_ar)
tail(output_ar)


##print(Sys.getlocale(category =  "LC_CTYPE"))
##original_ctype <- Sys.getlocale(category =  "LC_CTYPE")

##Sys.setlocale("LC_CTYPE", "arabic")
##all above extra from 1st edition 

output_ar_df <- as.data.frame(output_ar)
output_ar_df$arabic <- row.names(output_ar_df)

for ( i in 1:nrow(output_ar_df)){output_ar_df$arabic_trans[i] = reverse.transliterate(output_ar_df$arabic[i])}

write.table(output_ar_df, "arabic_outpit.txt", quote = FALSE,col.names = FALSE,
            row.names = T , sep = "\t",fileEncoding = "UTF-8")

mytdm <- cbind(arabic_m, names(arabic_m))

write.table(mytdm, "termDocMatrix.txt", quote = FALSE,col.names = FALSE,
            row.names = T , sep = "\t",fileEncoding = "UTF-8")

##Sys.setlocale("LC_CTYPE", original_ctype )

original_ar <- row.names(arabic_m)
translit <- transliterate(original_ar)
translitkey <- cbind(original_ar,translit)

##print(Sys.getlocale(category =  "LC_CTYPE"))
##original_ctype <- Sys.getlocale(category =  "LC_CTYPE")
##Sys.setlocale("LC_CTYPE", "arabic")
write.table(translitkey,'arabic_translit.txt', quote = FALSE ,col.names = FALSE , 
            row.names = FALSE, sep = "\t" , fileEncoding = "UTF-8")
##Sys.setlocale("LC_CTYPE", original_ctype )

text <- readLines("arabic.txt", encoding = "UTF-8")
tttext <- transliterate(text)
tap.corpus <- Corpus(DataframeSource(data.frame(tttext)))

tap.corpus <- tm_map(tap.corpus,content_transformer(tolower))
## tap.corpus <- tm_map(tap.corpus, removeWords, c )

tap.tdm <- TermDocumentMatrix(top.corpus)
tap.m <- as.matrix(tap.tdm)
tap.v <- sort(rowSums(tap.m),decreasing = TRUE)
tap.d <- data.frame(word = names(tap.v),freq = tap.v)

wordcloud2(data = tap.d)

arabic_d <- data.frame(word = names(arabic_v), freq=arabic_v )
wordcloud2(data = arabic_d)



########################### Connecting directly to facebook ###########################


install.packages("Rfacebook")
library(Rfacebook)

token <- 'EAARH1ZBkZCyHUBAIkL7MIpTBBW7ucNSZCMcCrBncZAGVn9IgcqxqRYGtFVSBqLbArOhqXI68luUnfpyG1DZC5j0sTS1Vysxf1fGvHK3z2yFGJVrrV0GTAbrZBIWCWyxz5qrFSzQCUZASJUIvASkZBexU7iv9uTxy1516yaq1ivjwYf8ZB9uIILViU1Nl0Ur0tAetObTH5eYCZA2Xq7zfev6HqJ6kcjOI4gFR08WqCHW8wAKx9xcAZBxg4zb'
me <- getUsers('me', token,private_info = T)

me$name
me$hometown
  

obama <- getPage("barackobama", token)
my_friends <- getFriends(token)
fb_page <-  getPage(page = "facebook", token)
