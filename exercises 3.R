install.packages("readr")
install.packages("quanteda")
install.packages("quanteda.textstats")
install.packages("stringdist")
install.packages("dplyr")
install.packages("tibble")
install.packages("ggplot2")

library(readr) # more informative and easy way to import data
library(quanteda) # includes functions to implement Lexicoder
library(quanteda.textstats) # for estimating similarity and complexity measures
library(stringdist) # for basic character-based distance measures
library(dplyr) #for wrangling data
library(tibble) #for wrangling data
library(ggplot2) #for visualization

tweets  <- readRDS(gzcon(url("https://github.com/cjbarrie/CTA-ED/blob/main/data/comparison-complexity/cabinet_tweets.rds?raw=true")))

head(tweets)
unique(tweets$username)
length(unique(tweets$username))

#make corpus object, specifying tweet as text field
tweets_corpus <- corpus(tweets, text_field = "tweet")

#add in username document-level information
docvars(tweets_corpus, "username") <- tweets$username

tweets_corpus

dfmat <- dfm(tokens(tweets_corpus,
                    remove_punct = TRUE)) %>%
  dfm_remove(stopwords("english"))

dfmat

corrmat <- dfmat %>%
  dfm_group(groups = username) %>%
  textstat_simil(margin = "documents", method = "correlation")

corrmat[1:5,1:5]

#estimate similarity, grouping by username

cos_sim <- dfmat %>%
  dfm_group(groups = username) %>%
  textstat_simil(margin = "documents", method = "cosine") #specify method here as character object

cosmat <- as.matrix(cos_sim) #convert to a matrix

#generate data frame keeping only the row for Theresa May
cosmatdf <- as.data.frame(cosmat[23, c(1:22, 24)])

#rename column
colnames(cosmatdf) <- "corr_may"

#create column variable from rownames
cosmatdf <- tibble::rownames_to_column(cosmatdf, "username")

ggplot(cosmatdf) +
  geom_point(aes(x=reorder(username, -corr_may), y= corr_may)) + 
  coord_flip() +
  xlab("MP username") +
  ylab("Cosine similarity score") + 
  theme_minimal()



#specify different similarity measures to explore
methods <- c("correlation", "cosine", "dice", "edice")

#create empty dataframe
testdf_all <- data.frame()

#gen for loop across methods types
for (i in seq_along(methods)) {
  
  #pass method to character string object
  sim_method <- methods[[i]]
  
  #estimate similarity, grouping by username
  test <- dfmat %>%
    dfm_group(groups = username) %>%
    textstat_simil(margin = "documents", method = sim_method) #specify method here as character object created above
  
  testm <- as.matrix(test) #convert to a matrix
  
  #generate data frame keeping only the row for Theresa May
  testdf <- as.data.frame(testm[23, c(1:22, 24)])
  
  #rename column
  colnames(testdf) <- "corr_may"
  
  #create column variable from rownames
  testdf <- tibble::rownames_to_column(testdf, "username")
  
  #record method in new column variable
  testdf$method <- sim_method
  
  #bind all together
  testdf_all <- rbind(testdf_all, testdf)  
  
}

#create variable (for viz only) that is mean of similarity scores for each MP
testdf_all <- testdf_all %>%
  group_by(username) %>%
  mutate(mean_sim = mean(corr_may))

ggplot(testdf_all) +
  geom_point( aes(x=reorder(username, -mean_sim), y= corr_may, color = method)) + 
  coord_flip() +
  xlab("MP username") +
  ylab("Similarity score") + 
  theme_minimal()


speeches  <- readRDS(gzcon(url("https://github.com/cjbarrie/CTA-ED/blob/main/data/comparison-complexity/speeches.rds?raw=true")))

head(speeches)

speeches$flesch.kincaid <- textstat_readability(speeches$text, measure = "Flesch.Kincaid")

# returned as quanteda data.frame with document-level information;
# need just the score:
speeches$flesch.kincaid <- speeches$flesch.kincaid$Flesch.Kincaid

#get mean and standard deviation of Flesch-Kincaid, and N of speeches for each speaker
sum_corpus <- speeches %>%
  group_by(speaker) %>%
  summarise(mean = mean(flesch.kincaid, na.rm=TRUE),
            SD=sd(flesch.kincaid, na.rm=TRUE),
            N=length(speaker))

# calculate standard errors and confidence intervals
sum_corpus$se <- sum_corpus$SD / sqrt(sum_corpus$N)
sum_corpus$min <- sum_corpus$mean - 1.96*sum_corpus$se
sum_corpus$max <- sum_corpus$mean + 1.96*sum_corpus$se

sum_corpus

ggplot(sum_corpus, aes(x=speaker, y=mean)) +
  geom_bar(stat="identity") + 
  geom_errorbar(ymin=sum_corpus$min,ymax=sum_corpus$max, width=.2) +
  coord_flip() +
  xlab("") +
  ylab("Mean Complexity") + 
  theme_minimal() + 
  ylim(c(0,20))


#exercises 1
dfmat_grouped <- dfm_group(dfmat, groups = username)

dist_eucl <- as.matrix(textstat_dist(dfmat_grouped, method = "euclidean"))

dist_summary <- data.frame(
  username = rownames(dist_eucl)[-23],
  euclidean = dist_eucl[23, -23]
)

ggplot(dist_summary, aes(x = reorder(username, euclidean), y = euclidean)) +
  geom_point(color = "steelblue") +
  coord_flip() +
  labs(x = "MP Username", y = "Euclidean Distance to May") +
  theme_minimal()


#exercises 2
extra_measures <- textstat_readability(speeches$text, 
                                       measure = c("FOG", "SMOG", "ARI"))

speeches$FOG <- extra_measures$FOG
speeches$SMOG <- extra_measures$SMOG
speeches$ARI <- extra_measures$ARI

cor_matrix <- cor(speeches[, c("flesch.kincaid", "FOG", "SMOG", "ARI")], 
                  use = "complete.obs")

print(cor_matrix)

