library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(rstatix)

df <- dbo_final_movies

df$homepage <- as.factor(df$homepage)
df$original_is_en <- as.factor(df$original_is_en)
df$`revenue_budget_ratio>3` <- as.factor(df$`revenue_budget_ratio>3`)

# Description of DB

stat <- summary(df)
str(df)

# plot variables

x<-names(df)

strlst <- NULL
numlst <- NULL
for (v in names(df)) {
  if(typeof(df[[v]])=='character' | is.factor(df[[v]])) {
    strlst <- c(strlst,v) 
  } else {
    numlst <- c(numlst, v)
  }
}
strlst
numlst

# Change char to factor
for (v in strlst) {
  if(typeof(df[[v]])=='character') {
    df[[v]] <- factor(df[[v]])
  }
}

df$revenue_budget_ratio <- as.numeric(df$revenue_budget_ratio)
summary(df)


df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()


df %>% 
  group_by(original_language) %>% 
  summarise(revenue_avg=mean(revenue,na.rm=T), revenue_sd=sd(revenue,na.rm=T),n=n()) %>% 
  arrange(desc(n))

# correlation plot

install.packages("corrplot")
library('corrplot')
my_diff <- df[, sapply(df, is.numeric)]
Diff.cor = cor(my_diff, use = "pairwise.complete.obs", method = "spearman")
corrplot(Diff.cor)


# outcome 

hist(log(df$revenue))

# categoey dividing the outcome

ggplot(data=df) +
  geom_density(aes(log(revenue), group=original_language, color=original_language))

ggplot(data=df) +
  geom_density(aes(log(revenue), group=original_language, color=original_language))

ggplot(data=df) +
  geom_density(aes(log(revenue), group=popularity_cat, color=popularity_cat))

str(df$original_is_en)
df$original_is_en <- as.character(df$original_is_en)

ggplot(data=df) +
  geom_density(aes(log(revenue), group=original_is_en, color=original_is_en))

ggplot(data=df) +
  geom_density(aes(log(revenue), group=months, color=months)

ggplot(data=df) +
  geom_density(aes(log(revenue), group=homepage, color=homepage))
               
ggplot(data=df) +
  geom_density(aes(log(revenue), group=best_seller, color=best_seller))

# finding outliers

for (i in numlst) {
  boxplot(df[[i]], ylab = i)
}

#boxplot(df$budget, ylab="budget")
#boxplot(df$popularity, ylab="popularity")
#boxplot(df$runtime, ylab="runtime")
#boxplot(df$revenue_rank, ylab="revenue rank")
#boxplot(df$yearly_revenue_sum, ylab="yearly revenue sum")
#boxplot(df$revenue_year_percent, ylab="revenue year percent")

outlierMatrix <- function(df,threshold=1.5) {
  vn <- names(df)
  outdata <- data.frame(row1=1:nrow(df))
  for(v in vn) {
    if(is.numeric(df[[v]])) {
      outlow <- quantile(df[[v]],probs = 0.25,na.rm = T) 
      outhigh <- quantile(df[[v]],probs = 0.75, na.rm = T)
      irq_level <- (outhigh - outlow) * threshold
      outlow <- outlow - irq_level
      outhigh <- outhigh +  irq_level
      mv <- ifelse(df[[v]] < outlow | df[[v]] > outhigh, 1, 0)
      outdata[v] <- mv
    } else {
      mv <- rep(0,nrow(df))
    }
  }
  outdata$row1 <- NULL
  return(outdata)
}

df_out <- outlierMatrix(df)
head(df_out)

#budget_non <- df$budget[which(df_out$budget==0)]
#length(budget_non)
#ks <- ks.test(df$budget, budget_non)

# finding distribution without outliers

res1 <-NULL
for (n in numlst){
  out <- df[[n]]
  non <- df[[n]][which(df_out[[n]]==0)]
  outnum <- length(out)-length(non)
  pval <- suppressWarnings(ks.test(out,non)$p.value)
  res1 <- rbind(res, cbind(var=n, outlier_pct=outnum, distribution_changed=ifelse(pval<0.05, "+", "-")))
}
res1

# finding correlation without outliers

install.packages("cocor")
library(cocor)
budget_out <- df$budget
budget_non <- df$budget[which(df_out$budget==0)]
revenue_non <- df$revenue[which(df_out$budget==0)]
df1 <- data.frame(budget_out=budget_out,revenue_out=df$revenue )
df2 <- data.frame(budget_non=budget_non, revenue_non=revenue_non)
  
cr1<- cocor(~ budget_out+revenue_out | budget_non + revenue_non, data = list(df1,df2)) 
cr1 

df1 <- NULL
df1<- data.frame(budget=df$budget, revenue=df$revenue, df_out=df_out$budget)
df1$bu

df1 <- NULL
df2<-NULL
popularity_out <- df$popularity
popularity_non <- df$popularity[which(df_out$popularity==0)]
revenue_non <- df$revenue[which(df_out$popularity==0)]
df1 <- data.frame(popularity_out=popularity_out,revenue_out=df$revenue )
df2 <- data.frame(popularity_non=popularity_non, revenue_non=revenue_non)

cr2<- cocor(~ popularity_out+revenue_out | popularity_non + revenue_non, data = list(df1,df2)) 
cr2 


df1 <- NULL
df2<-NULL
year_out <- df$year
year_non <- df$year[which(df_out$year==0)]
revenue_non <- df$revenue[which(df_out$year==0)]
df1 <- data.frame(year_out=year_out,revenue_out=df$revenue )
df2 <- data.frame(year_non=year_non, revenue_non=revenue_non)

cr3<- cocor(~ year_out+revenue_out | year_non + revenue_non, data = list(df1,df2)) 
cr3 

df1 <- NULL
df2<-NULL
runtime_out <- df$runtime
runtime_non <- df$runtime[which(df_out$runtime==0)]
revenue_non <- df$revenue[which(df_out$runtime==0)]
df1 <- data.frame(runtime_out=runtime_out,revenue_out=df$revenue )
df2 <- data.frame(runtime_non=runtime_non, revenue_non=revenue_non)

cr4<- cocor(~ runtime_out+revenue_out | runtime_non + revenue_non, data = list(df1,df2)) 
cr4 


outdf <- NULL
nondf <- NULL
res2 <-NULL
for (n in numlst){
  out <- df[[n]]
  non <- df[[n]][which(df_out[[n]]==0)]
  revenue_non <- df$revenue[which(df_out[[n]]==0)]
  outdf <- data.frame(x_out=out,y_out=df$revenue)
  nondf <- data.frame(x_non=non, y_non=revenue_non)
  cr<- cocor(~ x_out+y_out | x_non + y_non, data = list(outdf,nondf)) 
  pval <- cr@fisher1925$p.value
  res2 <- rbind(res2, cbind(var=n, correlation_changed=ifelse(pval<0.05, "+", "-")))
}
res2

# creating joined table of changed distribution and correlation

res <- inner_join(data.frame(res1), data.frame(res2), by="var")
res$drop <- ifelse(res$distribution_changed=="+" & res$correlation_changes =="+", "NO", "Yes")
res

# creating new varibels without outliers

df$popularity_no_outliers <- df_out$popularity
df$year_no_outliers <- df_out$year
df$runtime_no_outliers <- df_out$runtime

mutate(df, popularity_no_outliers=ifelse(popularity_no_outliers==0,df$popularity,0))
summary(df$popularity_no_outliers)


df < - df %>%
  mutate(popularity_no_outliers = if_else(popularity_no_outliers == 0, popularity , 0))

df < - df %>%
  mutate(year_no_outliers = if_else(year_no_outliers == 0, year , 0))

df < - df %>%
  mutate(runtime_no_outliers = if_else(runtime_no_outliers == 0, runtime , 0))


# FINDING MISSING VALUES

summary(df)
df_missing <- is.na.data.frame(df)
head(df_missing)

df_missing <- 1*df_missing
heatmap(df_missing, scale="column")

heatmap(as.matrix(df_missing), scale="column")


missingMatrix <- function(data) {
  vn <- names(data)
  missdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    mv <- ifelse(is.na(data[[v]]),1,0)
    missdata[v] <- mv
  }
  missdata$row1 <- NULL
  return(missdata)
}

getMissingness <- function (data, getRows = FALSE) {
  require(dplyr)
  l <- nrow(data)
  vn <- names(data)
  nadf <- data
  cnt <- NULL
  miss <- function(x) return(sum(is.na(x)))
  for (n in vn) {
    nadf[[n]] <- ifelse(is.na(nadf[[n]]) == T, 1, 0)
    cnt <- rbind(cnt, data.frame(n, sum(nadf[[n]])))
  }
  names(cnt) <- c("var", "na.count")
  cnt$rate <- round((cnt$na.count/nrow(nadf)) * 100, 1)
  nadf$na.cnt <- 0
  nadf$na.cnt <- rowSums(nadf)
  cnt <- cnt %>% dplyr::arrange(desc(na.count)) %>% dplyr::filter(na.count > 
                                                                    0)
  totmiss <- nadf %>% dplyr::filter(na.cnt == 0) %>% dplyr::tally()
  idx <- NULL
  msg <- (paste("This dataset has ", as.character(totmiss), 
                " (", as.character(round(totmiss/nrow(data) * 100, 1)), 
                "%)", " complete rows. Original data has ", nrow(data), 
                " rows.", sep = ""))
  if (getRows == TRUE & totmiss != 0) {
    nadf$rn <- seq_len(nrow(data))
    idx <- nadf %>% dplyr::filter(na.cnt == 0) %>% dplyr::select(rn)
  }
  print(list(head(cnt, n = 10), msg))
  return(list(missingness = cnt, message = msg, rows = idx$rn))
}

mm <- getMissingness(data = df)
mm$missingness

df_na <- missingMatrix(df)
install.packages("naniar")
library(naniar)
options(repr.plot.width = 15, repr.plot.height = 8)
vis_miss(df,warn_large_data=F)

df_na <- df_na[,mm$missingness$var]
head(df_na)


install.packages("corrgram")
library(corrgram)
corrgram(df_na, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Missingness correlation")

library(gridExtra)
library(ggplot2)

for (m in mm$missingness$var) {
  p <- list()
  i <- 1
  for (v in numlst) {
    if (v != m) {
      ff <- data.frame(v=df[[v]],m=factor(df_na[[m]]))
      p[[i]] <- suppressMessages(ggplot(data=ff,aes(x=v,group=m,color=m)) +
                                   geom_density()+labs(title=paste(v, "(miss=", m,")"),x =v))
      i <- i + 1
    }
  }
  do.call(grid.arrange,p)
}

# test the missing mechanism using glm

mis <- NULL
for (m in mm$missingness$var) {
  ff <- df[m]
  ff[[m]] <- df_na[[m]]
  mod <- glm(ff[[m]] ~.,data=ff, family="binomial")
  sm <- summary(mod)
  if (is.null(sm)==F) {
    sm2 <- data.frame(var=row.names(sm$coefficients), pvalue=sm$coefficients[,4])
    mis <- rbind(mis, cbind(m,sm2))
  } else {
    print(sm)
  }
}
row.names(mis) <- NULL
mis