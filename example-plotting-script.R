# Clear memory ----
rm(list=ls())

# Load libraries ----
library(tidyverse)
library(fst)

# Functions for plotting ----
# functions for summarising data on plots----
se <- function(x) sd(x) / sqrt(length(x))

se.min <- function(x) (mean(x)) - se(x)
se.max <- function(x) (mean(x)) + se(x)

# Theme for plotting ----
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=15),
    legend.title = element_blank(),
    axis.line = element_line(colour = "black"),
    legend.position = "top",
    text=element_text(size=14),
    strip.text.y = element_text(size = 14,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=15),
    axis.title.y=element_text(vjust=0.6, angle=90, size=15),
    axis.text.y=element_text(size=14),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank(),
    plot.title = element_text(color="black", size=14, face="bold.italic"))

# Set working directory ----
working.dir<-dirname(rstudioapi::getActiveDocumentContext()$path) # to directory of current file - or type your own

data.dir<-paste(working.dir,"Example data",sep="/")

# Read in data ----
setwd(data.dir)
dir()

maxn<-read_fst("complete.maxn.fst")
length<-read_fst("complete.length.fst")
mass<-read_fst("complete.mass.fst")

# Maxn abundance by Status plot ----
maxn.filtered<-maxn%>%
  #filter(campaignid%in%c("2015-09_Exmouth.Gulf_stereoBRUVs"))%>%
  filter(family%in%c("Lethrinidae"))%>%
  filter(genus%in%c("Lethrinus"))%>%
  filter(species%in%c("nebulosus"))%>%
  glimpse()

status.plot <- ggplot(maxn.filtered,aes(x = factor(status), y = maxn, colour = status, fill = status,notch=FALSE, outlier.shape = NA)) + 
    stat_boxplot(geom='errorbar') +
    geom_boxplot(outlier.color = NA, notch=FALSE) +
    stat_summary(fun.y=mean, geom="point", shape=23, size=4) + # this is adding the dot for the mean
    theme_bw() +
    Theme1 +
    xlab("Status") + 
    ylab("Abundance per stereo-BRUV") +
    ggtitle("Plot of abundance by Status")

status.plot

# Length Histogram ----
length.filtered<-length%>%
  #filter(campaignid%in%c("2015-09_Exmouth.Gulf_stereoBRUVs"))%>%
  filter(family%in%c("Lethrinidae"))%>%
  filter(genus%in%c("Lethrinus"))%>%
  filter(species%in%c("nebulosus"))%>%
  glimpse()

length.histogram<-ggplot(length.filtered,aes(x = length,colour = status,fill=status))+
  geom_histogram(alpha=0.5, position="identity",binwidth=5)+
  xlab("Length (mm)") + ylab("Count") +
  theme_bw() +
  Theme1

length.histogram
