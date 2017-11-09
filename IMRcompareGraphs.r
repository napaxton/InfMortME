#####
#Infant Mortality


library(tidyverse); library(dplyr)

##  Load data ##
bdn <- read_tsv("~/GitHub/InfMortME/us_infant_mortality_2.tsv")

##  Transform all data  ##
allsts <- bdn %>%
    gather(datecat, measure, 2:61, convert=T) %>%
    separate(datecat, c("year","infant"), sep="_", convert = T) %>%
    spread(infant, measure) %>%
    rename(state = X1, births = Births, deaths = Deaths, imr = IMR)

## Subset of comparable rural, white states
c.states <- c("Maine","Iowa","West Virginia","Idaho", "New Hampshire","Vermont") 
ne.states <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont")

compsts <- filter(allsts, state %in% c.states)
neweng <- filter(allsts, state %in% ne.states)

## Graph of Maine infant mortality (full state; original data does not distinguish between rural and urban subunits)
imrme <- ggplot(data=filter(allsts, state=="Maine"), aes(year, imr))  +
    geom_point() +
    geom_smooth(method = "lm", color = "red", se=F) +
    geom_smooth(method="lm", aes(), data=filter(allsts, year<= 2004, state == "Maine"), se=F) +
    geom_smooth(method="lm", aes(), data=filter(allsts, year > 2004, state == "Maine"), se=F) +
    geom_line(linetype=3) +
    ggtitle(label="Infant Mortality in Maine, 1995-2014", 
            subtitle="Subset by decades: 1995-2004, 2005-2014") +
    labs(y = "Infant Mortality (per 1000 live births)", x = "Year")

pdf("ImrMaine.pdf", width = 9, height = 6.5)
imrme
dev.off()

## Graph of Maine in comparable states, IMR
imr.comp<- ggplot(data=compsts, aes(year, imr, group=state))  +
    geom_point() +
    geom_smooth(method = "lm", color = "red", se=F) +
    geom_smooth(method="lm", aes(), data=filter(compsts, year<= 2004), se=F) +
    geom_smooth(method="lm", data=filter(compsts, year > 2004), se=F) +
    geom_line(linetype=3) +
    facet_wrap(~state) +
    ggtitle(label="Infant Mortality in selected states, 1995-2014", 
            subtitle="Subset by decades: 1995-2004, 2005-2014") +
    labs(y = "Infant Mortality (per 1000 live births)", x = "Year")  +  
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.75) ) 

pdf("ImrComp.pdf", width = 9, height = 6.5)
imr.comp
dev.off()

## Graph of Maine in New England
imr.ne<- ggplot(data=neweng, aes(year, imr, group=state))  +
    geom_point() +
    geom_smooth(method = "lm", color = "red", se=F) +
    geom_smooth(method="lm", aes(), data=filter(neweng, year<= 2004), se=F) +
    geom_smooth(method="lm", data=filter(neweng, year > 2004), se=F) +
    geom_line(linetype=3) +
    facet_wrap(~state) +
    ggtitle(label="Infant Mortality in selected states, 1995-2014", 
            subtitle="Subset by decades: 1995-2004, 2005-2014") +
    labs(y = "Infant Mortality (per 1000 live births)", x = "Year")  +  
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.75) ) 

pdf("ImrNewEng.pdf", width = 9, height = 6.5)
imr.ne
dev.off()
