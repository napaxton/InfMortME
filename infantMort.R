library(tidyverse); library(dplyr)

##  Load data ##
bdn <- read_tsv("https://github.com/jakeemerson/maine_infant_mortality_data/raw/master/data/us_infant_mortality_2.tsv")
# bdn <- read_tsv("C:/Users/np45344/DataAnalysis/InfantMort/us_infant_mortality_2.tsv")


##  Transform all data  ##
bdn <- bdn %>%
    gather(datecat, measure, 2:61, convert=T) %>%
    separate(datecat, c("year","infant"), sep="_", convert=T) %>%
    spread(infant, measure)

# as.Date(bdn$year, origin="1995") -> bdn$year

# names(bdn) <- c("state","year","births","deaths","imr")
write_csv(bdn, "infMortStateYear.csv")


by_state <- group_by(bdn, state)
sum_state <- summarize(by_state, mean(imr))

## Subset of rural, white states
states=c("Maine","Iowa","West Virginia","Idaho", "New Hampshire","Vermont")
bdn.sub <- filter(bdn, state %in% states)

lm04 <- lm(imr ~ year, data=filter(bdn, state == "Maine", year <= 2004 ))
lm14 <- lm(imr ~ year, data=filter(bdn, state == "Maine", year > 2004 ))

# ### Maine data only ###
# filter(bdn, X1 == "Maine") -> bdn.me
# select(bdn.me, ends_with("_IMR")) -> bdn.me
# gather(bdn.me) -> bdn.me
# rename(bdn.me, imr = value) -> bdn.me
# bdn.me <- bdn.me %>%
#   separate(key, sep=4, into=c("year","drop")) %>%
#   select(-drop)
# bdn.me <- transform(bdn.me, year = as.numeric(year))

# m <- qplot(year, imr, data=bdn.me, geom = "smooth", method= "lm", formula= y~x)

# m2 <- ggplot(data=bdn.me, aes(year, imr))  +
#   geom_point() +
#   geom_smooth(method = "lm", color = "red") +
#   geom_smooth(method="lm", aes(), data=filter(bdn.me, year<= 2004)) +
#   geom_smooth(method="lm", data=filter(bdn.me, year >= 2005))

m3 <- ggplot(data=bdn.sub, aes(year, imr, group=state))  +
    geom_point() +
    geom_smooth(method = "lm", color = "red") +
    geom_smooth(method="lm", aes(), data=filter(bdn.sub, year<= 2004)) +
    geom_smooth(method="lm", data=filter(bdn.sub, year > 2004)) +
    facet_wrap(~state) +
    ggtitle(label="Infant Mortality in selected states, 1995-2014",
            subtitle="Subset by decades: 1995-2004, 2005-2014") +
    labs(y = "Infant Mortality (per 1000 live births)", x = "Year")

m3 +  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.75) )
m3 + scale_x_date(date_breaks="2 years")

# # Function version of ggplot call above? 
# mortPlot <- function(data, x, y, method = "lm", color = "red", cutyear=2004, facet=state){
#     ggplot2::ggplot(data, aes(x,y)) +
#         geom_point() +
#         geom_smooth(method = "lm", color = "red") +
#         geom_smooth(method="lm", aes(), data=filter(data, year<= cutyear)) +
#         geom_smooth(method="lm", data=filter(data, year > cutyear)) +
#         facet_wrap(~facet) +
#         ggtitle(label="Infant Mortality in selected states, 1995-2014",
#                 subtitle="Subset by decades: 1995-2004, 2005-2014") +
#         labs(y = "Infant Mortality (per 1000 live births)", x = "Year")
# }
