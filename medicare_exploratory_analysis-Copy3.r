
library(tidyverse)
library(dplyr)
library(ggplot2)
library(zipcode)
data(zipcode)
library(VIM)
library(lubridate)

data1 <- read.csv("2015.csv", stringsAsFactors = FALSE, dec=",")
data2 <- read.csv("2016.csv", stringsAsFactors = FALSE, dec=",")
data3 <- read.csv("2017.csv", stringsAsFactors = FALSE, dec=",")

head(data2); # head(data3);  glimpse(data2)

data1$Year <- 2015
data2$Year <- 2016
data3$Year <- 2017

payment <- rbind(data1, data2, data3)
dim(payment)

payment$Average.Covered.Charges <- as.numeric(gsub("[\\$,]","", payment$Average.Covered.Charges))
payment$Average.Medicare.Payments <- as.numeric(gsub("[\\$,]","", payment$Average.Medicare.Payments))
payment$Average.Total.Payments <- as.numeric(gsub("[\\$,]","", payment$Average.Total.Payments))

sum(is.na(payment))

df_top15 <- payment %>% filter(Year == 2015) %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                        total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(Provider.State) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c))

df_top15 <- df_top15 %>% top_n(15, wt = total_charges) %>%  mutate(country_medicare_charges = sum(df_top15$total_charges), charges_proportion = total_charges/country_medicare_charges) %>%
  arrange(desc(total_charges))
knitr::kable(df_top15)

df_top16 <- payment %>% filter(Year == 2016) %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                        total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(Provider.State) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c)) 
df_top16 <- df_top16 %>%  top_n(15, wt = total_charges) %>%  mutate(country_medicare_charges = sum(df_top16$total_charges), charges_proportion = total_charges/country_medicare_charges) %>%
  arrange(desc(total_charges))
knitr::kable(df_top16)

df_top17 <- payment %>% filter(Year == 2017) %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                        total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(Provider.State) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c)) 
df_top17 <- df_top17 %>% top_n(15, wt = total_charges) %>%  mutate(country_medicare_charges = sum(df_top17$total_charges), charges_proportion = total_charges/country_medicare_charges) %>%
  arrange(desc(total_charges))
knitr::kable(df_top17)

nata <- ggplot(df_top15, 
       aes(x = reorder(Provider.State, charges_proportion), y = charges_proportion)) + 
  geom_bar(stat="identity", position="identity", fill=ifelse(df_top15$Provider.State == "NY", "#1380A1", "#dddddd")) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  theme_bw() +
  coord_flip() +
  labs(title="Medicare total charges",
       subtitle = "Highest total charges by State, 2015") +
  theme(panel.grid.major.x = element_line(color="#cbcbcb"), 
        panel.grid.major.y=element_blank())

santa <- ggplot(df_top16, 
       aes(x = reorder(Provider.State, charges_proportion), y = charges_proportion)) + 
  geom_bar(stat="identity", position="identity", fill=ifelse(df_top16$Provider.State == "NY", "#1380A1", "#dddddd")) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  theme_bw() +
  coord_flip() +
  labs(title="Medicare total charges",
       subtitle = "Highest total charges by State, 2016") +
  theme(panel.grid.major.x = element_line(color="#cbcbcb"), 
        panel.grid.major.y=element_blank())

marina <- ggplot(df_top17, 
       aes(x = reorder(Provider.State, charges_proportion), y = charges_proportion)) + 
  geom_bar(stat="identity", position="identity", fill=ifelse(df_top17$Provider.State == "NY", "#1380A1", "#dddddd")) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  theme_bw() +
  coord_flip() +
  labs(title="Medicare total charges",
       subtitle = "Highest total charges by State, 2017") +
  theme(panel.grid.major.x = element_line(color="#cbcbcb"), 
        panel.grid.major.y=element_blank())

#nata + coord_fixed(ratio=10)
#santa + coord_fixed(ratio=10)
#marina + coord_fixed(ratio=10)

options(repr.plot.width=3, repr.plot.height=3) 

nata; santa; marina

#ggarrange(nata, marina, santa) 
ggarrange(nata, marina, santa,
                    labels = c("A", "B", "C"),
                    ncol = 1, nrow = 3)

ggarrange(
  nata,                # First row with line plot
  # Second row with box and dot plots
  ggarrange(marina, santa, ncol = 2, labels = c("B", "C")), 
  nrow = 2, 
  labels = "A"       # Label of the line plot
  ) 

library(ggpubr)
theme_set(theme_pubr())

total.mediare.payments <- payment %>% group_by(Provider.State) %>% 
mutate(total.medicare.payments = sum(Average.Medicare.Payments))

head(total.mediare.payments)

us.total <- payment %>% group_by(Year) %>%
        summarise(
                  us.mean.total.discharges          = mean(Total.Discharges),
                  us.mean.average.covered.charges   = mean(Average.Covered.Charges),
                  us.mean.average.total.payments    = mean(Average.Total.Payments),
                  us.mean.average.medicare.payments = mean(Average.Medicare.Payments))
                  
dim(us.total); head(us.total)

options(repr.plot.width=4, repr.plot.height=4) 

a<-ggplot(data=us.total, aes(x=Year, y=us.mean.total.discharges)) +
  geom_bar(stat="identity", fill="#1380A1")+
  geom_text(aes(label=us.mean.total.discharges), vjust=1, color="white", size=2)+
  ggtitle("Total Discharges") +
  xlab("") + ylab("")+
  theme_minimal()+
theme(plot.title = element_text(color="black", size=14, face="bold.italic"))

b<-ggplot(data=us.total, aes(x=Year, y=us.mean.average.covered.charges)) +
  geom_bar(stat="identity", fill="#2E86C1")+
  geom_text(aes(label=us.mean.average.covered.charges), vjust=1, color="white", size=2)+
   ggtitle("Covered Charges") +
  xlab("") + ylab("")+
  theme_minimal()+
  theme(plot.title = element_text(color="black", size=14, face="bold.italic"))

c<-ggplot(data=us.total, aes(x=Year, y=us.mean.average.total.payments)) +
  geom_bar(stat="identity", fill="grey")+
   geom_text(aes(label=us.mean.average.total.payments), vjust=1, color="white", size=2)+
   ggtitle("Total Payments") +
  xlab("") + ylab("")+
  theme_minimal() + theme(plot.title = element_text(color="black", size=14, face="bold.italic"))

p<-ggplot(data=us.total, aes(x=Year, y=us.mean.average.medicare.payments)) +
  geom_bar(stat="identity", fill="black")+
 geom_text(aes(label=us.mean.average.medicare.payments), vjust=1.6, color="white", size=2)+
 ggtitle("Medicare Payments") +
  xlab("") + ylab("")+ labs(title = "Medicare Payments",
        subtitle = "Medicare payments increased from 1122 to 79998  this is the 8%")+
  theme_minimal() + theme(
 plot.title = element_text(color="black", size=14, face="bold.italic"), plot.subtitle = element_text(hjust = 0.5, size=6, color = "black", face = "bold.italic"))
      
      
#sul <- c('2015', '2016', '2017')
#p <- ggplot(data=us.total, aes(x=Year, y=us.mean.average.medicare.payments, fill=sul)) +
#  geom_bar(stat="identity", position=position_dodge())+
#  scale_fill_brewer(palette="Paired")+
#  theme_minimal()

#ggarrange(nata, marina, santa) 
ggarrange(a, b, c, p,
                   # labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2, widths = 0.2, heights = 0.5) 

                             #tu gvinda amovatrialot  + coord_flip()

by.state <- payment %>% group_by(Provider.State) %>% filter(Year == 2017) %>%
        summarise(count =n(),
                  state.mean.total.discharges          = mean(Total.Discharges),
                  state.mean.average.covered.charges   = mean(Average.Covered.Charges),
                  state.mean.average.total.payments    = mean(Average.Total.Payments),
                  state.mean.average.medicare.payments = mean(Average.Medicare.Payments))
                  
                                            
dim(by.state); head(by.state)

by.state$us.mean.total.discharges <- us.total$us.mean.total.discharges
by.state$us.mean.average.covered.charges <- us.total$us.mean.average.covered.charges
by.state$us.mean.average.total.payments <- us.total$us.mean.average.total.payments
by.state$us.mean.average.medicare.payments <- us.total$us.mean.average.medicare.payments

state.average.ratios <- by.state %>% group_by(Provider.State) %>%
 mutate(ratio.state.discharges = state.mean.total.discharges/us.mean.total.discharges,
       ratio.covered.charges = state.mean.average.covered.charges/us.mean.average.covered.charges,
       ratio.total.payments = state.mean.average.total.payments /us.mean.average.total.payments,
       ratio.medicare.payments = state.mean.average.medicare.payments/us.mean.average.medicare.payments)

head(by.state)

us.states <- state.average.ratios %>%
        select(Provider.State, ratio.covered.charges) %>%
        arrange(ratio.covered.charges) %>%
        mutate(Avg = mean(1, na.rm = TRUE),
               Ratio = ifelse(ratio.covered.charges - Avg > 0, "Above the Countrie's Average", "Below the Countrie's Average")) %>%
arrange(desc(ratio.covered.charges))
dim(us.states)
head(us.states, 15)

covered.charges <- ggplot(us.states, aes(ratio.covered.charges, Provider.State, color = Ratio)) +
        geom_segment(aes(x = Avg, y = Provider.State, xend = ratio.covered.charges, yend = Provider.State), color = "grey50") +
        geom_point()+ 
        labs(title = "Total Covered Charges by State")+
        theme_minimal() +
        theme(axis.title = element_blank())

us.states <- state.average.ratios %>%
        select(Provider.State, ratio.medicare.payments) %>%
        arrange(ratio.medicare.payments) %>%
        mutate(Avg2 = mean(1, na.rm = TRUE),
               Ratio = ifelse(ratio.medicare.payments - Avg2 > 0, "Above the Countrie's Average", "Below the Countrie's Average"))%>%
             arrange(desc(ratio.medicare.payments))
head(us.states, 15)

medicare.payments <- ggplot(us.states, aes(ratio.medicare.payments, Provider.State, color = Ratio)) +
        geom_segment(aes(x = Avg2, y = Provider.State, xend = ratio.medicare.payments, yend = Provider.State), color = "grey50") +
        geom_point()+ 
        labs(title = "Medicare Payments by State")+
        theme_minimal() +
        theme(axis.title = element_blank()) 
#medicare.payments

require(gridExtra)

grid.arrange(medicare.payments, covered.charges, ncol=2)


library(cluster)


payment <- as.data.frame(payment)

p1 <- boxplot(payment$Total.Discharges ~ payment$Provider.State, par(las=3), 
        xlab='Cluster', ylab='Medicare payments',
        main='Medicare payments ty cluster')

df_top <- payment %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                        total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(Provider.Name) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c)) %>% 
  top_n(10, wt = total_charges) %>%
  arrange(desc(total_charges))

knitr::kable(df_top)

df_top %>% ggplot(aes(x=reorder(Provider.Name, total_charges), y=total_charges)) +
  geom_bar(stat='identity') +
  xlab("Provider") +
  ylab("Total charges by Provider ($)") +
  ggtitle("Providers that billed the most in Medicare") +
  coord_flip()

df_top <- payment %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                        total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(DRG.Definition) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c)) %>% 
  top_n(10, wt = total_charges) %>%
  arrange(desc(total_charges))

knitr::kable(df_top)

df_top %>% ggplot(aes(x=reorder(DRG.Definition, total_charges), y=total_charges)) +
  geom_bar(stat='identity') +
  xlab("DRG") +
  ylab("Total charges by DRG ($)") +
  theme(axis.text = element_text(size = 8)) +
  ggtitle("Most billed DRG") +
  coord_flip()

df_top %>% arrange(desc(total_paid)) %>% 
  ggplot(aes(x=reorder(DRG.Definition, total_paid), y=total_paid)) +
  geom_bar(stat='identity') +
  xlab("DRG") +
  ylab("Total paid by DRG ($)") +
  theme(axis.text = element_text(size = 8)) +
  ggtitle("Most paid DRG") +
  coord_flip()


df_top <- payment %>% mutate(total_c = Average.Covered.Charges*Total.Discharges,
                        total_p = Average.Medicare.Payments*Total.Discharges) %>% 
  group_by(DRG.Definition) %>%
  summarise(total_charges=sum(total_c), total_paid = sum(total_p),
            ratio_paid_charged = sum(total_p)/sum(total_c)) %>%
  arrange(desc(total_charges))

knitr::kable(df_top)

drg.by.state <- payment %>% group_by(DRG.Definition, Provider.State, Year) %>%
        summarise(count =n(),
                  State.mean.total.discharges          = mean(Total.Discharges),
                  State.mean.average.covered.charges   = mean(Average.Covered.Charges),
                  State.mean.average.total.payments    = mean(Average.Total.Payments),
                  State.mean.average.medicare.payments = mean(Average.Medicare.Payments))
                 
dim(drg.by.state); 

#head(drg.by.state)

usstates1 <- payment %>%
  group_by(Provider.State) %>% filter(Year == 2015) %>%
  summarize(totalcost = sum(Average.Covered.Charges, na.rm =TRUE))%>%
  mutate(percent = (totalcost/sum(totalcost))*100)%>%
  arrange(desc(totalcost))

head(usstates1, 10)
usstates1 <- payment %>%
  group_by(Provider.State) %>% filter(Year == 2016) %>%
  summarize(totalcost = sum(Average.Covered.Charges, na.rm =TRUE))%>%
  mutate(percent = (totalcost/sum(totalcost))*100)%>%
  arrange(desc(totalcost))

head(usstates1, 10)

usstates1 <- payment %>%
  group_by(Provider.State) %>% filter(Year == 2017) %>%
  summarize(totalcost = sum(Average.Covered.Charges, na.rm =TRUE))%>%
  mutate(percent = (totalcost/sum(totalcost))*100)%>%
  arrange(desc(totalcost))

head(usstates1, 10)

my.bloody.theme <- within(trellis.par.get(), {    # Initialize theme with default value
  axis.line$col <- NA                             # Remove axes
  plot.polygon <- within(plot.polygon, {
    col <- "#8A0606"                              # Set bar colors to a nice bloody red
    border <- NA                                  # Remove bars' outline
  })
  axis.text$cex <- 1                              # Default axis text size is a bit small. Make it bigger
  layout.heights <- within(layout.heights, {
    bottom.padding <- 0                           # Remove bottom padding
    axis.bottom <- 0                              # Remove axis padding at the bottom of the graph
    axis.top <- 0                                 # Remove axis padding at the top of the graph
  })
})

par(mar = c(5, 8, 4, 2) + 0.2)

# Specify colours for the bars and bar borders
border.vec <- color.vec <- rep("grey", 53)
# Specify colours of text labels for the bars
country.vec <- rep("black", 53) 

# Change bar and text colour to red for United Kingdom
index <- which(usstates1$Provider.State == "NY")
border.vec[index] <- 
  color.vec[index] <- 
  country.vec[index] <- "red"

# leave out x-axis labelling in the main barplot...
bp <- barplot(usstates1$percent, 
        las = 1,
        xlab="Gini coefficient",
        horiz = TRUE,
        border = border.vec,
        col = color.vec,
        xlim = c(0, 0.5),
        xaxp = c(0, 0.5, 5))
abline(v=seq(0, 0.4, by=0.1), col='white', lwd=2)

#... and add it in manually at the end
text(y = bp, x = -0.01, 
     labels = usstates1$Provider.State ,
      xpd = TRUE, col = country.vec, adj = 1)

attach(usstates1)
       par(mar = c(5, 8, 4, 2) + 0.1)
barplot(percent, 
        names.arg = Provider.State,
        xlab="States",
        horiz = TRUE,
        las = 1)

graph <- barchart(Provider.State ~ percent, data = usstates1, 
                   decreasing = TRUE, box.width=0.75, main='Total Cost By State in 2017')
print(graph)

graph <- update(
  graph,
  main='25 most violence packed films by deaths per minute',    # Title of the barchart
  par.settings = my.bloody.theme,                               # Use custom theme
  xlab = NULL,                                                  # Remove label of x axis
  scales=list(x=list(at=NULL)),                                 # Remove rest of x axis
  xlim = c(0, 6.7),                                             # Set graph limits along x axis to accomodate the additional text (requires some trial and error)
  box.width=0.75)                                               # Default bar width is a bit small. Make it bigger)

print(graph)

nata <- ggplot(usstates1, aes(percent, Provider.State)) +
        geom_segment(aes(x = 0, y = Provider.State, xend = percent, yend = Provider.State), color = "grey50") +
        geom_point()
nata

nata <- ggplot(usstates1, aes(percent, Provider.State)) +
        geom_segment(aes(x = 0, y = Provider.State, xend = percent, yend = Provider.State), color = "grey50") +
        geom_point()
nata

mapk <- usstates1 %>%
        select(Provider.State, totalc) %>%
        arrange(Provider.State) %>%
        mutate(Avg = mean(totalc, na.rm = TRUE),
               Above = ifelse(totalc - Avg > 0, TRUE, FALSE),
               county = factor(Provider.State, levels = .$Provider.State))

head(mapk)

ggplot(mapk, aes(totalc, Provider.State, color = Above)) +
        geom_segment(aes(x = Avg, y = Provider.State, xend = totalc, yend = Provider.State), color = "grey50") +
        geom_point()

library(lattice)        # Very versatile graphics package
library(latticeExtra)

head(kak)

graph <- barchart(DRG.Definition ~ total.discharges, data = kak)
print(graph)

kak <- head(top10totaldischarges17, 10)

top10totaldischarges15 <- payment %>% filter(Year == 2015) %>% select(DRG.Definition, Total.Discharges) %>%
group_by(DRG.Definition) %>% 
summarise(total.discharges = sum(Total.Discharges)) %>%
mutate(percent = total.discharges/sum(total.discharges))%>%
arrange(desc(total.discharges))
head(top10totaldischarges15, 10)

top10totaldischarges16 <- payment %>% filter(Year == 2016) %>% select(DRG.Definition, Total.Discharges) %>%
group_by(DRG.Definition) %>% 
summarise(total.discharges = sum(Total.Discharges)) %>%
mutate(percent = total.discharges/sum(total.discharges))%>%
arrange(desc(total.discharges))
head(top10totaldischarges16, 10)

top10totaldischarges17 <- payment %>% filter(Year == 2017) %>% select(DRG.Definition, Total.Discharges) %>%
group_by(DRG.Definition) %>% 
summarise(total.discharges = sum(Total.Discharges)) %>%
mutate(percent = total.discharges/sum(total.discharges))%>%
arrange(desc(total.discharges))
head(top10totaldischarges17, 10)
