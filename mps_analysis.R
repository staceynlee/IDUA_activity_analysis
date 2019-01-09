# Load libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotrix)
library(gcookbook)
library(data.table)

# Import MPS-I csv data
df = read.csv("MPS1-Data-From-20160811.csv", header = TRUE)

# Cleanup data

# Remove retest columns
df1 = select(df, -GAA_P_IS_R, -GAA_P_IS_R_Percent, -GAA_P_IS_R_Activity, -AverageBLANKQC_IDUA_2_P_IS_R, -IDUA_2_P_IS_R, 
       -IDUA_2_P_IS_R_Percent, -IDUA_2_P_IS_R_Activity, -AverageBLANKQC_ABG_P_IS_R, -ABG_P_IS_R, -ABG_P_IS_R_Percent, 
       -ABG_P_IS_R_Activity)

# Remove duplicate lab numbers
df2 = df1[!duplicated(df1$labnumber), ]

# Remove unsats
df3 = df2[!grepl("UNS", df2$ReportAs), ]

# Remove nulls in IDUA_P_IS_R
df4 = df3[!grepl("NULL", df3$IDUA_P_IS_R), ]

# Remove nulls in birth weight
df5 = df4[!grepl("NULL", df4$WEIGHTGMS), ]

# Write CSV from df5
write.csv(df5, file = "MPS_edited_data.csv")

# Descriptive statistics

# Coefficient of variation function

CV <- function(mean, sd){
  (sd/mean)*100
}

# IDUA
df5$IDUA_P_IS_R = as.numeric(as.character(df5$IDUA_P_IS_R))
summarise(df5, Mean = mean(IDUA_P_IS_R), Median = median(IDUA_P_IS_R), SD = sd(IDUA_P_IS_R), 
          SE = std.error(IDUA_P_IS_R), CV = CV(Mean, SD))


df5$IDUA_P_IS_R_Activity = as.numeric(as.character(df5$IDUA_P_IS_R_Activity))
summarise(df5, Mean = mean(IDUA_P_IS_R_Activity), Median = median(IDUA_P_IS_R_Activity), SD = sd(IDUA_P_IS_R_Activity), 
          SE = std.error(IDUA_P_IS_R_Activity), CV = CV(Mean, SD))

# Total Males vs Females
table(df5$sex)

# Create % data
a = table(df5$sex)
z = as.data.frame(round((a/sum(a)) * 100, digits = 2))
z = filter(z, Freq > 0)

# Plots
# Sex
ggplot(df5) + geom_bar(aes(x=sex, y=frequency(df5$sex), fill=sex), stat="identity") +
  labs(title="Samples By Sex") + labs(x="Sex", y="Count") + 
  geom_text(data=z,aes(x=Var1,y=Freq,label=Freq),vjust=-0.5)

# Birthweight
df5$WEIGHTGMS = as.numeric(as.character(df5$WEIGHTGMS))
bw_breaks = c(0, 1000, 1500, 2500, 10000)
bw_labels = c("Extremely Low Birth Weight", "Very Low Birth Weight", "Low Birth Weight", "Normal Birth Weight")

data <- as.data.table(df5)
setDT(data)[ , bwgroups := cut(df5$WEIGHTGMS, breaks = bw_breaks, right = FALSE, labels = bw_labels)]

table(data$bwgroups)
ggplot(data, aes(x=bwgroups, fill=bwgroups)) + geom_bar(stat="count") +
  labs(title="Samples By Birth Weight") + labs(x= "Birth Weight Categories", y="Number of Samples") + 
  theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1))

# Collection Age
df5$COLLECTIONAGE = as.numeric(as.character(df5$COLLECTIONAGE))
ca_breaks = c(0, 24, 48, 72, 96, 120, 144, 168, 192, 216, 240, 264, 288, 312, 336, 1080, 1824, 2568, 3312, 4801)
ca_labels = c("Less than 24hr", "24-48hr", "49-72hr", "73-96hr", "4-5 days", "5-6 days", 
              "6-7 days", "7-8 days", "8-9 days", "9-10 days", "10-11 days", "11-12 days", 
              "12-13 days", "13-14 days", "14-45 days", "46-76 days", "77-107 days", "108-138 days", "Above 138 days")

data <- as.data.table(df5)
setDT(data)[ , cagroups := cut(df5$COLLECTIONAGE, breaks = ca_breaks, right = FALSE, labels = ca_labels)]

table(data$cagroups)
ggplot(na.omit(data), aes(x=cagroups, fill=cagroups)) + geom_bar(stat="count") +
  labs(title="Samples By Collection Age") + labs(x= "Collection Age", y="Number of Samples") + 
  theme(legend.position="none", axis.text.x = element_text(angle = 90, hjust = 1))

# IDUA Activity by Sex
aggregate(IDUA_P_IS_R_Activity ~ sex, df5, mean)
ggplot(df5, aes(x=factor(sex), y=IDUA_P_IS_R_Activity, fill=sex)) + stat_summary(fun.data = mean_se, geom = "errorbar") + 
  stat_summary(fun.y="mean", geom="bar") + labs(title="IDUA Activity By Sex") + 
  labs(x= "Sex", y="IDUA Activity (uM/L/hr)")

# IDUA Activity by Birth Weight
aggregate(IDUA_P_IS_R_Activity ~ bwgroups, data, mean)
ggplot(data, aes(x=factor(bwgroups), y=IDUA_P_IS_R_Activity, fill=bwgroups)) + stat_summary(fun.data = mean_se, geom = "errorbar") + 
  stat_summary(fun.y="mean", geom="bar") + labs(title="IDUA Activity By Birth Weight") + 
  labs(x= "Birth Weight Category", y="IDUA Activity (uM/L/hr)") + theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1))

# Subset Normal BW
nbw_df = filter(data, bwgroups == "Normal Birth Weight")

# Histogram of Normal BW IDUA Activity
hist(nbw_df$IDUA_P_IS_R_Activity, breaks= seq(0, 30, by=0.5), ylim=c(0,4000), xlab="IDUA Activity", main="", col="grey50")
# minor.tick(nx=5, tick.ratio=0.5)

hist(nbw_df$IDUA_P_IS_R_Activity, breaks= seq(0, 30, by=0.5), ylim=c(0,5), xlab="IDUA Activity", main="", col="grey50")

ggplot(data=nbw_df, aes(nbw_df$IDUA_P_IS_R_Activity)) + geom_histogram(breaks= seq(0, 30, by=0.5), fill = "grey50", colour = "black") + 
  labs(x= "IDUA Activity", y="Frequency") + scale_x_continuous(breaks=seq(0,30,5), minor_breaks = seq(0, 30, 1), expand = c(0,0)) + theme_bw()

ggplot(data=nbw_df, aes(nbw_df$IDUA_P_IS_R_Activity)) + geom_histogram(breaks= seq(0, 30, by=0.5), fill = "grey50", colour = "black") + 
  labs(x= "IDUA Activity", y="Frequency") + scale_x_continuous(breaks=seq(0,30,5), minor_breaks = seq(0, 30, 1), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                  panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(data=nbw_df, aes(nbw_df$IDUA_P_IS_R_Activity)) + 
  geom_histogram(breaks= seq(0, 2, by=0.5), fill = "grey50", colour = "black") + 
  labs(x= "IDUA Activity", y="Frequency") + 
  scale_x_continuous(breaks=seq(0,2,0.5), minor_breaks = seq(0, 2, 0.25), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,3), expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

