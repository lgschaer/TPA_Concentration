#Load data for TPA Degradation Measurements
##Your data should be formatted in a CSV file
###Columns: SampleID, Enrichment Name, Substrate, Time, Replicate, Absorbance
tpadeg <- as.csv("/Users/lgschaer/Desktop/MTU_Projects/TA_TPA/03262021_TPADegradation.csv", row.names = 1, header = TRUE, sep = ",", check.names = TRUE, stringsAsFactors = TRUE)
head(tpadeg)

tpadeg2 <- tpadeg %>%
  group_by(Sample, Substrate, Time) %>%
  mutate(
    tpaConc = (Absorbance-0.461)/0.1152
  ) %>%
  summarise(
    maxConc = max(tpaConc),
    minConc = min(tpaConc),
    avgConc = mean(tpaConc))
head(tpadeg2)

colors <- c("lightblue","Blue", "Purple", "Red", "Orange", "Green", "Magenta", "Grey", "Black")
colors2 <- c("Blue", "Purple", "Red", "Orange", "Green", "Magenta", "Grey", "Black")

#plot with ggplot
ggplot(tpadeg2, mapping = aes(x = Sample, y = avgConc, 
                              fill = Sample), show.legend = TRUE)+
  geom_errorbar(aes(ymax = maxConc, ymin = minConc), color = "black", width = 0.5)+
  geom_col(color = "black")+
  facet_grid(cols = vars(Time))+
  ylab("Concentration TPA (g/L)") +
  xlab("Consortia")+
  scale_fill_manual(values = colors2) +
  theme_linedraw()+
  theme(strip.text = element_text(face = "bold", size = 12),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 15),
        axis.text.y.left = element_text(size = 20),
        axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, vjust = 0.5),
        axis.title.y = element_text(size = 20, hjust = 0.5),
        title = element_text(size = 25))
