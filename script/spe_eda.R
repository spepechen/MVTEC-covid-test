#TODO: mouse over not working in plotly (can ignore tho)

# library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)


load(file="output/joined_data_by_isocode.RData")

View(joined)
View(ddExtra)

sapply(joined, class)


# location = international is all NA 
filtered_joined <- joined %>% 
  filter(location != "International" & location != "World")


# checking NA count in each column -------------------------
missing_value_rate_table  <- function(your_dataframe) {
  missing_value_rate <- colMeans(is.na(your_dataframe))*100
  
  df <- data.frame(missing_value_rate)
  
  # set index as column
  df <- cbind(col_name = rownames(df), df)
  rownames(df) <- 1:nrow(df)
  
  m <- df %>%
    rename(rate = missing_value_rate) %>%
    mutate(rate= round(rate, 2))%>% 
    arrange(desc(rate))
  
  return(m) 
}

#write.csv(m,'output/missing_value_rate.csv')

summary(joined$hosp_patients)


# Checking distribution of qualitative var -------------------------
# long categorical names are fine 
# as we can adjust a few things with ggplot to accommodate that 
# https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html
as.data.frame(table(ddExtra$Government_Type)) %>% 
  arrange(Freq) %>%  # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(Var1=factor(Var1, levels=Var1)) %>% # This trick update the factor levels
  ggplot() + 
  geom_col(aes(x=Freq, y=Var1)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title="Gov Type Count",
       y="")

# Plotting out missing value rate as heatmap
heatmap_maker <- function(...){
  # https://stackoverflow.com/questions/45134317/how-to-filter-a-data-frame-programmatically-with-dplyr-and-tidy-evaluation
  F <- quos(...) #quos captures ... as a list, without evaluating the arguments.
  g <- filtered_joined %>% 
    filter(!!!F) %>%  #!!! splices and unquotes the arguments for evaluation
    group_by(iso_code) %>%
    group_modify(~tibble(missing_value_rate_table(.))) %>%
    mutate(text = paste0("iso_code: ", iso_code, "\n", "rate: ", rate, "\n", "column: ", col_name)) # new column: text for tooltip:
  
  p <- ggplot(g, aes(x= reorder(iso_code, rate), y= reorder(col_name, rate), fill= rate)) + 
    geom_tile() +
    theme(axis.text.x = element_text(size=4, angle = 90),
          axis.text.y = element_text(size=5)) +
    labs( y="", x="") 
  
  return(p)
}

heatmap_maker(Country_Clasification == 'BRICS')
heatmap_maker(Country_Clasification == 'OECD')
heatmap_maker(Development_Status == 'Developed economies')
heatmap_maker(Development_Status == 'Developing economies')


# Wrap plot in plotly widget
plot_brics <- heatmap_maker(Country_Clasification == 'BRICS')
plotly_brics <- ggplotly(plot_brics, tooltip="text") 
saveWidget(plotly_brics, file="BRICS.html")


plot_oecd <- heatmap_maker(Country_Clasification == 'OECD')
plotly_oecd <- ggplotly(plot_oecd, tooltip="text") 
saveWidget(plotly_oecd, file="OECD.html")


plot_all <- heatmap_maker(location != "1111") #just to include everyone 
plotly_all <- ggplotly(plot_all, tooltip="text") 
saveWidget(plotly_all, file="all.html")


#IGNORE THE REST MESSY CODE 

# Plotting all countries ---------------------------------------
# groups <- filtered_joined %>% 
#   group_by(iso_code) %>%
#   group_modify(~tibble(missing_value_rate_table(.))) %>%
#   mutate(text = paste0("iso_code: ", iso_code, "\n", "rate: ", rate, "\n", "column: ", col_name)) # new column: text for tooltip:
# 
# # reordering heatmap
# # https://community.rstudio.com/t/ggplot2-heatmap-not-arranging-from-highest-to-lowest/22119
# p <- ggplot(groups, aes(x= reorder(iso_code, rate), y= reorder(col_name, rate), fill= rate)) + 
#   geom_tile() +
#   theme(axis.text.x = element_text(size=4, angle = 90),
#         axis.text.y = element_text(size=5)) +
#   labs(#title="Gov Type Count",
#        y="", x="") 
#   
# pp <- ggplotly(p, tooltip="text") 
# saveWidget(pp, file=paste0( getwd(), "MissingValueRate.html"))
# 
# 
# missing_value_rate_table(ddExtra)
# 
# subset(filtered_joined, select=-excluding_these)
# 
# # so much effort to create list of var to exclude
# # but no need for now....
# ddExtra_col <- c(names(ddExtra))  
# excluding4plot <- ddExtra_col[ddExtra_col != "GDP_per_unit_of_energy_use_constant" & ddExtra_col != "Urban_population_PC_of_total_population" ]
# to_remove <- append(excluding4plot, c("continent", "date", "location", "population"))
# `%ni%` <- Negate(`%in%`)
# # keep only more meaningful var 
# filtered_joined_subset <- subset(filtered_joined, select = names(filtered_joined) %ni% to_remove)


# save_plotly <- function(plot, filename){
#   p <- ggplotly(plot, tooltip="text") 
#   return(saveWidget(p, file=paste0(filename)))
# }
# 
# save_plotly(heatmap_maker(Country_Clasification == 'BRICS'), 
#             "BRICS.html")