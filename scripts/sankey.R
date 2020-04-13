library(networkD3)
library(tidyverse)
library(lubridate)

#sankey diagram (2 layers)
links = data.frame(
  source = c("Primary 1","Primary 2", "Primary 3", "Primary 4", "Primary 5", "Primary 6", "Primary 7", "Primary 8",
             "Sub_1_1", "Sub_1_2", "Sub_1_3", "Sub_1_4", "Sub_1_5", "Sub_1_6", #Primary 1
             "Sub_2_1", "Sub_2_2", "Sub_2_3", "Sub_2_4", #Primary 2
             "Sub_3_1", "Sub_3_3", "Sub_3_3", #Primary 3
             "Sub_4_1", "Sub_4_2", "Sub_4_3", "Sub_4_4", #Primary 4
             "Sub_5_1", "Sub_5_2", "Sub_5_3", "Sub_5_4", #Primary 5
             "Sub_6_1", "Sub_6_2", "Sub_6_3", "Sub_6_4", "Sub_6_5",  #Primary 6
             "Sub_7_1", "Sub_7_2", "Sub_7_3", "Sub_7_4", "Sub_7_5", #Primary 7
             "Sub_8_1", "Sub_8_2", "Sub_8_3", "Sub_8_4", "Sub_8_5", "Sub_8_6" #Primary 8
  ), 
  target = c("Primary 1","Primary 1", "Primary 1", "Primary 1", "Primary 1", "Primary 1", "Primary 1", "Primary 1",
             "Primary 2", "Primary 2", "Primary 2", "Primary 2", "Primary 2", "Primary 2",
             "Primary 3", "Primary 3", "Primary 3", "Primary 3",
             "Primary 4", "Primary 4", "Primary 4",
             "Primary 5", "Primary 5", "Primary 5", "Primary 5",
             "Primary 6", "Primary 6", "Primary 6", "Primary 6", 
             "Primary 7", "Primary 7", "Primary 7", "Primary 7", "Primary 7",
             "Primary 8", "Primary 8", "Primary 8", "Primary 8", "Primary 8",
             "Primary 9", "Primary 9", "Primary 9", "Primary 9", "Primary 9", "Primary 9" 
  ), 
  value = c(12, 6, 35, 6, 230, 5, 32, 84,
            2, 4, 2, 2, 1, 1,
            1, 2, 1, 2, 
            28, 5, 2,
            3, 1, 1, 1, 
            1, 1, 2, 1, 
            18, 2, 2, 2, 8,
            25, 10, 4, 4, 41,
            46, 38, 35, 15, 30, 37)
)


nodes=data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% unique())
links$IDsource=match(links$source, nodes$name)-1 
links$IDtarget=match(links$target, nodes$name)-1

# Add a 'group' column to each connection:
links$group=as.factor(c("Primary 1","Primary 2", "Primary 3", "Primary 4", "Primary 5", "Primary 6", "Primary 7", "Primary 8",
                        "direct_mail2", "direct_mail3",  "direct_mail4",  "direct_mail5",  "direct_mail6",  "direct_mail7",
                        "events2", "events3", "events4", "events5",
                        "internet2", "internet3", "internet4", 
                        "newspaper2", "newspaper3", "newspaper4", "newspaper5",
                        "radio2", "radio3", "radio4", "radio5",
                        "referral2", "referral3", "referral4", "referral5", "referral6",
                        "tech_lead2", "tech_lead3", "tech_lead4", "tech_lead5", "tech_lead6",
                        "PastCustomer0", "PastCustomer13","PastCustomer46","PastCustomer79","PastCustomer1012","PastCustomer1316"
))

# Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
nodes$group=as.factor(c("my_unique_group"))

# Give a color for each group:
my_color <- 'd3.scaleOrdinal() 
.domain(["direct_mail","events","internet","newspaper","past_customer","radio", "referral", "tech_lead", 
                        "direct_mail2", "direct_mail3",  "direct_mail4",  "direct_mail5",  "direct_mail6",  "direct_mail7",
                        "events2", "events3", "events4", "events5",
                        "internet2", "internet3", "internet4", 
                        "newspaper2", "newspaper3", "newspaper4", "newspaper5",
                        "radio2", "radio3", "radio4", "radio5",
                        "referral2", "referral3", "referral4", "referral5", "referral6",
                        "tech_lead2", "tech_lead3", "tech_lead4", "tech_lead5", "tech_lead6",
                        "PastCustomer0", "PastCustomer13","PastCustomer46","PastCustomer79","PastCustomer1012","PastCustomer1316",
  "my_unique_group"]) 
.range(["#4292c6", "#8c96c6", "#807dba", "#f16913", "#74c476", "#6baed6", "#4292c6", "#cb181d", 
  "#6baed6", "#9ecae1", "#6baed6", "#9ecae1", "#6baed6", "#9ecae1",
  "#ef3b2c", "#fb6a4a", "#fc9272", "#fb6a4a",
  "#9e9ac8", "#bcbddc", "#bcbddc",
  "#fd8d3c",  "#fd8d3c",  "#fd8d3c",  "#fd8d3c",
  "#6baed6", "#c6dbef", "#c6dbef", "#c6dbef",
  "#6baed6", "#c6dbef", "#c6dbef", "#c6dbef", "#c6dbef",
  "#ef3b2c", "#fb6a4a", "#fc9272", "#fb6a4a", "#fc9272", 
  "#74c476", "#74c476","#74c476","#74c476","#74c476","#74c476",
  "#ffffff"])' 

# Make the Network
sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", Value = "value", 
              NodeID = "name", colourScale = my_color, LinkGroup="group", NodeGroup="group", fontSize = 12)



