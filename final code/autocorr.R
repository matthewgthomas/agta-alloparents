#####################################
#-----------------------------------------------------------------------------------------------
### MAKE GRAPHS TO EXPLORE AUTOCORRELATION 
#-----------------------------------------------------------------------------------------------
#####################################

#WHICH EGOS TO USE: 
#Mote --- Name ---- ID
#57 Jemmy Donato [A66016]
#56 Karen Donato [A66018]
#05 Laila Plata [A66041]
#01 Dario Donato [A66004]
#27 Neneng Malacaster [A66030]
#247 Elmer Donato [A66034]
#237 Joey Donato [A66015]


#load data 
messagesdiago1 <- read_csv("data/autocorr/messagesdiago1.csv")
require(ggplot2)
require(cowplot)

#subset to only the above senders
messages <- subset(messagesdiago1, sender_id == 56 | sender_id == 05
                   | sender_id == 247
                   | sender_id == 19)

messages57 <- subset(messagesdiago1, sender_id == 57)
messages27 <- subset(messagesdiago1, sender_id == 27)
messages5 <- subset(messagesdiago1, sender_id == 5)
messages19 <- subset(messagesdiago1, sender_id == 19)


messages$receiver_id <- as.factor(messages$receiver_id)
messages$sender_id <- as.factor(messages$sender_id)

ggplot(messages5, aes(x=local_time, y=receiver_id, color=sender_id)) + 
  geom_point(alpha = 0.5) 