library(stringr)
library(ggplot2)

#Extract data from raw wave files
suborn <- read.delim('E:/Summer 2017/2_Stats/suborn_responses.txt')
suborn$discourse <- str_replace(suborn$recordedFile, '.wav', '')
suborn$item<-factor(suborn$item_original)
suborn$Focus <- factor(suborn$condition, labels = c('Third','Second','First','Wide'))
suborn$Intonation <- 'Declarative'
suborn$Structure <- factor(suborn$bracketing, labels =c("(AB)C", "A(BC)"))

suborn <- suborn[,c('discourse', 'item', 'Focus', 'Intonation', 'Structure')]

suborq <- read.delim('E:/Summer 2017/2_Stats/suborq_responses.txt')
suborq$discourse <- str_replace(suborq$recordedFile, '.wav', '')
suborq$item<-factor(suborq$condition)
suborq$Focus <- factor(suborq$bracketing, labels = c('Third','Second','First','Wide'))
suborq$Intonation <- 'Interrogative'
suborq$Structure <- factor(suborq$order, labels =c("(AB)C", "A(BC)"))

suborq <- suborq[,c('discourse','item', 'Focus', 'Intonation', 'Structure')]

condition.data <- rbind(suborn, suborq)


#Combine the 2 datasets and remove null entries
interest_tracks <- read.csv('E:/Summer 2017/2_Stats/tracks.txt')
# summary(interest_tracks)

interest_tracks$duration <- interest_tracks$end - interest_tracks$begin
interest_tracks$norm_word_positon <- interest_tracks$word_position / interest_tracks$word_num_phones
interest_tracks$norm_time <- (interest_tracks$time - interest_tracks$begin) / interest_tracks$duration
interest_tracks$justified_time <- (interest_tracks$time - interest_tracks$begin) 
interest_tracks$utt_norm_time <- interest_tracks$norm_time + interest_tracks$utterance_position
interest_tracks$element_norm_time <- (interest_tracks$word_element_number + (interest_tracks$word_position - 1)  / interest_tracks$word_num_phones)+ interest_tracks$norm_time * (1/ interest_tracks$word_num_phones)
interest_tracks$phone_id <- paste(interest_tracks$phone, interest_tracks$discourse, as.character(interest_tracks$begin),sep='_')
interest_tracks <- merge(interest_tracks, condition.data)


interest_tracks[interest_tracks$Focus == 'Wide',]$element_norm_time <- interest_tracks[interest_tracks$Focus == 'Wide',]$element_norm_time + 2



completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
t_data = subset(interest_tracks, element_norm_time >= 4)
t_data=subset(t_data,item==1)


t_data_2 = subset(interest_tracks, element_norm_time >= 5&element_norm_time < 6)
t_data_2=subset(t_data_2,item==1)
t_data_2=completeFun(t_data_2,"F0_relative")

t_data=completeFun(t_data,"F0_relative")
summary(t_data)
ggplot(aes(x=justified_time, y = F0_relative), data= subset(t_data, phone_id == 'OW1_suborn_1067_1_3_5.69'))+ geom_point() + facet_grid(Focus~Structure*Intonation)   #+ scale_x_continuous(breaks=seq(1:7))
ggsave('utt_pitch_tracks.pdf', width = 8, height = 8)
