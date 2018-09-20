library(ggplot2)

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

stressed_time=c()
stressed_time_list=list()
i=1
for (phone in levels(factor(sub_t_data$phone[which(grepl(1, sub_t_data$phone))]))){
  stressed_time=c(stressed_time,min(sub_t_data$element_norm_time[which(sub_t_data$phone==phone)]))
}
stressed_time_list[[i]]=stressed_time


completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
t_data = subset(interest_tracks, element_norm_time >= 4)
t_data=subset(t_data,item==1)
t_data=completeFun(t_data,"F0_relative")
summary(t_data)
ggplot(aes(x=justified_time, y = F0_relative), data= subset(t_data, phone_id == 'OW1_suborn_1067_1_3_5.69'))+ geom_point() + facet_grid(Focus~Structure*Intonation)   #+ scale_x_continuous(breaks=seq(1:7))
ggsave('utt_pitch_tracks.pdf', width = 8, height = 8)
