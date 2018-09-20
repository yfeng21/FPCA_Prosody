library(stringr)
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
