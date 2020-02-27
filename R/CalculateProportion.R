###############################################################################
#
# L1_I1_prop 	Per item zou een proportie correct score berekend moeten worden
#             (aantal goede antwoorden / door aantal antwoorden)> dus hier (L1_I1_Q1)/1
# meanprop
###############################################################################

calculate_proportion <- function(mydata) {
    participants<-unique(mydata$participant_id)
    tasks<-unique(mydata$task_id)
    rowNewData = 0
    newData <- data.frame(matrix(ncol = 0, nrow = 0))


    for (participant in participants) {
        for (task in tasks) {
            data = NULL
            # select participant and task
            data = subset(mydata, task_id == task)
            data = subset(data, participant_id == participant)
            data = subset(data, level!=0)

            if(nrow(data) == 0 ) {
              # when length is zero, no results found for this participant and task
              next
            }

            # calculate mean per level/item
            scores<-tapply(as.numeric(data$score),  list(data$level, data$item), sum, na.rm=TRUE)
            questions<-tapply(data$question_number,  list(data$level, data$item), max)
            levels<-unique(data$level)
            items<-unique(data$item)

            rowNewData = rowNewData+1

            for (level in levels) {
                for (item in items) {
                    propColumnName = paste('L', level, '_I', item, "_prop", sep = "")
                    if(questions[level, item] == 0) {
                        newData[rowNewData, propColumnName] = NA
                        next
                    }
                    newData[rowNewData, propColumnName] = scores[level, item]/questions[level, item]
                }
            }
            newData[rowNewData, 'totalscore'] = sum(as.numeric(data$score))
            newData[rowNewData, 'totalquestions'] = nrow(data)
            newData[rowNewData, 'task_id'] = task
            newData[rowNewData, 'participant_id'] = participant
        }
    }
    return(newData[order(
        newData$task_id,
        newData$participant_id),])
}
