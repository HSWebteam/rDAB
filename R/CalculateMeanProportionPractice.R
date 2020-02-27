###############################################################################
#
# L1_I1_prop 	Per item zou een proportie correct score berekend moeten worden
#             (aantal goede antwoorden / door aantal antwoorden)> dus hier (L1_I1_Q1)/1
# meanprop
###############################################################################

getAveragePractice <- function(mydata){
    rowNewData = 1
    participants<-unique(mydata$participant_id)
    tasks<-unique(mydata$task_id)
    newData <- data.frame(matrix(ncol = 0, nrow = 0))

    for (participant in participants) {
        for (task in tasks) {
            # select participant and task
            data = subset(mydata, task_id == task)
            data = subset(data, participant_id == participant)
            task_type = unique(data['taak'])

            # select all but level 0
            data = subset(data, level==0)
            if(length(data) == 0 ) {
                # when length is zero, no results found for this participant and task
                next
            }

            # calculate mean per level/item
            data<-tapply(as.numeric(data$score),  list(data$level, data$item), mean, na.rm=TRUE)

            # mean over the level/item means
            data<-apply(data, 1, mean)
            data<-summary(data)

            newData[rowNewData, 'task_id'] = task
            newData[rowNewData, 'participant_id'] = participant
            newData[rowNewData, 'meanprop_practice'] = data["Mean"]
            rowNewData = rowNewData+1
        }
    }

    if(is.vector(newData$task_id)) {
        return(newData[order(
            newData$task_id,
            newData$participant_id),])
    }
    return(newData)
}
