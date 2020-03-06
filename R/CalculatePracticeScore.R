###############################################################################
#
# geeft een score hoeveel oefenitems volledige goed zijn gegaan.
# LET OP! bij het apen spel wordt het eerste item niet mee geteld.
###############################################################################

getPracticeScore <- function(mydata){
    rowNewData = 1
    participants<-unique(mydata$participant_id)
    tasks<-unique(mydata$task_id)
    newData <- data.frame(matrix(ncol = 0, nrow = 0))

    for (participant in participants) {
        for (task in tasks) {
            # select participant and task
            data = subset(mydata, task_id == task)
            data = subset(data, participant_id == participant)

            # select all level 0
            data = subset(data, level == 0)
            # The first practice item of the monkey game should be skipped.
            # This item is not a reversed item.
            data = subset(data, (taak == 2 & item > 1) | taak == 1)

            # filter(data, taak != "2" & item != "1")
            if(length(data) == 0 ) {
                # when length is zero, no results found for this participant and task
                next
            }

            # calculate mean per level/item
            data<-tapply(as.numeric(data$score),  list(data$level, data$item), mean, na.rm=TRUE)
            # select items which have a mean of 1, in other words, all items correct.
            data = data[data == 1]
            newData[rowNewData, 'task_id'] = task
            newData[rowNewData, 'participant_id'] = participant
            newData[rowNewData, 'practice_items_correct'] = length(data)
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
