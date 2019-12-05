library(dplyr, nflscrapR)

multi_scrape_game_ids <- function(seasons, types, weeks = NULL, teams = NULL) {
  library(nflscrapR)
  
  #Schema of the scraped game IDs data frame.
  scrapedGameIDsColNames <- c("type", "game_id", "home_team", "away_team", "week", "season", "state_of_game", "game_url", "home_score", "away_score")
  #Create scraped game IDs data frame with a matching number of columns.
  scrapedGameIDs <- as.data.frame(matrix(,0,length(scrapedGameIDsColNames)))
  #Rename columns of scraped game IDs data frame.
  names(scrapedGameIDs) <- scrapedGameIDsColNames
  
  #Loop for each season and each type of game (which is either preseason, regular season, or postseason).
  for(s in seasons) {
    for(ty in types) {
      #Bind the data scraped so far with the data scraped using the current season, game type, week, and team information.
      scrapedGameIDs <- rbind(scrapedGameIDs, scrape_game_ids(s, ty, weeks = weeks, teams = teams))
    }
  }
  
  #Return the scraped gamed IDs data frame.
  return(scrapedGameIDs)
}

multi_scrape_game_play_by_play <- function(gameIDs) {
  library(nflscrapR)  
  
  #Create an empty list for each game ids' play-by-play data.
  scrapedGamePlayByPlayList = list()
  
  #Loop for each game id.
  for(i in 1:nrow(gameIDs)) {
    #Bind the data scraped so far with the data scraped using the current game id.
    tempScrapedGamePlayByPlay <- scrape_game_play_by_play(gameIDs[i,"game_id"], gameIDs[i,"type"], gameIDs[i,"season"], 0)
    scrapedGamePlayByPlayList[[i]] <- tempScrapedGamePlayByPlay[tempScrapedGamePlayByPlay$quarter_end==1,c("game_id","home_team","away_team","qtr","total_home_score","total_away_score")]
  }
  
  #
  scrapedGamePlayByPlay <- do.call(rbind, scrapedGamePlayByPlayList)
  
  scrapedGamePlayByPlay <- scrapedGamePlayByPlay[complete.cases(scrapedGamePlayByPlay), ]
  
  scrapedGamePlayByPlay <- unique(scrapedGamePlayByPlay)
  
  return(scrapedGamePlayByPlay)
}

game_data <- function(gameIDs, gamePlayByPlay) {
  
  n <- length(gameIDs$game_id)
  
  game_id <- gameIDs$game_id
  home_team <- gameIDs$home_team
  away_team <- gameIDs$away_team
  
  qtr1_home_score <- rep(NA, length=n)
  qtr1_away_score <- rep(NA, length=n)
  qtr2_home_score <- rep(NA, length=n)
  qtr2_away_score <- rep(NA, length=n)
  qtr3_home_score <- rep(NA, length=n)
  qtr3_away_score <- rep(NA, length=n)
  qtr4_home_score <- rep(NA, length=n)
  qtr4_away_score <- rep(NA, length=n)
  
  final_home_score <- gameIDs$home_score 
  final_away_score <- gameIDs$away_score
  
  for(i in 1:length(game_id)){
    tryCatch({qtr1_home_score[[i]] <- as.numeric(gamePlayByPlay[gamePlayByPlay$game_id==game_id[i]&gamePlayByPlay$qtr==1,][[1,"total_home_score"]])}, error=function(e){})
    tryCatch({qtr1_away_score[[i]] <- as.numeric(gamePlayByPlay[gamePlayByPlay$game_id==game_id[i]&gamePlayByPlay$qtr==1,][[1,"total_away_score"]])}, error=function(e){})
    tryCatch({qtr2_home_score[[i]] <- as.numeric(gamePlayByPlay[gamePlayByPlay$game_id==game_id[i]&gamePlayByPlay$qtr==2,][[1,"total_home_score"]])}, error=function(e){})
    tryCatch({qtr2_away_score[[i]] <- as.numeric(gamePlayByPlay[gamePlayByPlay$game_id==game_id[i]&gamePlayByPlay$qtr==2,][[1,"total_away_score"]])}, error=function(e){})
    tryCatch({qtr3_home_score[[i]] <- as.numeric(gamePlayByPlay[gamePlayByPlay$game_id==game_id[i]&gamePlayByPlay$qtr==3,][[1,"total_home_score"]])}, error=function(e){})
    tryCatch({qtr3_away_score[[i]] <- as.numeric(gamePlayByPlay[gamePlayByPlay$game_id==game_id[i]&gamePlayByPlay$qtr==3,][[1,"total_away_score"]])}, error=function(e){})
    tryCatch({qtr4_home_score[[i]] <- as.numeric(gamePlayByPlay[gamePlayByPlay$game_id==game_id[i]&gamePlayByPlay$qtr==4,][[1,"total_home_score"]])}, error=function(e){})
    tryCatch({qtr4_away_score[[i]] <- as.numeric(gamePlayByPlay[gamePlayByPlay$game_id==game_id[i]&gamePlayByPlay$qtr==4,][[1,"total_away_score"]])}, error=function(e){})
  }
  
  gameData <- do.call(cbind,list(game_id, home_team, away_team, qtr1_home_score, qtr1_away_score, qtr2_home_score, qtr2_away_score, qtr3_home_score, qtr3_away_score, qtr4_home_score, qtr4_away_score, final_home_score, final_away_score))
  rownames(gameData) <- 1:n
  colnames(gameData) <- c("game_id", "home_team", "away_team", "qtr1_home_score", "qtr1_away_score", "qtr2_home_score", "qtr2_away_score", "qtr3_home_score", "qtr3_away_score", "qtr4_home_score", "qtr4_away_score", "final_home_score", "final_away_score")
  gameData <- as.data.frame(gameData)
  
  return(gameData)
}

points_scored_frequency <- function(gameData){
  
  pointsScoredFrequency <- matrix(rep(0, 32*32), nrow = 32, ncol=32)
  rownames(pointsScoredFrequency) <- 0:31
  colnames(pointsScoredFrequency) <- 0:31
  
  winning_team_scored <- NULL
  losing_team_scored <- NULL
  
  for(i in 1:length(gameData$game_id)){
    
    tryCatch({
      winning_team_scored <- as.character(max(as.numeric(as.character(gameData$qtr1_home_score[i])),as.numeric(as.character(gameData$qtr1_away_score[i]))))
      losing_team_scored <- as.character(min(as.numeric(as.character(gameData$qtr1_home_score[i])),as.numeric(as.character(gameData$qtr1_away_score[i]))))
      pointsScoredFrequency[losing_team_scored,winning_team_scored] <- pointsScoredFrequency[losing_team_scored,winning_team_scored] + 1
    }, error=function(e){})
    
    tryCatch({
      if(as.numeric(as.character(gameData$qtr1_home_score[i]))>as.numeric(as.character(gameData$qtr1_away_score[i]))){
        winning_team_scored <- as.character(as.numeric(as.character(gameData$qtr2_home_score[i])) - as.numeric(as.character(gameData$qtr1_home_score[i])))
        losing_team_scored <- as.character(as.numeric(as.character(gameData$qtr2_away_score[i])) - as.numeric(as.character(gameData$qtr1_away_score[i])))
      } else if(as.numeric(as.character(gameData$qtr1_home_score[i]))<as.numeric(as.character(gameData$qtr1_away_score[i]))) {
        winning_team_scored <- as.character(as.numeric(as.character(gameData$qtr2_away_score[i])) - as.numeric(as.character(gameData$qtr1_away_score[i])))
        losing_team_scored <- as.character(as.numeric(as.character(gameData$qtr2_home_score[i])) - as.numeric(as.character(gameData$qtr1_home_score[i])))
      } else {
        winning_team_scored <- as.character(max(as.numeric(as.character(gameData$qtr2_home_score[i])) - as.numeric(as.character(gameData$qtr1_home_score[i])),
                                                as.numeric(as.character(gameData$qtr2_away_score[i])) - as.numeric(as.character(gameData$qtr1_away_score[i]))))
        losing_team_scored <- as.character(min(as.numeric(as.character(gameData$qtr2_home_score[i])) - as.numeric(as.character(gameData$qtr1_home_score[i])),
                                               as.numeric(as.character(gameData$qtr2_away_score[i])) - as.numeric(as.character(gameData$qtr1_away_score[i]))))
      }
      pointsScoredFrequency[losing_team_scored,winning_team_scored] <- pointsScoredFrequency[losing_team_scored,winning_team_scored] + 1
      
    },error=function(e){})
    
    tryCatch({
      if(as.numeric(as.character(gameData$qtr2_home_score[i]))>as.numeric(as.character(gameData$qtr2_away_score[i]))){
        winning_team_scored <- as.character(as.numeric(as.character(gameData$qtr3_home_score[i])) - as.numeric(as.character(gameData$qtr2_home_score[i])))
        losing_team_scored <- as.character(as.numeric(as.character(gameData$qtr3_away_score[i])) - as.numeric(as.character(gameData$qtr2_away_score[i])))
      } else if(as.numeric(as.character(gameData$qtr2_home_score[i]))<as.numeric(as.character(gameData$qtr2_away_score[i]))) {
        winning_team_scored <- as.character(as.numeric(as.character(gameData$qtr3_away_score[i])) - as.numeric(as.character(gameData$qtr2_away_score[i])))
        losing_team_scored <- as.character(as.numeric(as.character(gameData$qtr3_home_score[i])) - as.numeric(as.character(gameData$qtr2_home_score[i])))
      } else {
        winning_team_scored <- as.character(max(as.numeric(as.character(gameData$qtr3_home_score[i])) - as.numeric(as.character(gameData$qtr2_home_score[i])),
                                                as.numeric(as.character(gameData$qtr3_away_score[i])) - as.numeric(as.character(gameData$qtr2_away_score[i]))))
        losing_team_scored <- as.character(min(as.numeric(as.character(gameData$qtr3_home_score[i])) - as.numeric(as.character(gameData$qtr2_home_score[i])),
                                               as.numeric(as.character(gameData$qtr3_away_score[i])) - as.numeric(as.character(gameData$qtr2_away_score[i]))))
      }
      pointsScoredFrequency[losing_team_scored,winning_team_scored] <- pointsScoredFrequency[losing_team_scored,winning_team_scored] + 1
      
    },error=function(e){})
    
    tryCatch({
      if(as.numeric(as.character(gameData$qtr3_home_score[i]))>as.numeric(as.character(gameData$qtr3_away_score[i]))){
        winning_team_scored <- as.character(as.numeric(as.character(gameData$qtr4_home_score[i])) - as.numeric(as.character(gameData$qtr3_home_score[i])))
        losing_team_scored <- as.character(as.numeric(as.character(gameData$qtr4_away_score[i])) - as.numeric(as.character(gameData$qtr3_away_score[i])))
      } else if(as.numeric(as.character(gameData$qtr3_home_score[i]))<as.numeric(as.character(gameData$qtr3_away_score[i]))) {
        winning_team_scored <- as.character(as.numeric(as.character(gameData$qtr4_away_score[i])) - as.numeric(as.character(gameData$qtr3_away_score[i])))
        losing_team_scored <- as.character(as.numeric(as.character(gameData$qtr4_home_score[i])) - as.numeric(as.character(gameData$qtr3_home_score[i])))
      } else {
        winning_team_scored <- as.character(max(as.numeric(as.character(gameData$qtr4_home_score[i])) - as.numeric(as.character(gameData$qtr3_home_score[i])),
                                                as.numeric(as.character(gameData$qtr4_away_score[i])) - as.numeric(as.character(gameData$qtr3_away_score[i]))))
        losing_team_scored <- as.character(min(as.numeric(as.character(gameData$qtr4_home_score[i])) - as.numeric(as.character(gameData$qtr3_home_score[i])),
                                               as.numeric(as.character(gameData$qtr4_away_score[i])) - as.numeric(as.character(gameData$qtr3_away_score[i]))))
      }
      pointsScoredFrequency[losing_team_scored,winning_team_scored] <- pointsScoredFrequency[losing_team_scored,winning_team_scored] + 1
      
    },error=function(e){})
  }
  
  return(pointsScoredFrequency)
  
}

points_scored_observations <- function(pointsScoredFrequency) {
  
  pointsScoredObservations <- list()
  
  for(c in as.character(0:31)) {
    for(r in as.character(0:31)) {
      n <- pointsScoredFrequency[r,c]
      temp <- list()
      tryCatch({
        temp <- list()
        for (i in 1:n) {
          temp[[i]] <- c(as.numeric(as.character(r)), as.numeric(as.character(c)))
        }
        pointsScoredObservations <- append(pointsScoredObservations, temp)
      },error=function(e){})
    }
  }
  
  pointsScoredObservations <- do.call(rbind, pointsScoredObservations)
  colnames(pointsScoredObservations) <- c("losing_team_points_scored","winning_team_points_scored")
  
  pointsScoredObservations <- as.data.frame(pointsScoredObservations)
  
  return(pointsScoredObservations)
}

initial_distribution <- function(states, givenState) {
  
  #Create matrix of size 1 column and the same number of rows as the number of states.
  initialDistribution <- matrix(rep(0, length(states)), ncol = 1, nrow = length(states))
  
  #Rename the rows of the matrix to correspond to a state.
  rownames(initialDistribution) <- states
  
  initialDistribution[givenState,1] <- 1
  
  return(initialDistribution)
}

transition_matrix_one <- function(states, pointsScoredFrequency) {
  
  #Calculate the relative frequency of each observation in the points scored matrix.
  pointsScoredRelativeFrequency <- pointsScoredFrequency / sum(pointsScoredFrequency)
  
  x <- length(states)
  transitionMatrixOne <- matrix(rep(0, x^2), nrow = x, ncol = x)
  rownames(transitionMatrixOne) <- states
  colnames(transitionMatrixOne) <- states
  
  for(previousState in rownames(transitionMatrixOne)) {
    
    previousStateSplit <- strsplit(previousState, '-')[[1]]
    tryCatch({
      for(previousWinningTeamPointsScored in colnames(pointsScoredRelativeFrequency)) {
        updatedWinningTeamScore <- as.numeric(previousStateSplit[1]) + as.numeric(previousWinningTeamPointsScored)
        for(previousLosingTeamPointsScored in rownames(pointsScoredRelativeFrequency)) {
          updatedLosingTeamScore <- as.numeric(previousStateSplit[2]) + as.numeric(previousLosingTeamPointsScored)
          newWinningTeamSCore <- max(updatedWinningTeamScore, updatedLosingTeamScore)
          newLosingTeamScore <- min(updatedWinningTeamScore, updatedLosingTeamScore)
          nextState <- paste(newWinningTeamSCore, newLosingTeamScore, sep = "-")
          transitionMatrixOne[previousState,nextState] <- transitionMatrixOne[previousState, nextState] + pointsScoredRelativeFrequency[previousLosingTeamPointsScored,previousWinningTeamPointsScored]
        }
      }},
      error=function(e){
        transitionMatrixOne[previousState, ] <- rep(0,x)
        transitionMatrixOne[previousState, previousState] <- transitionMatrixOne[previousState, previousState] + 1})
  }
  
  for(previousState in rownames(transitionMatrixOne)) {
    if(sum(transitionMatrixOne[previousState, ]) != 1) {
      transitionMatrixOne[previousState, ] <- rep(0,x)
      transitionMatrixOne[previousState, previousState] <- 1
    }
  }
  
  return(transitionMatrixOne)
}

transition_matrix_two <-function(states, pointsScoredFrequency) {
  
  #Calculate the relative frequency of each amount of points scored for the winning team.
  winningTeamPointsScoredRelativeFrequency <- colSums(pointsScoredFrequency) / sum(pointsScoredFrequency)
  
  #Calculate the relative frequency of each amount of points scored for the winning team.
  losingTeamPointsScoredRelativeFrequency <- rowSums(pointsScoredFrequency) / sum(pointsScoredFrequency)
  
  transitionMatrixTwo <- matrix(rep(0, x^2), nrow = x, ncol = x)
  rownames(transitionMatrixTwo) <- states
  colnames(transitionMatrixTwo) <- states
  
  for(previousState in rownames(transitionMatrixTwo)) {
    previousStateSplit <- strsplit(previousState, '-')[[1]]
    tryCatch({
      for(previousWinningTeamPointsScored in as.character(0:31)) {
        updatedWinningTeamSCore <- as.numeric(previousStateSplit[1]) + as.numeric(previousWinningTeamPointsScored)
        winningTeamPointsScoredProbability <- winningTeamPointsScoredRelativeFrequency[previousWinningTeamPointsScored]
        for(previousLosingTeamPointsScored in as.character(0:31)) {
          updatedLosingTeamScore <- as.numeric(previousStateSplit[2]) + as.numeric(previousLosingTeamPointsScored)
          newWinningTeamSCore <- max(updatedWinningTeamSCore, updatedLosingTeamScore)
          newLosingTeamScore <- min(updatedWinningTeamSCore, updatedLosingTeamScore)
          nextState <- paste(newWinningTeamSCore, newLosingTeamScore, sep = "-")
          losingTeamPointsScoredProbability <- losingTeamPointsScoredRelativeFrequency[previousLosingTeamPointsScored]
          transitionMatrixTwo[previousState, nextState] <- transitionMatrixTwo[previousState, nextState] + winningTeamPointsScoredProbability * losingTeamPointsScoredProbability
          #print(paste(previousState, nextState, transitionMatrixTwo[previousState, nextState], sep = ", "))
        }
      }},
      error=function(e){
        transitionMatrixTwo[previousState, ] <- rep(0,x)
        transitionMatrixTwo[previousState, previousState] <- transitionMatrixTwo[previousState, previousState] + 1})
  }
  
  for(previousState in rownames(transitionMatrixTwo)) {
    if(sum(transitionMatrixTwo[previousState, ]) != 1) {
      transitionMatrixTwo[previousState, ] <- rep(0,x)
      transitionMatrixTwo[previousState, previousState] <- 1
    }
  }
  
  
  return(transitionMatrixTwo)
}

state_probability_distribution <- function(initialDistribution, transitionMatrix, quartersRemaining) {
  library(expm)
  return(t(transitionMatrix%^%quartersRemaining)%*%initialDistribution)
}

scorigami_probability <- function(stateProbabilityDistribution, missingStates) {
  
  missingStateProbabilityDistribution <- stateProbabilityDistribution[is.element(rownames(stateProbabilityDistributionOne), missingStates)]
  missingStateProbabilityDistribution <- matrix(missingStateProbabilityDistribution, ncol = 1, nrow = length(missingStateProbabilityDistribution))
  rownames(missingStateProbabilityDistribution) <- missingStates
  
  return(sum(missingStateProbabilityDistribution))
}