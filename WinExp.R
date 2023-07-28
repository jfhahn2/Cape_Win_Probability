library(tidyverse)
setwd("~/Documents/Hyannis")

#Read Historical Cape Data
full_data <- read_csv("full_pbp.csv")

# Group by Half Innings
data <- full_data %>% mutate(half_inn_id = paste(GameID, Top_Btm, Inning, sep = "-")) %>% 
  mutate(OutsOnPlay = (lead(Outs) - Outs) %% 3) %>%
  arrange(half_inn_id, InningPACount, PitchNumPA)
half_innings <- data  %>%
  group_by(half_inn_id) %>%
  summarize(RunsInInning = sum(RunsScoredOnPlay), OutsInInning = sum(OutsOnPlay))

# Join on half inning
join_data <- data %>%
  inner_join(half_innings, by = "half_inn_id")

# Make sure 3 outs in inning, dummy var at start of inning
three_outs <- join_data %>%
  filter(OutsInInning == 3) %>%
  mutate(NewInning = if_else(half_inn_id == lag(half_inn_id, default = "0"), 0, 1))

# Calculate runs already scored and yet to be scored
three_outs %>% 
  group_by(grp = cumsum(NewInning == 1)) %>% 
  mutate(AlreadyScored = cumsum(RunsScoredOnPlay) - RunsScoredOnPlay) %>% 
  ungroup() %>% 
  mutate(YetToScore = RunsInInning - AlreadyScored) -> run_vars

base_states <- run_vars %>% mutate(R1 = ifelse(is.na(Runner1B), 0, 1)) %>% mutate(R2 = ifelse(is.na(Runner2B), 0, 1)) %>% mutate(R3 = ifelse(is.na(Runner3B), 0, 1))

# Calculate RunExp for all 288 states
base_states %>% filter(Balls < 4, Strikes < 3, Outs < 3) %>% group_by(Outs, Balls, Strikes, R1, R2, R3) %>% summarize(RunExp = mean(YetToScore), num = n()) -> run_exp_pbp
base_states <- base_states %>% mutate(RunBSO = paste0(R1, R2, R3, " ", Balls, Strikes, Outs))

#Add Leadlag
base_states %>% mutate(NEW.STATE = lead(RunBSO)) -> newstates

#Fix Errors
newstates %>% mutate(NEW.STATE = ifelse((lead(Inning) != Inning) | Top_Btm != lead(Top_Btm), 3, NEW.STATE)) -> newstates


#Filter Non-Change States  Away
newstates %>% 
  filter((RunBSO != NEW.STATE) | RunsScoredOnPlay > 0) -> states_final


#Compute Transition Probs.
T_matrix <- states_final %>% 
  select(RunBSO, NEW.STATE) %>% table()

#Compute P(Transition State)
P_matrix <- 
  prop.table(T_matrix, 1)

#Add 3-Out State
P_matrix <- rbind(P_matrix, c(rep(0, 288), 1))


#Count Runners and Outs Function
count_runners_out <- function(s) { #takes a state as input
  val <- s %>% #
    str_split("") %>% 
    pluck(1)
  
  val[c(1:3, 7)] %>% 
    as.numeric() %>% 
    sum(na.rm = T)
} #sums runners and outs total

runners_out <- sapply(row.names(T_matrix),
                      count_runners_out)[-289]




#Calculate Runs
R <- outer(runners_out, runners_out, FUN = "-") 
names(R) <- names(T_matrix)[-289] 
R <- cbind(R, rep(0,288))

# Zero out impossible states
modR <- apply(R, c(1, 2), function(x) ifelse(x < 0 | x > 4, 0, 1))
modT <- T_matrix * modR


#If the new count is 0-0, add a run
R[,c(1:3, 37:39, 73:75, 109:111, 145:147, 181:183, 217:219, 253:255)] <- 
  R[,c(1:3, 37:39, 73:75, 109:111, 145:147, 181:183, 217:219, 253:255)] + 1



#Simulate a Game Function
simulate_game_remaining <- function(P, R, start = "000 000", TB = "Top", 
                                    Visitor_Score = 0, Home_Score = 0,
                                    Inning = 1) { 
  #Inputs
  #P = prob matrix
  #R = runs matrix
  #S = starting state
  #TB = top or bottom
  #VS = visitor score
  #HS = home score

  #Inning = 0
  
  #Initializers
  s <- which(rownames(P) == start)
  
  # 10 inning max in CCBL
  if (Inning > 10) {
    c(Visitor_Score, Home_Score)
  }
  
  #First 8 Innings
  while (Inning <= 8) {
    path <- NULL
    runs <- 0
    while(s < 289) {
      #If the current state is s, simulate a new state using s row in P
      s.new <- sample(1:289, size = 1, prob = P[s,]) 
      #print(s.new)
      path <- c(path, s.new) #adds to path
      runs <- runs + R[s, s.new] #total # of RS updated using value
      
      #print(runs)
      s <- s.new
    }
    
    if (TB == "Top") {
      Visitor_Score <- Visitor_Score + runs
      TB <-  "Bottom"
      s <- 1
    }
    
    else {
      Home_Score <- Home_Score + runs
      TB <-  "Top"
      Inning <- Inning + 1
      s <- 1
    }
    
  }
  
  #Past That (Run the top of the ninth)
  if (TB == "Top" && Inning == 9) {
    path <- NULL
    runs <- 0
    while(s < 289) {
      #If the current state is s, simulate a new state using s row in P
      s.new <- sample(1:289, size = 1, prob= P[s,]) 
      path <- c(path, s.new) #adds to path
      runs <- runs + R[s, s.new] #total # of RS updated using value
    
      s <- s.new
    }
    Visitor_Score <- Visitor_Score + runs
    TB <- "Bottom"
    s <- 1
  }
  
  #Check at middle of the ninth
  if(Home_Score > Visitor_Score) {
    c(Visitor_Score, Home_Score)
  }
  
  #Keep simulating past as needed
  if (TB == "Bottom" & Inning == 9) {
    #Run the bottom of the ninth
    path <- NULL
    runs <- 0
    while(s < 289 && runs <= Visitor_Score - Home_Score) {
      #If the current state is s, simulate a new state using s row in P
      s.new <- sample(1:289, size = 1, prob= P[s,]) 
      path <- c(path, s.new) #adds to path
      runs <- runs + R[s, s.new] #total # of RS updated using value
     
      s <- s.new
    }
    
    Home_Score <- Home_Score + runs 
    TB <-  "Top"
    Inning <- 10
    s <- 73  #FOR RUNNER ON 2ND IN EXTRAS
    
    
    if(Home_Score != Visitor_Score) {
      c(Visitor_Score, Home_Score)
    }
  }
    #Extra innings simulation - only 10th inning
    
    if (TB == "Top" & Inning == 10) {
      #Visitor Side
        path <- NULL
        runs <- 0
        while(s < 289) {
          #If the current state is s, simulate a new state using s row in P
          s.new <- sample(1:289, size = 1, prob= P[s,]) 
          path <- c(path, s.new) #adds to path
          runs <- runs + R[s, s.new] #total # of RS updated using value
         
          s <- s.new
        }
        Visitor_Score <- Visitor_Score + runs
        TB <-  "Bottom"
        s <- 73 #FOR RUNNER ON 2ND IN EXTRAS
    }
    
    if (TB == "Bottom" & Inning == 10) {
      
        
        #Home Side
        path <- NULL
        runs <- 0
        while(s < 289 && runs <= Visitor_Score - Home_Score) {
          #If the current state is s, simulate a new state using s row in P
          s.new <- sample(1:289, size = 1, prob= P[s,]) 
          path <- c(path, s.new) #adds to path
          runs <- runs + R[s, s.new] #total # of RS updated using value
          
          s <- s.new
        }
        Home_Score <- Home_Score + runs
      }
    
  #End after 10 no matter what
  
  
  c(Visitor_Score, Home_Score)
}


# Simulate a game from a given state
r1 <- replicate(1000, simulate_game_remaining(modT, R, "000 000", "Top", 0, 0, 1)) 
scores <- as.data.frame(t(r1))
names(scores) <- c("Away", "Home")
winners <- scores %>% mutate(Winner = ifelse(Away > Home, "Away", ifelse(Away == Home, "Tie", "Home")))
table(winners$Winner)

