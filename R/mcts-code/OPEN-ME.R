# Hi Andrew,
#
# Below is some code to help set up the Connect Three game. 
#
# When you play the game, chips are 0 for Player 0 (goes first) and 1 for Player 1 (goes second).
# 
# Chips go in at the TOP of the board (the board is played upside down from traditional Connect Four games).
# 
# To play a move, type the column number and press Enter. Choosing something that isn't a column ends the game.
#
# Now, some code. Please set your working directory to wherever all the code is.
# setwd("/Users/douglas/Documents/School/Fall 2011/STA 6866/project/report/code")

# If you want to start with no memory for the AI, run this.
branch.leaf.relationships <- list()

# If you want to start with a memory of about 25,000 game states, run this.
branch.leaf.relationships <- dget("BLR-4x4-AI1-25k.txt")


# Load the game files
source("connect-game-code.R") # basic game code
source("naive-AI.R") # the naive AI mentioned in the paper
source("pprint.R") # a function I use in some debug statements

# Load the MCTS AI
# If you want it WITHOUT the block checking, load this one (for comparisons)
source("mcts-ai-sans-block.R")
# If you want it WITH the block checking, load this one (for playing against)
source("mcts-ai-avec-block.R")



# If you just want to play a game, try this
gm(AI=TRUE)


# If you want to run some naive vs. MCTS games, try this. Try changing the bias!
num.games <- 100
bias=0.2 

nrow=4
ncol=4
winval=3
AI.player=1
Nsim=10
think.out.loud=FALSE

weight=TRUE
check.win <- TRUE

winners <- c()

for (ii in 1:num.games){
	winners[ii] <- randomVSmcts.game(nrow=nrow,ncol=ncol,winval=winval,AI.player=AI.player,Nsim=Nsim,think.out.loud=think.out.loud,bias=bias,weight=weight,check.win=check.win)
	if (ii%%10==0)
		pprint("Simulation number ",ii);
	}
mcts.win <- sum(winners==1)
pprint(mcts.win)
pprint("MCTS AI won ",mcts.win, " times, i.e. ", mcts.win/length(winners) * 100,"%")

# If you want to run some naive vs. naive games, try this.
rand.winners <- c()
for (ii in 1:num.games){
	rand.winners[ii] <- randomVSrandom.game(nrow=nrow,ncol=ncol,winval=winval,AI.player=AI.player,Nsim=Nsim,think.out.loud=think.out.loud,bias=bias,weight=weight,check.win=check.win)
	if (ii%%10==0)
		pprint("Simulation number ",ii);
	}
win1 <- sum(rand.winners==1)
pprint("Player 1 won ",win1, " times, i.e. ", win1/length(rand.winners) * 100,"%")

