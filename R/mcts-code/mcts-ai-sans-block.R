#
#connect4ai.R
#
#functions included: 
#	
#	main AI functions:
#	
#		AI.move(board,player,winval,Nsim=10,bias=1.2,think.out.loud=TRUE,weight=TRUE)
#		-- AI wrapper function to return a column to move to
#		
#		identifyNextStates(board,player)
#		-- identify the valid moves for a given player
#		
#		evaluateNextStates(choices,parent,board,player,winval,Nsims=10,think.out.loud,weight)
#		-- evaulate the states to make a choice
#		
#		leon(start.state,player,board,winval)
#		-- explore the tree by running random simulations
#		
#		update.blr(sim.big)
#		-- update the "tree" with the new results of random simulations
#		
#		identifyMoveProps(player,choices)
#		-- identify how good each move is
#
#		UCT(props,UCB,think.out.loud,weight=TRUE)
#		-- help pick the next state based on confidence bounds and randomness
#
#			
#	helper functions:
#	
#		stateID(board) 
#		-- return the base10 value for the board
#		
#		displayGame(visited.states,board)
#		-- display all boards in a game from start to finish
#		
#		convertToDecimal(base3state)
#		-- convert a base3 number to base10
#		
#		convertToTrinary(state,board)
#		-- convert a base10 number to base3
#		
#		toBoard(state,some.board)
#		-- convert a state to a board
#
#		findState(state)
#		-- identify if a given state is in the tree
#
#		stateToColumn(state,parent,board)
#		-- return the column that is different between two boards
#	
#
#	deprecated? functions:
#		
#		createStateFile(board)
#		saveStateFile(state)
#
#


AI.move <- function(board, player, winval, Nsim=10,bias=1.2,think.out.loud,weight=TRUE){
	ucb.bias <<- bias # tuneable bias parameter for UCB later on
	
	# come up with the branch.leaf.relationships object
	# might want to be able to load this from a file later
	# also, global variables suck 
	# wrap the whole thing in an environment?
	# might want to be careful about writing over this each time
	#branch.leaf.relationships <<- list()
	
	current.state <- stateID(board);
	choices <- identifyNextStates(board=board,player=player)
	next.state <- evaluateNextStates( 	choices=choices,
										parent=current.state,
										board=board,
										player=player, 
										winval=winval,
										think.out.loud=think.out.loud,
										weight=weight)
	column <- stateToColumn(state=next.state,parent=current.state,board=board)
	if (think.out.loud) pprint("I'm choosing column ",column)
	#return(column) # column we should try playing
	return(next.state) # actually, it will be easier to just return the state. 
						# some error checking was already done, here.
	}

stateToColumn <- function(state,parent,board){
	new.board <- toBoard(state=state,some.board=board)
	old.board <- toBoard(state=parent,some.board=board)
	new.board[which(is.na(new.board))] <- -66
	old.board[which(is.na(old.board))] <- -66
	#print(old.board)
	#print(new.board)
	for (ii in 1:ncol(new.board)){
		col.equal <- sum(!(new.board[,ii]==old.board[,ii])) 
		# should be 0 if all equal
		if (col.equal != 0)
			return(ii) # found the column
		}
	}
	
stateID <- function(board){
	temp.state <- as.vector(board)
	#if (is.null(temp.state)) print(is.null(temp.state))
	temp.state[which(is.na(temp.state))] <- 2
	state <- convertToDecimal(temp.state)
	return(state)
	}
	
convertToDecimal <- function(base3state){
	# convert base 3 number to base 10
	base3state <- base3state[length(base3state):1] # wrote the code backward below
	temp <- 0
	for (ii in 1:length(base3state)){
		temp <- temp + base3state[ii]*(3^(ii-1))
		}
	return(temp)
	}
	
findState <- function(state){
	return(!is.null(branch.leaf.relationships[[as.character(state)]]))
	}
	
identifyNextStates <- function(board,player){
	r <- ncol(board)
	valid.next.board <- list()
	for (ii in 1:r){
		temp <- addMove(board,column=ii,player=player)
		if (temp$valid.input){
			valid.next.board[[ii]] <- temp$board
			}
		}
	valid.next.nodes <- c() # if length==0, then we're at an end point... I think. 
	if (length(valid.next.board) > 0){
		for (ii in 1:length(valid.next.board)){
			if (!is.null(valid.next.board[[ii]])){
				valid.next.nodes[ii] <- stateID(valid.next.board[[ii]]);
				}
			}
		}
	return(na.omit(valid.next.nodes))
	}

# this function should return a state that we should pick
# another function will convert the state to a move (column)
evaluateNextStates <- function( choices, parent, board,
#								tree,
#								tree.winningChildren,
#								tree.parentVisits,
								player,
								winval,
								Nsims=10,
								think.out.loud,
								weight){
	# first we figure out which choices/nodes are already in the BLR
	# then we explore the remaining choices and update the BLR
	# finally we make a decision
	
	# but first, let's check to see if any of these choices are a win condition for us
	# if they are, let's just take it w.p. 1
	
	#if (win.check){
	winning.board <- c()
	for (ii in 1:length(choices)){
		win <- checkWinner(toBoard(choices[ii],board),winval=winval,player=player)
		if (win) winning.board <- choices[ii]
		}
	if (!is.null(winning.board)) {
		#print("Deterministic winning move")
		return(winning.board) # hey, we won!
		}
	
	
	##### BEGIN EXPERIMENTAL BLOCKING CODE
	
#	block.board <- c()
#	opponent <- (player+1)%%2
#	opponent.moves <- identifyNextStates(board=board,player=opponent)
	###print(opponent.moves)
#	for (ii in 1:length(opponent.moves)){
#		block <- checkWinner(toBoard(opponent.moves[ii],board),winval=winval,player=opponent)
#		if (block) {
			###print("we must block")
#			block.board <- choices[ii] 
#			}
		 ###we need to end up playing in the SAME COLUMN 
		 ###that opponent would play here, but NOT this board
#		} 
#	if (!is.null(block.board)){
		###print("opponent will win unless we block")
#		column <- stateToColumn(block.board,parent,board)
#		true.block.board <- addMove(board=toBoard(parent,board),column=column,player=player)
#		block.state <- stateID(true.block.board$board)
#		return(block.state) 
#		}
		
	##### END EXPERIMENTAL BLOCKING CODE
	
	 ##check for a block
	#block.board <- c()
	#for (ii in 1:length(choices)){
	#	block <- checkWinner(toBoard(choices[ii],board),winval=winval,player=player)
	#	if (block) block.board <- choices[ii]
	#	}
	#if (!is.null(block.board)) return(block.board) # hey, we won!
	
		#}
# DEBUG
#pprint("choices = ",choices)
	# let's check if the node is in the BLR
	toExplore <- c()
	for (ii in 1:length(choices)){
		if (!findState(choices[ii])){ # haven't been to the state before...
			toExplore[ii] <- choices[ii]
			}
		}
	
	# if we've already explored all the choices some, toExplore could be NULL
# DEBUG
#pprint("toExplore = ",toExplore)
	if (!is.null(toExplore)){
		toExplore <- na.omit(toExplore)
		# now we have the nodes to explore... let's get this party started
		for(ii in 1:length(toExplore)){
			simulated.game <- list()
			for (jj in 1:Nsims){ # simulating a bunch of games
				#print(paste("Current sim:",jj))
				simulated.game[[jj+(Nsims*(ii-1))]] <- leon(start.state=toExplore[ii],player=player,board=board,winval=winval)
				}
			# deal with the results of the simulated games
			update.blr(simulated.game); # using global variables
			}
			if (think.out.loud) pprint("Number of simualted games: ",length(simulated.game))
		}
	
	# code for updating values based on estimated probs here
	moveProps <- identifyMoveProps(player=player,choices=choices)
	props <- moveProps$props
	ni <- moveProps$visited
	current.state.visits <- branch.leaf.relationships[[as.character(parent)]][["visited"]][1]
	if (is.null(current.state.visits)) current.state.visits <- 1 # we haven't yet added the parent to the tree... we'll end up doing that later?
	ME <- ucb.bias*sqrt(max(log(current.state.visits),1)/ni) # adjust for when we've only been to the parent once
	UCB <- props + ME
	
	if (think.out.loud){
		pprint("Before making a decision, this is my information:")
		pprint("choices: ",choices)
		pprint("props: ",props)
		pprint("UCB: ",UCB)
		}
	
	# code for decision here 
	choice.index.chosen <- UCT(props,UCB,think.out.loud=think.out.loud,weight=weight)
	state.chosen <- choices[choice.index.chosen]
	return(state.chosen)
	}
	
UCT <- function(props,UCB,think.out.loud,weight=TRUE){
	n <- length(props)
	indices <- 1:n
	keep.for.sure <- drop.for.sure <- c()
	for (ii in 1:n){
		if (sum(!(props[ii] > UCB[-ii]))==0) 
			keep.for.sure <- ii # choice at position ii is better than all the others
		if (sum(!(UCB[ii] < props[ii]))==0)  
			drop.for.sure <- ii # choice at position ii did worse than all the others
		}
	if (!is.null(keep.for.sure)) {
		if (think.out.loud) pprint("We're keeping: ",keep.for.sure);
		choice <- keep.for.sure # we found our guy
		}
	# if not...
	else{
		if(!is.null(drop.for.sure)){
			if (think.out.loud) pprint("We're dropping: ",drop.for.sure);
			indices <- indices[-drop.for.sure] # take out the awful ones
			}
		#print(indices)
		if (length(indices) > 1){
			# either sampling with even weights or not (based on props)
			if (weight) {
				#print(props)
				#print(indices)
				if (sum(props[indices]==0)==length(props[indices])){ # we're going to lose
					# avoids the "too few pos. probs." error
					choice <- sample(indices,1)
				}
				else {
					choice <- sample(indices,1,prob=props[indices])
					}
				}
			else {
				choice <- sample(indices,1)
				}
			}
		else {
			choice <- indices # there is only one left somehow, so we go with it
			}
		}
	if (think.out.loud) print(paste("choice =",choice))
	return(choice)
	}
	
update.blr <- function(sim.big){
	for (jj in 1:length(sim.big)){
		#print(paste("index =",jj))
		sim <- sim.big[[jj]]
		leaf <- sim$last.state
		#print(leaf)
		vs <- sim$visited.states 
		for (ii in vs){
			#print(paste("ii =",ii))
			cur.str <- as.character(ii) # current string
			if (is.null(branch.leaf.relationships[[cur.str]])){
				# currently empty
				branch.leaf.relationships[[cur.str]][["leaves"]] <<- rep(leaf,2)
				branch.leaf.relationships[[cur.str]][["leaves"]] <<- leaf # we ACTUALLY NEED THIS
				# we need to create the list the right way so it is atomic and, this requires using a vector. 
				# I hate R. 
				}
			else{
				all.leaves <- branch.leaf.relationships[[cur.str]][["leaves"]]
				all.leaves <- unique(c(all.leaves,leaf))
				branch.leaf.relationships[[cur.str]][["leaves"]] <<- all.leaves
				}
			
			if (ii == leaf){
				branch.leaf.relationships[[cur.str]][["winner"]] <<- sim$winning.player
				}
			# if this is a parent/branch, let's count how many times this was visited
			if (ii != leaf) {
				if (is.null(branch.leaf.relationships[[cur.str]][["visited"]])){
					branch.leaf.relationships[[cur.str]][["visited"]] <<- 1
					}
				else {
					k <- branch.leaf.relationships[[cur.str]][["visited"]]
					branch.leaf.relationships[[cur.str]][["visited"]] <<- k+1
					}
				r <- branch.leaf.relationships[[cur.str]][["win1"]]
				if (is.null(r)) r <- 0
				if (sim$winning.player==1){
					branch.leaf.relationships[[cur.str]][["win1"]] <<- r + 1
					}
				else {
					branch.leaf.relationships[[cur.str]][["win1"]] <<- r + 0 
					# this last line is just bookkeeping to make sure it doesn't stay NULL
					}
				}
			# now we add in code to keep track of how many times PLAYER 1 wins
			# in the child nodes from any given parent
			# If the AI is playing as PLAYER 0, then we can get that number from
			# visited - wins1
			}
		}
	# no need to return anything as we're using a global for BLR
	#return(branch.leaf.relationships) 
	}
	
identifyMoveProps <- function(player,choices){
	player1 <- (player==1)
	props <- visited <- c()
	for (ii in 1:length(choices)){
		cur.str <- as.character(choices[ii])
		w <- branch.leaf.relationships[[cur.str]][["win1"]]
		#print(w)
		v <- branch.leaf.relationships[[cur.str]][["visited"]]
		#print(v)
		visited[ii] <- v
		if ( player1) { props[ii] <- w/v }
		if (!player1) { props[ii] <- 1-(w/v) }
		}
	#print(props)
	#print(visited)
	return(list(props=props,visited=visited))
	}

### begin old code
#update.blr <- function(sim.big,blr){
#	for (jj in 1:length(sim.big)){
#		sim <- sim.big[[jj]]
#		leaf <- sim$last.state
#		vs <- sim$visited.states[-length(visited.states)]
#		for (ii in vs){
#			cur.str <- as.character(ii) # current string
#			if (is.null(blr[[cur.str]])){
#				blr[[cur.str]] <- leaf
#				}
#			else{
#				blr[[cur.str]][[length(blr[[cur.str]]) + 1]] <- leaf
#				}
#			}
#		}
#	return(blr)
#	}
#
# returns which PLAYER wins at each state
#update.winningChildren <- function(sim.big,twc){
#	for (jj in 1:length(sim.big)){
#		sim <- sim.big[[jj]]
#		twc[sim$last.state] <- sim$winning.player
#		}
#	return(twc)
#	}
#update.parentVisits <- function(sim.big,tpv){
#	for (jj in 1:length(sim.big)){
#		sim <- sim.big[[jj]]
#		for (ii in sim$visited.states){
#			if(!is.na(tpv[ii])){ # tree.parentVisits MUST be at least a single NA and not just c() by this point
#				tpv[ii] <- tpv[ii] + 1}
#			else {
#				tpv[ii] <- 1
#				}
#			}
#		}
#	return(tpv)
#	}
#
### end old code

#set.seed(1)
leon <- function(start.state,player,board,winval){ # explorer function, named for Ponce de Leon, plays a random game
	visited.states <- start.state
	#pprint("visited.states at creation: ",visited.states)
	game.over <- FALSE
	full.board <- FALSE
	while(!game.over){
		current.board <- toBoard(visited.states[length(visited.states)],some.board=board)
		game.over.a <- checkWinner(current.board,winval=winval,player=player)
		game.over.b <- checkWinner(current.board,winval=winval,player=(player+1)%%2)
		#print(game.over.a)
		#print(game.over.b)
		#print(current.board)
		full.board <- game.over.c <- (sum(is.na(current.board))==0)
		game.over <- (game.over.a || game.over.b || game.over.c)
		valid.next.nodes <- identifyNextStates(board=current.board,player=player)
		#pprint("valid.next.nodes = ",valid.next.nodes)
		#pprint("full.board = ",full.board)
		#pprint("game.over = ",game.over)
		#pprint("visited.states = ",visited.states)
		if (full.board || game.over || length(valid.next.nodes)==0 || visited.states[length(visited.states)] == 0) {
			game.over <- TRUE; # either we started with a won game, or the game was won while we played
			# if the game was won in simulations, we need to note this!
			winning.state <- visited.states[length(visited.states)];
			if (full.board){
				winning.player <- -99;
				}
			else {
				winning.player <- (player + 1) %% 2 # if it was won LAST turn, the player that went last won
				}
			final.results <- list(winning.player=winning.player,visited.states=visited.states,last.state=winning.state)
			return(final.results)
			}
		else {
			if (length(valid.next.nodes) > 1){
				rand.state <- sample(valid.next.nodes,1) # WOW, sample() behaves oddly
				}
			else {
				rand.state <- valid.next.nodes # not random, only 1 possible move, but sample will sample from discrete uniform if there is only 1 number
				}
			visited.states[length(visited.states)+1] <- rand.state
			}
		player <- (player + 1)%%2
		}
	}
	
displayGame <- function(visited.states,board){
	for(ii in 1:length(visited.states)){
		print(toBoard(visited.states[ii],board))
		}
	}

# toBoard takes the decimal representation of a board and returns the board itself (matrix)
toBoard <- function(state,some.board){
	# some.board is just used for dimensions
	id <- convertToTrinary(state,some.board)
	id[which(id==2)] <- NA
	board <- matrix(data=id,nrow=nrow(some.board),ncol=ncol(some.board))
	return(board)
	}
	
convertToTrinary <- function(state,board){
	n <- nrow(board)*ncol(board)
	num <- c()
	temp <- state
	for (ii in 1:n){
		num[ii] <- floor(temp / 3^(n-ii))
		temp <- temp %% 3^(n-ii)
		}
	return(num)
	}
#
#createStateFile <- function(board){
	#
	# R vectors can be of AT MOST length 2^31 - 1 
	# There doesn't seem to be a good tree structure in R, so...
	# We'll assign each state a base-3 number based on the states in the board
	# and then convert base-3 to base-10 and deal with the nodes that way.
	# We'll use multiple vectors, if necessary, all referred to with the same indices.
	#
	# For the assignment, we'll play Connect 3 on a 4x4 board.
	#
#	
	# check to make sure we don't break R
#	if (ncol(board)*nrow(board) <= 19)
		# create the vector of appropriate length
#		state.tree <- rep(NA,3^(ncol(board)*nrow(board))) 
#	
#	return(state.tree)
#	}
#
#saveStateFile <- function(state){
#	setwd("/Users/douglas/Documents/School/Fall 2011/STA 6866/project")
#	dput(state, file="state.txt");
#	}
#	
#
