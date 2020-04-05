gm <- function(nrow=4,ncol=4,winval=3,AI=FALSE,think.out.loud=FALSE,bias=1.2,Nsim=10,weight=TRUE){
	if (!AI){
		no.AI.game(nrow=nrow,ncol=ncol,winval=winval)
		}
	if (AI){
		start.AI.game(nrow=nrow,ncol=ncol,winval=winval,AI.player=1,Nsim,think.out.loud=think.out.loud,bias=bias,weight=weight)
		}
	}


randomVSmcts.game <- function(nrow=4,ncol=4,winval=3,AI.player=1,Nsim=10,think.out.loud=FALSE,bias=1.2,weight=TRUE,check.win=TRUE,check.block=TRUE){
	board <- createBoard(nrow,ncol);
	winval <- winval
	game.over <- FALSE
	random.player <- (AI.player+1)%%2 # defaults to 0
	player <- 0 # player 0 always starts game
	move <- 1
	visited.states <- stateID(board) # initial position
	prev.state <- stateID(board)
	while(!game.over && move < nrow*ncol){
		if (player == random.player){
			AI.state <- random.AI.move( board, player,parent=prev.state ) # parent is only used in blocking!
			board <- toBoard(state=AI.state,some.board=board)
			#print(board)
			}
		else if (player == AI.player) {
			AI.state <- AI.move( board, player, winval, Nsim,
								think.out.loud=think.out.loud, bias=bias,
								weight=weight )
			prev.state <- AI.state
			board <- toBoard(state=AI.state,some.board=board)
			#print(board)
			}
		move <- move + 1
		visited.states[move] <- stateID(board)
		game.over <- checkWinner(board, winval, player)
		player <- (player + 1)%%2
		}

	#print("Final Board")
	#print(board)
	winner <- (player+1)%%2
	last.state <- stateID(board)
	
	game.summary <- list()
	game.summary[[1]] <- list(winning.player=winner,visited.states=visited.states,last.state=last.state)
	
	update.blr(game.summary)
	#print(paste("***** Game Over ****** Winner was Player",winner)
	return(winner)
	}
	
randomVSrandom.game <- function(nrow=4,ncol=4,winval=3,AI.player=1,Nsim=10,think.out.loud=FALSE,bias=1.2,weight=TRUE,check.win=TRUE,check.block=TRUE){
	board <- createBoard(nrow,ncol);
	winval <- winval
	game.over <- FALSE
	random.player <- (AI.player+1)%%2 # defaults to 0
	player <- 0 # player 0 always starts game
	move <- 1
	visited.states <- stateID(board) # initial position
	prev.state <- stateID(board)
	while(!game.over && move < nrow*ncol){
		if (player == random.player){
			AI.state <- random.AI.move( board, player,parent=prev.state ) # parent is only used in blocking!
			board <- toBoard(state=AI.state,some.board=board)
			#print(board)
			}
		else if (player == AI.player) {
			AI.state <- random.AI.move( board, player,parent=prev.state ) # parent is only used in blocking!
			prev.state <- AI.state
			board <- toBoard(state=AI.state,some.board=board)
			#print(board)
			}
		move <- move + 1
		visited.states[move] <- stateID(board)
		game.over <- checkWinner(board, winval, player)
		player <- (player + 1)%%2
		}

	#print("Final Board")
	#print(board)
	winner <- (player+1)%%2
	last.state <- stateID(board)
	
	game.summary <- list()
	game.summary[[1]] <- list(winning.player=winner,visited.states=visited.states,last.state=last.state)
	
	update.blr(game.summary)
	#print(paste("***** Game Over ****** Winner was Player",winner)
	return(winner)
	}
	
start.AI.game <- function(nrow=4,ncol=4,winval=3,AI.player=1,Nsim,think.out.loud,bias,weight){
	# need to check if BLR exists at some point
	# also, should update BLR at the end of the game with the result
	board <- createBoard(nrow,ncol);
	winval <- winval
	game.over <- FALSE
	human.player <- (AI.player+1)%%2 # defaults to 0
	player <- 0 # player 0 always starts game
	move <- 1
	visited.states <- stateID(board) # initial position

	while(!game.over && move < nrow*ncol){
		if (player == human.player){
			board <- move(board, player)
			print(board)
			}
		else if (player == AI.player) {
#			AI.state <- random.AI.move( board, player,parent=prev.state )
			AI.state <- AI.move( board, player, winval, Nsim,
								think.out.loud=think.out.loud, bias=bias,
								weight=weight )
			board <- toBoard(state=AI.state,some.board=board)

			}
		move <- move + 1
		visited.states[move] <- stateID(board)
		game.over <- checkWinner(board, winval, player)
		player <- (player + 1)%%2
		}
		
	winner <- (player+1)%%2
	last.state <- stateID(board)
	
	game.summary <- list()
	game.summary[[1]] <- list(winning.player=winner,visited.states=visited.states,last.state=last.state)
	
	update.blr(game.summary)

	print("Final Board")
	print(board)
	print(paste("***** Game Over ****** Winner was Player",(player+1)%%2))
	}

#getAImove <- function( 	board, player, winval, Nsim,
#						think.out.loud=think.out.loud, bias=bias,
#						weight=weight){
#							
#	}

no.AI.game <- function(nrow=4,ncol=4,winval=3){
	board <- createBoard(nrow,ncol);
	winval <- winval
	game.over <- FALSE
	player <- 0
	move <- 1
	while(!game.over && move < nrow*ncol){
		board <- move(board, player)
		move <- move + 1
		game.over <- checkWinner(board, winval, player)
		player <- (player + 1)%%2
		}

	print("Final Board")
	print(board)
	print(paste("***** Game Over ****** Winner was Player",(player+1)%%2))
	}
	
move <- function(board,player){
	valid.input <- FALSE
	while(!valid.input){
		print(board);
		print(paste("Player ",player,", select a column: ",sep=""))
		column <- as.numeric(readLines(n=1)) # NO ERROR CHECKING YET - PLEASE DON'T BREAK ME, ANDREW
		result <- addMove(board,column,player)
		board <- result$board
		valid.input <- result$valid.input
		}
	return(board);
	}
	
addMove <- function(board,column,player){
		row <- which(is.na(board[,column]))[1]
		valid.input <- !is.na(row)
		if (valid.input)
			board[row,column] <- player
		return(list(board=board,valid.input=valid.input))
	}

createBoard <- function(rows, cols){
	return(matrix(data=NA,nrow=rows,ncol=cols))
	}
	
checkWinner <- function(board, winval, player){
	winner <- rep(player, winval); 
	w <- length(winner);
	
	win <- FALSE;
	# check rows
	r <- nrow(board);
	for (ii in 1:r){
		if(matcher(winner,board[ii,])){
			win <- TRUE;
			return(win); # someone one with a horizontal match
			}
		}
	# check cols
	c <- ncol(board);
	for (ii in 1:c){
		if(matcher(winner,board[,ii])){
			win <- TRUE;
			return(win); # someone one with a vertical match
			}
		}
		
	# check diagonals

	win <- checkDiags(board, r, c, winner);
	if (win) return(win);
	# now we just need to flip this bad boy and do it again
	win <- checkDiags(board[,c:1], r, c, winner);
	if (win) return(win);
		

	
	return(win); # nobody won yet 
	}
	
checkDiags <- function(board, r, c, winner){
	# we'll work bottom to top, left to right
	# we'll check diagonals with slope = -1 first, then switch to slope = 1
	
	# this works up the rows starting from the bottom

	win <- FALSE;
	w <- length(winner)

	current.vec <- c()
	element <- 1
	for (ii in nrow(board):1){
		#print("cur.r")
		#print(cur.r <- ii)
		cur.r <- ii
		cur.c <- 1;
		while( (cur.r <= r && cur.c <= c) ){
			current.vec[element] <- board[cur.r,cur.c]
			element <- element + 1
			cur.r <- cur.r + 1
			cur.c <- cur.c + 1
			}
		#print("current.vec")
		#print(current.vec)
		if (length(current.vec) >= w){
			if (matcher(winner,current.vec)){
				win <- TRUE;
				return(win);
				}
			}
		current.vec <- c()
		element <- 1
		}
		
	# this works in row 1, across the columns
	current.vec <- c()
	element <- 1
	for (ii in 1:ncol(board)){
		#print("cur.c")
		#print(cur.c <- ii)
		cur.c <- ii
		cur.r <- 1; # always start in row 1
		while( (cur.r <= r && cur.c <= c) ){
			current.vec[element] <- board[cur.r,cur.c]
			element <- element + 1
			cur.r <- cur.r + 1
			cur.c <- cur.c + 1
			}
		#print("current.vec")
		#print(current.vec)
		if (length(current.vec) >= w){
			if (matcher(winner,current.vec)){
				win <- TRUE;
				return(win);
				}
			}
		current.vec <- c()
		element <- 1
		}
		
	return(win);
	}
	
	
matcher <- function(winner, vec){
	w <- length(winner); 
	n <- length(vec);
	win <- FALSE;
	#print(vec) # DEBUG
	for (ii in 1:(n-w+1)){
		if (length(na.omit(vec[ii:(ii+w-1)]))>=w) { # this MIGHT be sufficient for checking... there shouldn't be *any* NAs in a winning string
		# if the above error checking doesn't work, try replacing *all* NAs in the vec with like... -99 or something
		# should be hidden from user
			if (sum(winner == na.omit(vec[ii:(ii+w-1)])) == w){
				win <- TRUE; return(win); 
				}
			}
		}
	return(win);
	}
