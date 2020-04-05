# an AI that plays only random moves, for comparison with AI.move	
random.AI.move <- function( board, player, 
							check.win=TRUE, check.block=TRUE, 
							parent, winval=3){
								
	# previous state is ONLY used in the blocking
	
	
	current.state <- stateID(board);
	choices <- identifyNextStates(board=board,player=player)
	
	# below is the code from the MCTS AI to always pick a board if the AI will win on that selection
	if (check.win){
		winning.board <- c()
		for (ii in 1:length(choices)){
			win <- checkWinner(toBoard(choices[ii],board),winval=winval,player=player)
			if (win) winning.board <- choices[ii]
			}
		if (!is.null(winning.board)) return(winning.board)
		}
	
	## same blocking code as MCTS now wields
#	if (check.block){
#		block.board <- c()
#		opponent <- (player+1)%%2
#		opponent.moves <- identifyNextStates(board=board,player=opponent)
		##print(opponent.moves)
#		for (ii in 1:length(opponent.moves)){
#			block <- checkWinner(toBoard(opponent.moves[ii],board),winval=winval,player=opponent)
#			if (block) {
				##print("we must block")
#				block.board <- choices[ii] 
#				}
			 ##we need to end up playing in the SAME COLUMN 
			 ##that opponent would play here, but NOT this board
#			} 
#		if (!is.null(block.board)){
			##print("opponent will win unless we block")
#			column <- stateToColumn(block.board,parent,board)
#			true.block.board <- addMove(board=toBoard(parent,board),column=column,player=player)
#			block.state <- stateID(true.block.board$board)
#			return(block.state) 
#			}
#		}

	# pick another board at random (using no tree information)
	if (length(choices) > 1) { 
		next.state <- sample(choices,1) 
		}
	else { 
		next.state <- choices # only one
		}
	return(next.state)
	}