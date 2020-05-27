# damka 
I use prolog(in swi-prolog) for the logic of the damka game and java for the gui.

to connect the two, you need to add to your system path
(by MyComputer->properties->advance properties and add to the PATH variable the path to swi folder,
	and the bin folder and the jpl-7.0.1.jar
	like, C:\Program Files\swipl ,C:\Program Files\swipl\bin , C:\Program Files\Java\jpl-7.0.1.jar
	make sure to restart the computer afterwords)
make sure the help file is in the packet of the java files(else error will occure).

hope youenjoy this game.

In this damka game i used alpha-beta puring (un improvment of minimax algorithem)
to search the best move(until reached the wanted depth) from a given position.
The huristic function is needed when:
	reached the depth limit 
  or
	can't move from this position
  or
	this position end the game(by win or draw)
This function depend on the level of the game,
but in any level the function return a positive value ,Val,if the black(computer) play
and negative value ,-Val , if the white play. 
At level - easy:
	Val = The length of black tools - the length of the white tools
	
At level - normal :
	each minion value is 1 and queen is 3
	Val = difference between 
			the sum of the value of the black tools 
				and 
			the sum of the value of the white tools
			
At level - hard :
	each minion have a value between 1 to (size of the board/2)
	the value depend on the row of the tool,
	the max value is in row  = 1 or size of the board and decrease until the half board
	each queen have a value of size of the board
	Val = difference between 
			the sum of the value of the black tools 
				and 
			the sum of the value of the white tools			
