.var KeyA = $700B
.var KeyB = $700C
.var KeyC = $700D
.var KeyD = $700E
.var KeyF = $700F

.var keyOffset = $7022
.var keyTimer = $7058
.var lastOsc = $7066


//------------------------------------------
// Keyboard Reading Data
//------------------------------------------

	// Column activation data
col:
	.byte $7F, $7F, $FD, $FD,  $FD, $FB, $FB, $FB,  $FB, $F7, $F7, $F7
	.byte $EF, $EF, $EF, $EF,  $DF, $DF, $DF, $BF,  $BF, $BF, $BF, $FE, 0

	// Row testing data
row:
	.byte $40, $08, $02, $01,  $40, $02, $01, $40,  $08, $02, $01, $40
	.byte $02, $01, $40, $08,  $02, $40, $08, $02,  $01, $40, $08, $01, 0

	//------------------------------------------
	// Note  c  #  d  #  e  f  #  g  #  a  #  b 
	//  Key  q  2  w  3  e  r  5  t  6  y  7  u 
	//  Hex  1  2  3  4  5  6  7  8  9  10 11 12
	//------------------------------------------
	// Note  c  #  d  #  e  f  #  g  #  a  #  b
	//  Key  i  9  o  0  p  @  -  *  &  |  CL RE
	//  Hex  13 14 15 16 17 18 19 20 21 22 23 24
	//------------------------------------------