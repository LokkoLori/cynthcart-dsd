.pc =$0801 "Basic Upstart Program"
:BasicUpstart($c000)

.import source "utils.asm"

.pc = $c000 "program"

    sei       //disable maskable IRQs

    jsr $e544 //clear char screen

    lda #$7f
    sta $dc0d  //disable timer interrupts which can be generated by the two CIA chips
    sta $dd0d  //the kernal uses such an interrupt to flash the cursor and scan the keyboard, so we better stop it.

    lda $dc0d  //by reading this two registers we negate any pending CIA irqs.
    lda $dd0d  //if we don't do this, a pending CIA irq might occur after we finish setting up our irqmain we don't want that to happen.

    lda #$01   //this is how to tell the VICII to generate a raster interrupt
    sta $d01a

    lda #$00   //this is how to tell at which rasterline we want the irq to be triggered
    sta $d012

    lda #$1b   //as there are more than 256 rasterlines, the topmost bit of $d011 serves as
    sta $d011  //the 8th bit for the rasterline we want our irq to be triggered. here we simply set up a character screen, leaving the topmost bit 0.

    lda #$35 //we turn off the BASIC and KERNAL rom here
    sta $01 //the cpu now sees RAM everywhere except at $d000-$e000, where still the registers of

    :writeAddress($fffe,vicirq)
	
	cli
	
	
	.import source "string.asm"
	
loop:
	jmp loop
	

vicirq:
	:handlestring(qrow, qcol, 0, 0)
	:handlestring(arow, acol, 40, 0)
	:handlestring(zrow, zcol, 80, 0)
	rti

//        *    @    p    o    i    u    y    t    r    e    w    q
qcol:                     
	.byte $BF, $DF, $DF, $EF, $EF, $F7, $F7, $FB, $FB, $FD, $FD, $7F, 0
qrow:                     
	.byte $02, $40, $02, $40, $02, $40, $02, $40, $02, $40, $02, $40
	

//        =    ; 	:    L    K    J    H    G    F    D    S    A
acol:
	.byte $BF, $BF, $DF, $DF, $EF, $EF, $F7, $F7, $FB, $FB, $FD, $FD, 0
arow:
	.byte $20, $04, $20, $04, $20, $04, $20, $04, $20, $04, $20, $04
	

//        RS   /    .    ,    M    N    B    V    C    X    Z    LS
zcol:
	.byte $BF, $BF, $DF, $DF, $EF, $EF, $F7, $F7, $FB, $FB, $FD, $FD, 0
zrow:
	.byte $10, $80, $10, $80, $10, $80, $10, $80, $10, $80, $10, $80
	
.print "success"