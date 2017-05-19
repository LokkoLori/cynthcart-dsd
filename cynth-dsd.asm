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
	
loop:
	jsr readkeyboard
	jmp loop
	
	.import source "keys.asm"

readkeyboard:
	// clear notes
	lda #255
	sta KeyA
	sta KeyB
	sta KeyC
	sta KeyD
	sta KeyF

	ldx #4
	ldy #0
	// check for note keys being pressed
checkLoop:
	//lda #0
	//sta $dc02
	//lda $dc00
	//sta 1060
	
	//lda #ff
	//sta $dc02
	lda col,y
	beq quitCheck
	sta 56320
	lda 56321
	and row,y
	bne notPressed
	tya
	clc
	adc keyOffset
	sta KeyA,x
	dex
	bmi quitCheck
notPressed:
	iny
	jmp checkLoop

quitCheck:

	//lda keyTimer
	//beq contReadKeys
	
	ldx #5
	ldy #0
printkey:
	
	lda KeyA,y
	sta 1024,y
	iny
	dex
	bne printkey
	
	rts
	
vicirq:

    pha  //store registers into stack
    txa
    pha       
    tya
    pha      

    lda #$ff //necessary
    sta $d019

    lda #BLACK
    sta SCR_BORDER_COLOR
    lda #WHITE
    sta SCR_BACK_COLOR
	
	//do the visual
	
	lda #LIGHT_GREEN
    sta SCR_BACK_COLOR

    pla  //restore registers from stack
    tay       
    pla 
    tax       
    pla        

    rti
	
.print "success"