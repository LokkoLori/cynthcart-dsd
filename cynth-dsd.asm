.pc =$0801 "Basic Upstart Program"
:BasicUpstart($0820)

.import source "utils.asm"

.pc = $0820 "program"

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

	lda #$00
	sta sid+24
	
	//enable filters
	lda #%00000111
	sta sid+23
	
	.var ch1 = 0
	.var ch2 = 7
	.var ch3 = 14
	
	:initsidchannel(ch1)
	:initsidchannel(ch2)
	:initsidchannel(ch3)

loop:
	
	jsr joyhandling
	:handlestring(rqrow, rqcol, 12, ch1, string1, 0)
	:handlestring(rarow, racol, 12, ch2, string2, 40)
	:handlestring(rzrow, rzcol, 12, ch3, string3, 80)
	jmp loop
	
.macro geteasefunc()
{
	//giga hack
	lda actease_table
	sta funcloc+2
	sta 1350
	lda actease_table+1
	sta funcloc+3
	sta 1351
funcloc:
	nop
}
	
readjoystate:
	//saving joystate
	lda #0
	sta $dc02
	lda $dc00
	sta joystate
	sta 1900
	//release joy
	lda #$ff
	sta $dc02
	
	rts
	
joyhandling:

	jsr readjoystate
	lda #$ff
	cmp joystate
	bne joyactive
	jmp joypassive
joyactive:
	lda #0
	cmp joyisactive
	beq joyactivated
	jmp joyhandling_end
	
joyactivated:
	lda #1
	sta joyisactive
	lda joystate
	and #%00010000
	cmp #0
	beq firebutton
	:writeAddress(actease_table, easetable)
	//all filter
	lda #%01110000
	sta filters
	jmp startnote
firebutton:
	:writeAddress(actease_table, easetable_mute)
	//lowpass filter
	lda #%00010000
	sta filters
startnote:
	//pop up the volume
	:geteasefunc()
	lda easetable
	sta volume
	ora filters
	sta sid+24
	lda #0
	sta easepos
	jmp joyhandling_end

joypassive:
	lda #0
	cmp joyisactive
	bne joyreleased
	jmp joyhandling_end
	
joyreleased:
	lda #0
	sta joyisactive
	
joyhandling_end:
	rts
	
//---------------------------------------------------------------------------
	
easevolume:
	lda volume
	sta 1801
	cmp #0
	bne volumedown 
	jmp easevolume_end
volumedown:
	lda easepos
	sta 1802
	tax
	:geteasefunc()
	lda easetable,x
	sta volume
	ora filters
	sta sid+24
	sta 1803
	cmp #0
	beq easeover
	inc easepos
	sta 1804
	jmp easevolume_end
easeover:
	lda #0
	sta easepos
	
easevolume_end:
	rts
	
// ----------------------------------------

effects:

	lda #%0000111
	sta free
	:vibratechannel(ch1, cntr, free)
	:vibratechannel(ch2, cntr, free)
	:vibratechannel(ch3, cntr, free)
	
	rts
	
// ---------------------------------------

settings:
	
	lda #$ff
	cmp joystate
	beq readsetting_keys
	jmp settings_end

readsetting_keys:
	
	lda #$FE
	sta input_lo
	
	lda input_up
	and #$10
	beq f1_key_setting
	jmp read_f3_key
f1_key_setting: //standard bass tune E A D
	lda #16
	sta string1
	lda #21
	sta string2
	lda #26
	sta string3
	
	jmp settings_end
	
read_f3_key:
	lda input_up
	and #$20
	beq f3_key_setting
	jmp read_f5_key
f3_key_setting: //standard low 3 string E A D
	lda #28
	sta string1
	lda #33
	sta string2
	lda #38
	sta string3
	jmp settings_end
	
read_f5_key:
	lda input_up
	and #$40
	beq f5_key_setting
	jmp settings_end
f5_key_setting:
	//standard high 3 string G B E
	lda #43
	sta string1
	lda #47
	sta string2
	lda #52
	sta string3
	//jmp settings_end
	
	jmp settings_end
	
settings_end:
	rts

//---------------------------------------------------------------------------
	
	
joystate:
	.byte 0
joyisactive:
	.byte 0
volume:
	.byte 0
free:
	.byte 0
cntr:
	.byte 0
	
	// basetune, waveform
string1:
	.byte 28, %00100000
string2:  
	.byte 33, %00100000
string3:
	.byte 38, %00100000
	
vicirq:
	pha  //store registers into stack
    txa
    pha       
    tya
    pha 
	
	lda #$ff //necessary
    sta $d019
	
	inc cntr
	lda cntr
	sta 1800
	
	jsr settings
	//jsr effects	
	jsr easevolume
	
	pla  //restore registers from stack
    tay       
    pla 
    tax       
    pla
	
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
	

//        q    w    e    r    t    y    u    i    o    p    @    *    
rqcol:                                                                
	.byte $7F, $FD, $FD, $FB, $FB, $F7, $F7, $EF, $EF, $DF, $DF, $BF, 0
rqrow:                                                                
	.byte $40, $02, $40, $02, $40, $02, $40, $02, $40, $02, $40, $02 
	

//        A    S    D    F    G    H    J    K    L    :    ; 	 =    
racol:                                                                
	.byte $FD, $FD, $FB, $FB, $F7, $F7, $EF, $EF, $DF, $DF, $BF, $BF, 0
rarow:                                                                
	.byte $04, $20, $04, $20, $04, $20, $04, $20, $04, $20, $04, $20 
	

//        LS   Z    X    C    V    B    N    M    ,    .    /    RS   
rzcol:                                                                
	.byte $FD, $FD, $FB, $FB, $F7, $F7, $EF, $EF, $DF, $DF, $BF, $BF, 0
rzrow:                                                                
	.byte $80, $10, $80, $10, $80, $10, $80, $10, $80, $10, $80, $10 
	
	
	
easetable:
	.byte 15, 11, 10, 9, 8, 8, 7, 7, 7, 6, 6, 6, 6
	.byte 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5
	.byte 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4
	.byte 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3
	.byte 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
	.byte 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0
	
easetable_mute:
	.byte 10, 8, 6, 5, 5, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 2, 2, 1, 0
	
easepos:
	.byte 0
	
actease_table:
	.byte 0, 0
	
filters:
	.byte %00100000
	
.import source "freqtable.asm"
	
.print "success"