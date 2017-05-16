; Commodore 64 Synthcart
; by Paul Slocum
;------------------------
; TEXT EDITOR TAB=3
;------------------------
;
; Change Log:
; 1.0 RELEASE
; + first official release
; 1.1 RELEASE
; + added SID HEX editor
; + changed tuning keys to avoid accidentally changing the tuning
; + added ability to turn the SID filter on and off
; 1.2.0
; + filter adjustment for SID Symphony
; 1.2.1
; + pitch fix for PAL
; + autodetection for PAL/NTSC
; 1.2.2
; + keyboard driver rewrite
; + moved tuning from piano to ASDF... 
; + moved filter On/Off to ZXC
; + keys swap portamento and octave
; + move video mode to SHFT+FGH and require shift for VIC mode
; + add key to switch between PAL/NTSC (RUNSTOP+Z/X)
; + separate functions for "show everything" and "variable init" 
; + video mode with no text (shift+:/;)
; + SID register $20-$26 edits all 3 oscillators at once
; + write help display routine
; + add help screen
; + reverse octave keys
; + add help for SID edit mode
; + redo hex editor piano KB layout
; + save SID edits
; + added "COPYING TO RAM" text when RAM copy is on
; 1.2.3
; + disable key-commands when 3 piano keys are held
;		to avoid quirks with keyboard matrix
; + fixed LFO indicator w/ fullscreen video in helpMode
; + reinstated shift-lock holding notes
; + add another octave to NTSC note chart (had 1 less than PAL)
; + add another octave to tuning charts for 5ths in top octave
; + auto-paddle on
; + set up custom test for space bar
; + disable settings changes w/ space bar
; + corrected "sine" to "tringl" in patch names
; + restored startup patch to "saw bass"
; o add support for second paddle (pitch, LFO depth, LFO rate, pulse width)
;	+ add key command to switch controls and turn off
;	+ add display of paddle status
;	+ add code to run things from the second paddle
; 1.2.4 release candidate 
; + designated paddle 1 and 2 in help screen
; 1.3.0 (future)
; - paddle 2 auto-on
; - make smarter key->oscillator assignment to fix long release
; - LFO during release
; - LFO -> PW
;--------------------------
; - more patches
; - filter type
; - mono-stack mode
; - noise reduction
; - sync
; - ring
; - filter Q
; - LFO -> Pulse Width and Volume and Filter
; - online help
; - add echo long/med/short
; - add mono stack mode
; - Envelope -> Filter Cutoff
; - MIDI adapter support
;------------------------

	processor 6502

	; Image run mode:
CART equ 0 ; run at $8000 off cartridge ROM
RAM equ 1 ; run at $1000, needs to be copied or decompressed into $1000
DISK equ 2 ; run at $8000, include initial load location word
KERNEL equ 3 ; set up as replacement for 8k BASIC section of KERNEL

;**********************************************************
;**********************************************************
;**********************************************************
MODE equ DISK   ; DISK, CART, KERNEL, or RAM (obsolete), 

RAMCOPY equ 1	; Copy program to RAM before running
;**********************************************************
;**********************************************************
;**********************************************************

	; Include .CRT emulation header
	;include crt_hdr.asm

	; *********************************************
	; ORG setup
	; *********************************************

	;==================================================
	; load from disk as PRG with auto-run
	IF MODE=DISK
BASEADDR equ 2047
	org BASEADDR ; the beginning of the BASIC program area

	; disk load location
	byte $01,$08
	; BASIC program to call the cynthcart machine code...
	; 10 SYS 2061
	byte $0b,$08,  $0a,$00,$9e,$32,  $30,$36,$31,$00,  $00,$00 
	; next effective address after this is 2061 / $80D
	ENDIF

	;==================================================
	; load from RAM, requires wrapper to load into RAM
	IF MODE=RAM
BASEADDR equ $1000
	org BASEADDR
	ENDIF

	;==================================================
	; straight cart ROM
	IF MODE=CART
BASEADDR equ $8000
	org BASEADDR
	word Startup
	word Startup
	; 5 byte cartridge startup code
	byte $C3, $C2, $CD, $38, $30
	ENDIF

	;==================================================
	; to replace BASIC ROM
	IF MODE=KERNEL
BASEADDR equ $8000
	org BASEADDR
	word $E394   ; RESET
	word $E37B   ; Warm Start
	ENDIF

	;---------------------------------------
	; variables and constants here
	;---------------------------------------
	include cynth_vars.asm

	; *********************************************
	; Start of program
	; *********************************************
Startup:
	
	IF RAMCOPY=1
	ldx #0
RAMTextCopy:
	lda RAMText,x
	beq quitRAMTextCopy
	cmp #64
	bmi showSpaceRAM
	sbc #64
showSpaceRAM
	sta 1024,x
	inx
	jmp RAMTextCopy
RAMText:
	byte "COPYING TO RAM...",0
quitRAMTextCopy:
	;------------
	ldx #8*4
	lda #<copyStart
	sta copyPtrS
	lda #>copyStart
	sta copyPtrS+1
	lda #<ramStart
	sta copyPtrD
	lda #>ramStart
	sta copyPtrD+1
ramCopy1:
	ldy #0
ramCopy2:
	lda (copyPtrS),y
	sta (copyPtrD),y
	dey
	bne ramCopy2
	inc copyPtrS+1
	inc copyPtrD+1
	dex
	bne ramCopy1
	jmp ramStart
copyStart:
	rorg $3000 ; RAM destination
ramStart:	
	ENDIF

	
	IF MODE!=DISK
	; System Startup Stuff
	; (not needed if starting from disk)
	sei
	jsr $FF84 ; initialize I/O devices
	jsr $FF87 ; initalise memory pointers
	jsr $FF8A ; restore I/O vectors
	jsr $FF81 ; initalise screen and keyboard
	cli
	ENDIF

;	byte $ea,$ea,$ea,$ea, $ea,$ea,$ea,$ea ; NOPs

	IF MODE=KERNEL
	org $A483
	ENDIF

	LDA $D011	; Disable VIC-II (This has to be done because of
	AND #$EF    ; badlines
	STA $D011

	; Disable RESTORE key
	lda #193
	sta 792
	
	lda #0
	sta BACK_COLOR
	sta BORD_COLOR

	;*****************************
	; Detect PAL/NTSC
	;*****************************
;palntsc:
	sei ; disable interrupts
wait:
	lda $d012
	bne wait ; wait for rasterline 0 or 256
wait1:
	lda $d011 ; Is rasterbeam in the area
	bpl wait1 ; 0-255? if yes, wait
wait2:
	ldy #$00
synch1:
	lda $d012
	cmp #$37 ; top PAL rasterline
	bne synch1
	lda $d012 ; if next is 0, then PAL
synch2:
	cmp $d012
	beq synch2
	lda $d012
	cli ; enable interrupts

	sta NTSCmode
	
	;****************************************************
	; init screen and variables
	;****************************************************

	lda #0
	jsr setFullScreenMode

	jsr variableInit
	jsr displayInit
	

	;===========================================
	;===========================================
	; Main Loop
	;===========================================
	;===========================================
Loop:

	; frame counter
	inc Frame
	lda Frame
	and #%00111111
	bne SkipHFrame
	inc FrameH
SkipHFrame:

	;-------------------------------
	; LFO
	;-------------------------------

	; Set current LFO modulation
	; into (pitch) shift variables

	;---------------------------
	; get pitch bend from paddle2
	lda paddle2
	cmp #4
	bne noPadBend
	lda paddleY
	sta bender
noPadBend:

	;----------------------------
	; get depth from paddle2
	lda paddle2
	cmp #3
	bne noPadLFO
	lda paddleY
	lsr
	lsr
	lsr
	tay
	jmp skipFixedLFODepth
	;---------------------
	; calculate LFO depth
	; increases per octave
noPadLFO:
	ldx LFODepth
	ldy LFODepthArray,x
skipFixedLFODepth:
	lda keyOffset 		; current octave offset
	cmp #12
	bmi endDepth
	iny
	cmp #36
	bne endDepth
	tya
	asl
	tay
endDepth	; y now contains the depth value
	

	; figure out LFO position
	lda FrameH
	ldx LFORate
	beq LFOSkip	
	lda Frame
	dex
	dex
	dex
	beq LFOSkip
	lsr
	inx
	beq LFOSkip
	lsr
	lsr
LFOSkip:
	lsr

	and #$0F
	tax
	lda LFOArrH,x
	sta shiftH1
	bne negativeLFO

	clc
	lda LFOArrL,x
depthLoop
	adc LFOArrL,x
	dey
	bne depthLoop
	sta shiftL1
	jmp endLFO	

negativeLFO
	lda LFOArrL,x
depthLoopN
	adc LFOArrL,x
	dey
	bne depthLoopN
	sta temp
	lda #255
	sec
	sbc temp
	sta shiftL1
endLFO:

	; if depth=0 then cancel LFO
	lda paddle2
	cmp #3
	beq doLFO
	lda LFODepth
	bne doLFO
	lda #0
	sta shiftL1
	sta shiftH1
doLFO:

	lda helpMode ; do show LFO if helpMode on
	bne showLFO
	lda fullScreenMode
	bne dontErase ; don't show LFO if in full screen mode
	;--------
	; Show it
showLFO:
	lda LFOdisp,x
	tax
	lda #160
	sta 1063,x

	;clear the previous one
	lda #32
	sta 1064,x
	cpx #1
	beq dontErase
	sta 1062,x
dontErase:

	; set up shift for second SID
	; chip, pitch a tad higher for
	; awesome chorus effect
	clc
	lda shiftL1
	adc #SID2OFFSET
	sta shiftL2
	lda shiftH1
	adc #0
	sta shiftH2

	; LFO->filter
;	lda shiftL1
;	clc
;	adc #128
;	sta SID1+SFILTH
;	sta SID2+SFILTH

skipLFO:

	; Echo stuff
;	inc EchoCur
;	inc EchoPtr
	; Save note in echo buffer
;	ldx EchoCur
;	sta EchoBuffer,x

	;---------------------
	; Read keyboard
	jsr readKeyboard
	;---------------------

	lda bender
	beq noBenderReset
	dec bender
	dec bender
noBenderReset

	; insert bender into LFO offset
	lda shiftL1
	sec
	sbc bender
	sta shiftL1
	lda shiftH1
	sbc #0
	sta shiftH1

	lda shiftL2
	sec
	sbc bender
	sta shiftL2
	lda shiftH2
	sbc #0
	sta shiftH2

	;DEBUG -- disable LFO/bender
;	lda #0
;	sta shiftL1
;	sta shiftL2
;	sta shiftH1
;	sta shiftH2

	;----------------------------

	; determine whether to use portamento player
	; or regular player...
	lda portOn
	bne playPort

	;Normal Voice Playing Loop
	;---------------------------------
	ldx #2
NsetRegs:
	stx temp
	ldy KeyA,x
	lda voiceOffset,x
	tax
	cpy #255
	bne NsoundOn
	lda #0
	beq NsoundOff
NsoundOn:

	; load note and deal
	; with tuning ------;
	lda NTSCmode			;
	beq palPlay			;
ntscPlay:				;
	clc					;
	lda NSoundLArr,y	;
	adc (tunePtrL),y	;
	sta pitchTmpL		;
	lda NSoundHArr,y	;
	adc (tunePtrH),y	;
	sta pitchTmpH		;	PAL/NTSC split
	jmp skipPalPlay		;
palPlay:				;
	clc					;
	lda PSoundLArr,y	;
	adc (tunePtrL),y	;
	sta pitchTmpL		;
	lda PSoundHArr,y	;
	adc (tunePtrH),y	;
	sta pitchTmpH		;
skipPalPlay:		;;;;;

	; play SID #1
	clc
	lda pitchTmpL
	adc shiftL1		; add LFO/bend/tuning offset
	sta SID1+SV1FL,x ; set low freq
	lda pitchTmpH
	adc shiftH1		; add LFO/bend/tuning offset
	sta SID1+SV1FH,x ; set high freq

	; play SID #2
	clc
	lda pitchTmpL
	adc shiftL2		; add LFO/bend/tuning offset
	sta SID2+SV1FL,x ; set low freq
	lda pitchTmpH
	adc shiftH2		; add LFO/bend/tuning offset
	sta SID2+SV1FH,x ; set high freq

	lda #1
	; Set voice gates on or off
NsoundOff:
	ora WaveType,x
	sta SID1+SV1WAVE,x
	sta SID2+SV1WAVE,x
	sta sidData+SV1WAVE,x

	ldx temp
	dex
	bpl NsetRegs
	;------------------------------------------------------------------
	; end of non-portamento playing loop
	;------------------------------------------------------------------
	jmp skipPort


	;--------------------------------------------------------------------
	;Portamento Voice Playing Loop
	;--------------------------------------------------------------------
playPort:
	; set up pointer to portamento speed array
	; (which is the tuning array)

	ldx #2
portLoop:
	ldy KeyA,x
	cpy #255
	bne noPlayNote
	jmp playNote
noPlayNote:
	lda Frame
	and #1
	beq noPlayNote2
	jmp playNote
noPlayNote2:


	;check portamn direction;
	lda NTSCmode				;
	beq palPortH			;
ntscPortH:					;
	lda pitchHA,x			;
	cmp NSoundHArr,y		;
	beq portCheckL			;
	bmi portUp				; PAL/NTSC split
	bpl portDown			;
palPortH:					;
	lda pitchHA,x			;
	cmp PSoundHArr,y		;
	beq portCheckL			;
	bmi portUp				;
	bpl portDown		;;;;;

portCheckL:
	
	;check portamn lowbyte--;
	lda NTSCmode				;
	beq palPortL			;
ntscPortL:					;
	lda pitchLA,x			;
	cmp NSoundLArr,y		;
	beq playNote ; note on	;
		;the stop so play	;
	bcs portDown			; PAL/NTSC split
	jmp skipPalPortL		;
palPortL:					;
	lda pitchLA,x			;
	cmp PSoundLArr,y		;
	beq playNote ; note on 	;
		;the stop so play	;
	bcs portDown			;
skipPalPortL:			;;;;;


portUp:
	lda pitchLA,x
	clc
	adc (portPtrL),y
	sta pitchLA,x
	lda pitchHA,x
	adc (portPtrH),y
	sta pitchHA,x
	lda #0 ; indicate port direction
	jmp oscCheck
portDown:
	lda pitchLA,x
	sec
	sbc (portPtrL),y
	sta pitchLA,x
	lda pitchHA,x
	sbc (portPtrH),y
	sta pitchHA,x
	lda #1	; indicate port direction

	; check for pitch oscillation
	; (which means it's at the correct note)
	; it's oscillating if: the port direction
	; has changed and the note hasn't.
oscCheck:
	cmp portLastDir,x
	sta portLastDir,x ; save it for next time 'round
	beq skipOsc
	tya
	cmp portLastNote,x	
	sta portLastNote,x ; save it...
	bne skipOsc

	; it's oscilating at; 
	; the note, so lock ;
	; it onto the actual;
	; note				;
	lda NTSCmode		;
	beq palLock			;
ntscLock:				;
	lda NSoundLArr,y	;
	sta pitchLA,x		;	PAL/NTSC split
	lda NSoundHArr,y	;
	sta pitchHA,x		;
	jmp skipPalLock		;
palLock:				;
	lda PSoundLArr,y	;
	sta pitchLA,x		;
	lda PSoundHArr,y	;
	sta pitchHA,x		;
skipPalLock:		;;;;;

skipOsc:

playNote:
	; deal with tuning
	clc
	lda pitchLA,x
	adc (tunePtrL),y
	sta pitchTmpL
	lda pitchHA,x
	adc (tunePtrH),y
	sta pitchTmpH

	tya	; Move the current key to A
	ldy voiceOffset,x
	cmp #255	; Check for note off
	bne soundOn
	lda #0
	beq soundOff
soundOn:


	; play SID #1
	clc
	lda pitchTmpL
	adc shiftL1		; add in LFO/bend/tuning offset
	sta SID1+SV1FL,y ; set low freq
	lda pitchTmpH
	adc shiftH1		; add in LFO/bend/tuning offset
	sta SID1+SV1FH,y ; set high freq

	; play SID #2
	clc
	lda pitchTmpL
	adc shiftL2		; add in LFO/bend/tuning offset
	sta SID2+SV1FL,y ; set low freq
	lda pitchTmpH
	adc shiftH2		; add in LFO/bend/tuning offset
	sta SID2+SV1FH,y ; set high freq

afterDStep

	lda #1
	; Set voice gates on or off
soundOff:
	ora WaveType,y
	sta SID1+SV1WAVE,y
	sta SID2+SV1WAVE,y
	sta sidData+SV1WAVE,x

	dex
	bmi quitPort
	jmp portLoop
quitPort:
	;--------------------------------------------------------------------
	;end of portamento play loop
	;--------------------------------------------------------------------


	;--------------------------------------------------------------------
	; continuation after playing loops...
	;--------------------------------------------------------------------
skipPort:

	; Reading paddles from Prog Ref Guide
	;-------------------------------------
pdlrd0                ; entry point for one pair (condition x 1st)
	sei
	lda Ciddra    ; get current value of ddr
	sta Buffer    ; save it away
	lda #$c0
	sta Ciddra    ; set port a for input
	lda #$80

pdlrd1
	sta PortA     ; address a pair of paddles
	ldy #$80      ; wait a while
pdlrd2
	nop
	dey
	bpl pdlrd2
 
	ldx SID1+25    ; get x value
	stx paddleX
	ldy SID1+26		; get y value
	sty paddleY

	lda PortA     ; time to read paddle fire buttons
	ora #80       ; make it the same as other pair
	sta Button    ; bit 2 is pdl x, bit 3 is pdl y

	lda Buffer
	sta Ciddra    ; restore previous value of ddr
	cli

	;-------------------------------------
	; auto paddle on
	lda paddle
	bne noPaddleAutoOn ; skip this if paddles are on
	cpx #160 
	bcc noPaddleTop
	lda #1
	sta paddleTop
noPaddleTop:
	cpx #96
	bcs noPaddleBottom
	lda #1
	sta paddleBottom
noPaddleBottom:
	;-----------
	lda paddleTop 			; if both paddle regions have
	beq noPaddleAutoOn	; been hit, then...
	lda paddleBottom
	beq noPaddleAutoOn
	lda #1					; turn paddle on
	jsr setPaddles
noPaddleAutoOn:

	;-------------------------------------
	; check to see if paddle control is on
	lda paddle
	beq noPaddleControl

	; paddle1 -> filter
	txa
	sta SID1+SFILTH
	sta sidData+SFILTH
	clc
	adc #SID_SYMPHONY_FILTER_OFFSET
	bcc noPaddleRoll
	lda #255
noPaddleRoll:
	sta SID2+SFILTH
noPaddleControl

	;-------------------------------------
	; paddle 2

;	paddle 2 -> Pulse Width
	lda paddle2
	cmp #1
	bne skipPW
	lda paddleY
	cmp #245	; check for top limit (= no sound)
	bcc notPTop	
	lda #245 ; limit to maximum
	sta paddleY
notPTop:
	;------------------
	lda paddleY
	lsr
	lsr
	lsr
	lsr
	sta SID1+SV1PWH
	sta SID1+SV2PWH
	sta SID1+SV3PWH
	sta SID2+SV1PWH
	sta SID2+SV2PWH
	sta SID2+SV3PWH
	sta sidData+SV1PWH
	sta sidData+SV2PWH
	sta sidData+SV3PWH
	;-------------
	lda paddleY
	asl
	asl
	asl
	asl
	ora #$0F
	sta SID1+SV1PWL
	sta SID1+SV2PWL
	sta SID1+SV3PWL
	sta SID2+SV1PWL
	sta SID2+SV2PWL
	sta SID2+SV3PWL
	sta sidData+SV1PWL
	sta sidData+SV2PWL
	sta sidData+SV3PWL
skipPW:

	;END paddle ------------------------------

	; Turn off Vic when no notes are playing
	lda VICMode
	beq vicOff 
	cmp #2
	beq vicOn
	lda dispOn
	bne vicOn
	lda KeyA
	cmp #255
	bne vicOn
	lda KeyB
	cmp #255
	bne vicOn
	lda KeyC
	cmp #255
	bne vicOn
vicOff:
	; Vic off
	lda $d011
	and #$EF
	sta $D011

	jmp endVic
vicOn:
	; Vic on
	lda $d011
	ora #$10
	sta $d011

endVic:
	lda #0
	sta dispOn

	lda KeyA
	sta temp
	jsr drawPattern
	lda KeyB
	sta temp
	jsr drawPattern
	lda KeyC
	sta temp
	jsr drawPattern

	jmp Loop
	;====================================================
	; bottom of main loop
	;====================================================


	;------------------------------------------
	; code to draw colored character patterns
	;------------------------------------------
;PTRNTEXTBASE equ 1224
PTRNTEXTBASE equ 1224-40*5
;PTRNCOLORBASE equ 55496
PTRNCOLORBASE equ 55496-40*5


drawPattern
	; don't draw video when in help mode
	lda helpMode
	beq continueVideo
	rts
continueVideo:
	;---------
	; setup
	ldx patPtr
	inx
	cpx #40
	bne noPatReset
	ldx #0
noPatReset:
	stx patPtr	
	;---------
	ldx #5
	; main pattern loop
patternLoop:
	lda patOffset,x
;	clc
	adc patPtr
	tay
;	clc
	lda temp
	cmp #255
	beq skipExtraColors	
	lda Frame
	and videoMode
	clc
	adc temp
skipExtraColors:
	adc #190
	sta (lowTextPtr),y
	sta PTRNTEXTBASE+200,y
	sta PTRNTEXTBASE+400,y
	sta PTRNTEXTBASE+600,y
	cpy #248
	bmi noTopText
	sta PTRNTEXTBASE+800,y
noTopText:
	sbc #13
	sta (lowColorPtr),y
	sta PTRNCOLORBASE+200,y
	sta PTRNCOLORBASE+400,y
	sta PTRNCOLORBASE+600,y
	cpy #248
	bmi noTopColor
	sta PTRNCOLORBASE+800,y
noTopColor:
	dex
	bpl patternLoop
	rts
	;--------------------- end of draw patterns

pwLFO
	byte %00010000, %00110000, %01010000, %01110000
	byte %10010000, %10110000, %11010000, %11110000
	byte %11110000, %11010000, %10110000, %10010000
	byte %01110000, %01010000, %00110000, %00010000

patOffset
	byte 0,40,80,120,160,200

voiceOffset:
	byte $0,$7,$E


;------------------------------------------
; Read the keyboard
;------------------------------------------
readKeyboard:
	; Keyboard read setup
	lda #127
	sta 56333

	;------------------------------------------
	; determine which keyset to use 
	; (raw/shift/commodorekey/runstop)
	;------------------------------------------

	; default is raw key functions (no modified keys)
	lda #<rawKeyFunctions		;-
	sta keyPtrL						;
	lda #>rawKeyFunctions		;
	sta keyPtrH						;-

	; Check for Shift/C=
	lda #~64 ; Right Shift
	sta 56320
	lda 56321
	and #16
	bne notAltKeys					;-
	lda #<shiftKeyFunctions		;
	sta keyPtrL						;
	lda #>shiftKeyFunctions		;-
	sta keyPtrH
notAltKeys:
	lda #~2 ; Left Shift
	sta 56320
	lda 56321
	and #128
	bne notAltKeys2				;-
	lda #<shiftKeyFunctions		;
	sta keyPtrL						;
	lda #>shiftKeyFunctions		;-
	sta keyPtrH
	jmp doKeyCheck
notAltKeys2:
	lda #~128 ; C= key
	sta 56320
	lda 56321
	and #32
	bne notAltKeys3
	lda #<commKeyFunctions		;-
	sta keyPtrL						;
	lda #>commKeyFunctions		;
	sta keyPtrH						;-
notAltKeys3:
	lda #~128 ; Run Stop
	sta 56320
	lda 56321
	and #128
	bne notAltKeys4
	lda #<runstopKeyFunctions	;-
	sta keyPtrL						;
	lda #>runstopKeyFunctions	;
	sta keyPtrH						;-
notAltKeys4:

	; handle keytimer
	; (to avoid accidental keypresses on shifted keys)
	lda keyTimer
	beq readKeys
	dec keyTimer
	jmp startCheck
readKeys:

	;********************************
	; Check for Notes Being Pressed
	;********************************
startCheck:
	; clear notes
	lda #255
	sta KeyA
	sta KeyB
	sta KeyC

	ldx #2
	ldy #0
	; check for note keys being pressed
checkLoop:
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
	bne checkLoop

quitCheck:

	lda keyTimer
	beq contReadKeys
	rts
contReadKeys:
	stx lastOsc

	; check for space bar
	lda #~$80
	sta 56320
	lda 56321
	and #$10
	bne noSpace
	jsr bendBender
	jmp skipKeyCheck
noSpace:

	; skip key command check is 3 or more piano keys
	; are held to avoid quirks with C64 keyboard
	; matrix hardware.
	ldx lastOsc
	cpx #255
	bne doKeyCheck
	jmp skipKeyCheck
doKeyCheck:

	;********************************
	; Generic command key check
	;********************************
	
	ldx #30*2	;28+1 keys to check, set and read value (2 bytes) for each key
keyChkLoop:
	lda commandKeys,x
	sta 56320
	lda 56321
	and commandKeys+1,x
	bne keyNotDown

	; key down!
	;-----------
	txa ;multiply x by 2
	asl ;  to get the offset 
	tay ;  into the key functions array
	
	; get address of function to call
	lda (keyPtrL),y
	sta temp16L
	iny
	lda (keyPtrL),y
	sta temp16H
	beq keyNotDown	;if the MSB address is zero, then there is
					;  no function assigned to this key so quit

	; put return address onto stack to simulate JSR with a JMP()
	lda #>returnAddress
	pha 
	lda #<returnAddress
	pha 

	; save the value of X
	stx saveX

	iny
	lda (keyPtrL),y	;get value to pass to function for X
	sta keyTemp
	iny
	lda (keyPtrL),y	;get value to pass to function for A
	tay
	lda keyTemp

	; indirect jump to function, which acts as a JSR since 
	;   we pushed the return address onto the stack
	jmp (temp16L)
	
	nop
returnAddress:
	nop

	ldx saveX ; restore X

	; only set the keytimer when a raw key is used
	lda keyPtrL
	cmp #<rawKeyFunctions
	beq keyNotDown

	lda #KEYTIME
	sta keyTimer	
	;-----------
	jmp skipKeyCheck	; quit keycheck after a key is found

keyNotDown:
	dex
	dex
	bpl keyChkLoop

skipKeyCheck:

	; Handle fifths if on:
	; play fifth of higher note
	; if third note isn't being played
	ldx lastOsc
	lda fifths
	beq noFifth2
	cpx #2
	beq noFifth2
	cpx #0
	bmi noFifth2
	lda KeyA+1,x
	clc
	adc #7
	sta KeyA,x
noFifth2

	; done
	rts

	;-------------------------------------------
	; new key-based set functions
	;-------------------------------------------

ksavePatch
	lda #1
	sta customPatchSaved
	;-------------------
	lda #10
	sta patchSetY
	jsr showPatchName
	;----------------
	ldx #$19
saveLoop:
	lda sidData,x
	sta sidSaveData,x
	dex
	bpl saveLoop
	;----------------
	lda fifths
	sta saveFifths
	lda paddle
	sta savePaddle
	lda octave
	sta saveOctave
	lda portOn
	sta savePortOn
	lda portSpd
	sta savePortSpeed
	lda LFODepth
	sta saveLFODepth
	lda LFORate
	sta saveLFORate
	lda volume
	sta saveVolume
	lda volModeRAM
	sta saveVolMode
	lda filter
	sta saveFilter
	;----------------	
	rts
	
kloadPatch
	; don't load patch if none has been saved
	lda customPatchSaved
	bne contLoadPatch
	rts	
contLoadPatch:
	lda #11
	sta patchSetY
	jsr showPatchName
	;-----------------------
	lda saveVolMode
	sta volModeRAM
	;.....................	
	lda saveVolume
	sta volume
	;.....................	
	lda saveFifths
	sta fifths
	jsr setFifthsText
	;.....................	
	lda savePaddle
	jsr setPaddles
	;.....................	
	lda saveOctave
	jsr setOctave
	;.....................	
	lda savePortOn
	sta portOn
	lda savePortSpeed
	jsr setPort
	;.....................	
	lda saveLFODepth
	jsr setLFODepth
	;.....................	
	lda saveLFORate
	jsr setLFORate
	;.....................	
	lda saveFilter
	sta filter
	;----------------	
	ldx #$19
loadLoop:
	lda sidSaveData,x
	sta SID1,x
	sta SID2,x
	sta sidData,x
	dex
	bpl loadLoop
	;----------------	
	lda sidData+SV1WAVE
	sta WaveType
	lda sidData+SV2WAVE
	sta WaveType2
	lda sidData+SV3WAVE
	sta WaveType3
	;----------------	
	rts

khelp
	jsr clrScr
	jsr displayInit
	lda #KEYTIME
	sta keyTimer	
	lda helpMode
	eor #1
	sta helpMode
	cmp #0
	beq skipDrawHelp
	;---------------
	ldx #>normalHelp ;low/MSB
	ldy #<normalHelp ;high/LSB
	jmp displayPage
skipDrawHelp:
;	ldx #39
;	lda #32
;clearLastRow:
;	sta 1024+23*40,x
;	dex
;	bpl clearLastRow
	rts

ksetPalNtsc:
	sta NTSCmode
	jmp displayInit

	; bend the bender down
bendBender:
	lda bender
	cmp #252
	beq notBender
	inc bender
	inc bender
	inc bender
	inc bender
notBender:
	rts
	
	; set VIC video chip mode
setVIC:
	sta VICMode
	rts
	
	; set paddle on/off
ksetPaddles:
	jsr setPaddles
	lda filter
	jmp setFilter

ksetPad2:
	sta paddle2
	cmp #0
	beq skipLastPadSave
	sta lastPad2 ; save last pad2 setting (other than "OFF")
skipLastPadSave
	asl
	asl
	clc
	adc #PAD2VALTEXT ; add in offset into value text array
	tax
	ldy #PAD2TEXT ; screen position
	jmp updateText

ksetFilter:
	ldx #0
	stx paddle ; turn off paddle controller first
	jsr setFilter
	lda #0
	jmp setPaddles

kfiltOnOff:
	sty filterStatus
	;------------------
	lda sidData+SFILTC
	ora filtOrValue,y
	and filtAndValue,y
	sta SID1+SFILTC
	sta sidData+SFILTC
	;------------------
	lda sidData+SFILTC
	ora filtOrValue,y
	and filtAndValue,y
	sta SID2+SFILTC
	;------------------
	lda filtDisableValue,y
	sta filterDisable
	;------------------
showFiltOnOff:	
	lda filtTextValue,y
	tax
	ldy #FILTERTEXT2
	jmp updateText
		
filtOrValue:
	byte $0F,0,0
filtAndValue:
	byte $FF,$F0,$F0
filtDisableValue:
	byte 0,0,1
filtTextValue:	
	byte 4,0,DISABLED

ksetTune:
	sty tuneSetting
	lda tuneArrPtrLL,y
	sta tunePtrL
	lda tuneArrPtrLH,y
	sta tunePtrL+1
	lda tuneArrPtrHL,y
	sta tunePtrH
	lda tuneArrPtrHH,y
	sta tunePtrH+1
	tya
	asl
	asl
	clc
	adc #TUNING
	tax
	ldy #TUNINGTEXT
	jmp updateText
	

setFullScreenMode:
	sta fullScreenMode
	cmp #0
	beq notFullScreen
	;--------
	lda #<(PTRNTEXTBASE)
	sta lowTextPtr
	lda #>(PTRNTEXTBASE)
	sta lowTextPtr+1
	lda #<(PTRNCOLORBASE)
	sta lowColorPtr
	lda #>(PTRNCOLORBASE)
	sta lowColorPtr+1
	rts
	;--------
notFullScreen:
	lda #<(PTRNTEXTBASE+200)
	sta lowTextPtr
	lda #>(PTRNTEXTBASE+200)
	sta lowTextPtr+1
	lda #<(PTRNCOLORBASE+200)
	sta lowColorPtr
	lda #>(PTRNCOLORBASE+200)
	sta lowColorPtr+1
	jsr displayInit
	rts


	;--------------------------------
	; Set Sync
	;--------------------------------
setSync
	sta videoMode
	tay
	asl
	asl
	beq syncOff
	sta temp
	ora WaveType
	sta WaveType
	lda temp
	ora WaveType2
	sta WaveType2
	lda temp
	ora WaveType3
	sta WaveType3
	jmp contSync
syncOff
	eor #255
	sta temp
	and WaveType
	sta WaveType
	lda temp
	and WaveType2
	sta WaveType2
	lda temp
	and WaveType3
	sta WaveType3
contSync:
	tya
	asl
	asl
	tax	
	ldy #SYNCTEXT
	jmp updateText
	rts

	;--------------------------------
	; Set Video Mode
	;--------------------------------
setVideoMode
	sta videoMode
	sty videoText
	tya	
	clc
	adc #"0"
	sta 1024+VIDEOTEXT
	rts

	;--------------------------------
	; Set Paddles
	;--------------------------------
setPaddles
	sta paddle
	asl
	bne noFilterReset
	ldx filter
	stx SID1+SFILTH
	stx SID2+SFILTH
	sta sidData+SFILTH
noFilterReset:
	ldy #0
	sty paddleTop
	sty paddleBottom
showPaddle:
	asl
	tax
	ldy #PADDLETEXT
	jmp updateText


	;--------------------------------
	; Set LFO Depth
	;--------------------------------
setLFODepth
	sta LFODepth
	ldy #LFODEPTHTEXT
	clc
	adc #"0"
	sta 1024,y
	lda #32
	ldx #8
	rts


	;--------------------------------
	; Set LFO Rate
	;--------------------------------
setLFORate
	sta LFORate
	ldy #LFORATETEXT
	clc
	adc #"0"
	sta 1024,y
	lda #32
	ldx #8
LFOClear:
	sta 1064,x
	dex
	bpl LFOClear
	rts

	;--------------------------------
	; Set Release
	;--------------------------------
	; A = release value
setRelease
	sta release
	sta SID1+SV1SR
	sta SID1+SV2SR
	sta SID1+SV3SR
	sta SID2+SV1SR
	sta SID2+SV2SR
	sta SID2+SV3SR
	sta sidData+SV1SR
	sta sidData+SV2SR
	sta sidData+SV3SR
	;----------------
showRelease:
	ldy #RELTEXT
	lda #REL_SHORT
	cmp release
	bmi notRel0
	lda #"0"
	jmp setReleaseText
notRel0:
	lda #REL_MED
	cmp release
	bmi notRel1
	lda #"1"
	jmp setReleaseText
notRel1:
	lda #"2"
setReleaseText:
	sta 1024,y
	rts

	;--------------------------------
	; Set Attack
	;--------------------------------
	; A = Attack value
setAttack:
	sta attack
	sta SID1+SV2AD
	sta SID1+SV3AD
	sta SID2+SV2AD
	sta SID2+SV3AD
	sta SID1+SV1AD
	sta SID2+SV1AD
	sta sidData+SV2AD
	sta sidData+SV3AD
	sta sidData+SV1AD
	;----------------
showAttack:
	ldy #ATKTEXT
	cmp #ATK_LONG
	bcc notAtk0
	lda #"2"
	jmp setAttackText
notAtk0:
	cmp #ATK_MED
	bcc notAtk1
	lda #"1"
	jmp setAttackText
notAtk1:
	lda #"0"
setAttackText
	sta 1024,y
	rts


	;-----------------------------------
	; Set Volume to A (for key command)
	;-----------------------------------
ksetVolume
	sta volume
	
	;-----------------------------------
	; Set Volume
	;-----------------------------------
setVolume
	lda volModeRAM
	and #$F0
	ora volume
	sta SID1+SVOLMODE
	sta SID2+SVOLMODE
	sta sidData+SVOLMODE

	; set volume text
	ldy #VOLTEXT
	lda #VOLLOW
	cmp volume
	bmi notLow
	ldx #VLOW
	jmp updateText
notLow
	lda #VOLMED
	cmp volume
	bmi notMed
	ldx #VMED
	jmp updateText
notMed
	ldx #VHIGH
	jmp updateText
	;-------------------------------------


	;-----------------------------------
	; Set Octave
	;-----------------------------------
setOctave
	sta octave
	tax
	lda octaveTable,x
	sta keyOffset
	txa
	clc
	adc #"0"
	tax
	sta 1024+OCTAVETEXT
	rts
	


	;-----------------------------------
	; Set Fifths Text
	;-----------------------------------
setFifthsText
	lda fifths
	asl
	asl
	tax
	ldy #FIFTHSTEXT
	jmp updateText
	;------------------------------------


	;-----------------------------------
	; Set Fifths
	;-----------------------------------
setFifths
	sta fifths
	jsr setFifthsText
	rts

	;-----------------------------------
	; Set Filter
	;-----------------------------------
setFilter
	sta SID1+SFILTH
	sta SID2+SFILTH
	sta sidData+SFILTH

	sta filter
	ldx paddle	; see if paddle is on (overrides filter)
	beq showFilter
	lda #16 ; "P"
	bne endFilter ; always taken
showFilter:
	lsr
	lsr
	lsr
	lsr
	lsr
	clc
	adc #"0"
endFilter:
	sta 1024+FILTERTEXT
	rts

	;-----------------------------------
   ; set port with A,Y (for key command)
	;-----------------------------------
ksetPort
   sta portOn
   sty portSpd

	;-----------------------------------
	; Set Port
	;-----------------------------------
setPort
;   sta portOn
;   sty portSpd

	lda portOn
	clc
	adc #48
	sta 1024+PORTTEXT

	; set up pointer to portamento speed array
	; (which is the tuning array)
	lda portSpd
	clc
	adc #4
	tay
;	ldy #5 ; portSpd
	lda tuneArrPtrLL,y
	sta portPtrL
	lda tuneArrPtrLH,y
	sta portPtrL+1
	lda tuneArrPtrHL,y
	sta portPtrH
	lda tuneArrPtrHH,y
	sta portPtrH+1
	rts

;------------------------------------------
; Keyboard Reading Data
;------------------------------------------

	; Column activation data
col:
	byte $7F, $7F, $FD, $FD,  $FD, $FB, $FB, $FB,  $FB, $F7, $F7, $F7
	byte $EF, $EF, $EF, $EF,  $DF, $DF, $DF, $BF,  $BF, $BF, $BF, $FE, 0

	; Row testing data
row:
	byte $40, $08, $02, $01,  $40, $02, $01, $40,  $08, $02, $01, $40
	byte $02, $01, $40, $08,  $02, $40, $08, $02,  $01, $40, $08, $01, 0

	;------------------------------------------
	; Note  c  #  d  #  e  f  #  g  #  a  #  b 
	;  Key  q  2  w  3  e  r  5  t  6  y  7  u 
	;  Hex  1  2  3  4  5  6  7  8  9  10 11 12
	;------------------------------------------
	; Note  c  #  d  #  e  f  #  g  #  a  #  b
	;  Key  i  9  o  0  p  @  -  *  &  |  CL RE
	;  Hex  13 14 15 16 17 18 19 20 21 22 23 24
	;------------------------------------------

commandKeys: ; row, column
	byte ~$80, $10		;spc 28
	byte ~$02, $04		;A   0	
	byte ~$02, $20		;S   1
	byte ~$04, $04		;D   2
	byte ~$04, $20		;F   3
	byte ~$08, $04		;G   4
	byte ~$08, $20		;H   5
	byte ~$10, $04		;J   6
	byte ~$10, $20		;K   7
	byte ~$20, $04		;L   8
	byte ~$20, $20		;:   9
	byte ~$40, $04		;;   10
	byte ~$40, $20		;=   11
	byte ~$02, $10		;Z   12
	byte ~$04, $80		;X   13
	byte ~$04, $10		;C   14
	byte ~$08, $80		;V   15
	byte ~$08, $10		;B   16
	byte ~$10, $80		;N   17
	byte ~$10, $10		;M   18
	byte ~$20, $80		;,   19
	byte ~$20, $10		;.   20
	byte ~$40, $80		;/   21
	byte ~$01, $80		;u/d 22
	byte ~$01, $04		;l/r 23
	byte ~$01, $10		;F1  24
	byte ~$01, $20		;F3  25
	byte ~$01, $40		;F5  26
	byte ~$01, $08		;F7  27
	byte ~$01, $02		;ret 29
	byte ~$80, $04		;Larr 30

rawKeyFunctions:
	;    functionPointer, inputData:Y,A
	word bendBender, $0000		;space
	word setRelease, REL_SHORT	;A
	word setRelease, REL_MED	;S
	word setRelease, REL_LONG	;D
	word $0000, $0000			;F
	word $0000, $0000			;G
	word 0,0					;H
	word 0,0	 				;J
	word 0,0      				;K
	word 0,0      				;L
	word ksetVolume,VOLLOW 		;:
	word ksetVolume,VOLMED 		;;
	word ksetVolume,VOLHIGH		;=
	word setPatch, $0000		;Z
	word setPatch, $0100		;X
	word setPatch, $0200		;C
	word setPatch, $0300		;V
	word setPatch, $0400		;B
	word setPatch, $0500		;N
	word setPatch, $0600		;M
   word setPatch, $0700		;,
	word setPatch, $0800		;.
	word setPatch, $0900		;/
	word setFifths, 0			;up/down
	word setFifths, 1			;left/right
	word setOctave, 3			;F1
	word setOctave, 2			;F3
	word setOctave, 1			;F5
	word setOctave, 0			;F7
	word khelp,0   				;return
	word kloadPatch,0				;Larrow

shiftKeyFunctions:
	;    functionPointer, inputData:Y,A
	word 0, $0000				;space
	word setAttack,ATK_SHORT;A
	word setAttack,ATK_MED 	;S
	word setAttack,ATK_LONG	;D
	word setVideoMode,$0108	;F
	word setVideoMode,$0203	;G
	word setVideoMode,$0315	;H
	word setVIC, 2	 			;J
	word setVIC, 1				;K
	word setVIC, 0				;L
	word setFullScreenMode, $0001	;:
	word setFullScreenMode, $0000	;;
	word 0, $0000				;=
	word setLFORate,0			;Z
	word setLFORate,1			;X
	word setLFORate,2			;C
	word setLFORate,3			;V
	word setLFODepth,0		;B
	word setLFODepth,1		;N
	word setLFODepth,2		;M
   word setLFODepth,3		;,
	word 0, $0000				;.
	word 0, $0000				;/
	word ksetPaddles, 0		;up/down
	word ksetPaddles, 1		;left/right
	word ksetPort, $0503		;F1
	word ksetPort, $0302		;F3
	word ksetPort, $0101		;F5
	word ksetPort, $0000		;F7
	word 0,0      				;return
	word 0,0						;Larrow

commKeyFunctions:
	;    functionPointer, inputData:Y,A
	word 0, $0000			;space
	word ksetFilter, $0000	;A
	word ksetFilter, $0020  ;S
	word ksetFilter, $0040	;D
	word ksetFilter, $0060	;F
	word ksetFilter, $0080	;G
	word ksetFilter, $00A0	;H
	word ksetFilter, $00C0	;J
	word ksetFilter, $00E0	;K
	word 0, $0000			;L
	word 0, $0000		 	;:
	word 0, $0000		 	;;
	word 0, $0000			;=
	word kfiltOnOff,$0000	;Z
	word kfiltOnOff,$0100	;X
	word kfiltOnOff,$0200	;C
	word ksetPad2, $0000	;V
	word ksetPad2, $0001	;B
	word ksetPad2, $0003	;N
	word ksetPad2, $0004	;M
	word 0, $0000			;,
	word 0, $0000			;.
	word 0, $0000			;/
	word 0, $0000			;up/down
	word 0, $0000			;left/right
	word 0, $0000			;F1
	word 0, $0000			;F3
	word 0, $0000			;F5
	word 0, $0000			;F7
	word 0,0   				;return
	word 0,0					;Larrow

runstopKeyFunctions:
	;    functionPointer, inputData:Y,A
	word 0, $0000			;space
	word ksetTune,$0000	;A
	word ksetTune,$0100  ;S
	word ksetTune,$0200	;D
	word ksetTune,$0300	;F
	word ksetTune,$0400	;G
	word ksetTune,$0500	;H
	word ksetTune,$0600	;J
	word ksetTune,$0700	;K
	word ksetTune,$0800	;L
	word ksetTune,$0900 	;:
	word ksetTune,$0A00 	;;
	word 0, $0000			;=
	word ksetPalNtsc,$0001	;Z
	word ksetPalNtsc,$0000	;X
	word 0, $0000			;C
	word 0, $0000			;V
	word 0, $0000			;B
	word 0, $0000			;N
	word 0, $0000			;M
   word 0, $0000			;,
	word 0, $0000			;.
	word 0, $0000			;/
	word 0, $0000			;up/down
	word 0, $0000			;left/right
	word SIDEdit,$0000	;F1
	word ksavePatch,0		;F3
	word 0,0					;F5
	word SIDEdit,$0001	;F7
	word 0,0   				;return
	word 0,0					;Larrow


patchName
	byte "SAW BASS        " ;0
	byte "SAW BASS 2      " ;1
	byte "SAW FILTER 5THS " ;2
	byte "SAW PORT        " ;3
	byte "PULSE           " ;4
	byte "PULSE HIGH      " ;5
	byte "TRINGL HIGH LONG" ;6
	byte "TRINGL PORT     " ;7
	byte "NOISE           " ;8
	byte "MUTE            " ;9
	byte "PATCH SAVED     " ;10
	byte "CUSTOM PATCH    " ;11

patchPaddle
	byte 	0,		0,		1,		0,		1,		1,		 0,		0,		1, 		0
patchPort                                                
	byte 	0,		2,		0,		2,		0,		2,		 0,		2,		0, 		0
patchFifths                                              
	byte 	0,		0,		1,		0,		0,		0,		 0,		0,		0, 		0
patchFiltCut                                             
	byte 	$c0,	$80,	$00,	$FF,	$00,	$00,	$c0,	$c0,	$00, 	0
patchVol                                                 
	byte 	$f,		$F,		$b,		$9,		$b,		$8,		$F,		$F,		$c, 	0
patchPWL                                                 
	byte 	0,		0,		0,		0,		0,		0,		 0,		0,		0, 		0
patchPWH                                                 
	byte 	8,		8,		8,	    8,	 	8,		8,		 8,		8,		8, 		0
patchWave                                                
	byte 	$20,	$20,	$20,	$20,	$40,	$40,	$10,	$10,	$80, 	0
patchAD                                                  
	byte 	$00,	$00,	$00,	$00,	$00,	$00,	$00,	$00, 	$00, 	0
patchSR                                                  
	byte 	$e0,	$e5,	$e0,	$e0,	$e0,	$e0,	$eE,	$e0,	$e0, 	0
patchFilt                                                
	byte 	$EF,	$EF,	$0F,	$0F,	$EF,	$EF,	$0F,	$0F,	$EF, 	$EF
patchMode                                                
	byte 	$10,	$10,	$10,	$10,	$20,	$20,	$10,	$10,	$10, 	$10
patchOctave                                               
	byte 	0,	   0,    	1,		1,		1,		2,		3,		2,		0, 		0

octaveTable
	byte 0,12,24,36,48

	;----------------------------------------
	; subroutine to set up patch 
	; (patch # stored in Y)
	;----------------------------------------
setPatch
	sty patchSetY

	lda patchVol,y
	sta volume

	lda patchPaddle,y
	jsr setPaddles
	ldy patchSetY

;	sta paddle

	lda patchFiltCut,y
	jsr setFilter

	lda patchPort,y
	sta portOn
	sta dispOn
	lda #3
	sta portSpd
	jsr setPort
	ldy patchSetY

	lda patchFifths,y
	sta fifths
	jsr setFifthsText
	ldy patchSetY

	lda patchPWL,y
	sta SID1+SV1PWL
	sta SID1+SV2PWL
	sta SID1+SV3PWL
	sta SID2+SV1PWL
	sta SID2+SV2PWL
	sta SID2+SV3PWL
	sta sidData+SV1PWL
	sta sidData+SV2PWL
	sta sidData+SV3PWL

	lda patchPWH,y
	sta SID1+SV1PWH
	sta SID1+SV2PWH
	sta SID1+SV3PWH
	sta SID2+SV1PWH
	sta SID2+SV2PWH
	sta SID2+SV3PWH
	sta sidData+SV1PWH
	sta sidData+SV2PWH
	sta sidData+SV3PWH

	lda patchWave,y
	sta WaveType2
	sta WaveType3
	sta WaveType

	lda patchAD,y
	jsr setAttack
	ldy patchSetY

	lda patchSR,y
	jsr setRelease
	ldy patchSetY

	lda patchFilt,y
	ldx filterDisable
	beq skipFilterDisable
	and #$F0
skipFilterDisable:
	sta SID1+SFILTC
	sta SID2+SFILTC
	sta sidData+SFILTC
	and #$01
	beq skipFilterOnText
	ldy #FILTERTEXT2
	ldx #4
	jsr updateText
skipFilterOnText
	ldy patchSetY

	lda patchMode,y
	and #$F0
	ora volume
	sta volModeRAM
	jsr setVolume
	ldy patchSetY

	lda patchOctave,y
	jsr setOctave
	ldy patchSetY

	jsr showPatchName

	rts
	;------------------------ end of setpatch

showPatchName:
	ldy patchSetY
	iny
	tya
	asl
	asl
	asl
	asl
	tay
	dey
	ldx #15
patchText:
	lda patchName,y
	cmp #64
	bmi pshowSpace
	sec
	sbc #64
pshowSpace:
	sta 1024+PATCHTEXT,x
	dey
	dex
	bpl patchText
	rts
	

HEX_DISP_OFFSET equ 4
	
	;===============================================
	; Allow the user to hex edit the SID registers
	; directly.
SIDEdit:

	sta hexKeyMode

	jsr beep
	jsr beep
	jsr beep

	jsr clrScr
	jsr displayInit

	; X = low data address
	; Y = high data address
	ldx #>hexEditHelp ;low/MSB
	ldy #<hexEditHelp ;high/LSB
	jsr displayPage

	jsr showSidValues

	;enable keyboard interrupt
	lda #129
	sta 56333
	; wait for all keys to be released first
waitForNoKey:
	lda 197
	cmp #64
	bne waitForNoKey

	; clear top line
	ldx #9
	lda #32
hexClearA
	sta 1024,x
	dex
	bpl hexClearA

	; display "sid edit" text
	ldx #92
	ldy #0
	jsr updateText

	; get/display the first hex digit of the address to edit
	lda #36
	sta 1024+HEX_DISP_OFFSET
	jsr getHexKey
	asl
	asl
	asl
	asl
	sta SIDeditAddr
	lda hexDisplay,x
	sta 1024+HEX_DISP_OFFSET

	; get/display the second hex digit of the address to edit
	lda #36
	sta 1025+HEX_DISP_OFFSET
	jsr getHexKey
	ora SIDeditAddr
	sta SIDeditAddr
	lda hexDisplay,x
	sta 1025+HEX_DISP_OFFSET
	
	; display a '>' between
	lda #62
	sta 1026+HEX_DISP_OFFSET

	; get/display the first hex digit of the value to write
	lda #36
	sta 1027+HEX_DISP_OFFSET
	jsr getHexKey
	asl
	asl
	asl
	asl
	sta SIDeditValue
	lda hexDisplay,x
	sta 1027+HEX_DISP_OFFSET

	; get/display the second hex digit of the value to write
	lda #36
	sta 1028+HEX_DISP_OFFSET
	jsr getHexKey
	ora SIDeditValue
	sta SIDeditValue
	lda hexDisplay,x
	sta 1028+HEX_DISP_OFFSET

	; if <=$20 then write to all 3 SID oscillator regs
	ldx SIDeditAddr
	cpx #$20
	bmi normalWrite
	;--------------
	lda SIDeditValue
	sta SID1-32,x
	sta SID2-32,x
	sta sidData-32,x
	sta SID1-32+#$7,x
	sta SID2-32+#$7,x
	sta sidData-32+#$7,x
	sta SID1-32+#$E,x
	sta SID2-32+#$E,x
	sta sidData-32+#$E,x
	cpx #SV1WAVE+32
	bne no3Wave
	sta WaveType
	sta WaveType2
	sta WaveType3
no3Wave:
	jmp skipNormalWrite
	;...............
normalWrite:
	; write the value to both sids
	ldx SIDeditAddr
	lda SIDeditValue
	sta SID1,x
	sta SID2,x
	sta sidData,x

	cpx #SV1WAVE
	bne noWave1
	sta WaveType
noWave1:

	cpx #SV2WAVE
	bne noWave2
	sta WaveType2
noWave2:

	cpx #SV3WAVE
	bne noWave3
	sta WaveType3
noWave3:

	cpx #SFILTH
	bne noFiltSave
	sta filter
noFiltSave:

	; turn off paddles if filter was adjusted
	cpx #SFILTL
	beq paddleOffhex
	cpx #SFILTH
	bne noPaddleOff
paddleOffhex:
	lda #0
	sta paddle ; turn off paddle controller first
	jsr showPaddle
noPaddleOff:

skipNormalWrite:
	ldx SIDeditAddr
	lda SIDeditValue

	; check for a volume/mode change...
	; if changed, write it to related variables too
	cpx #SVOLMODE
	bne noVolumeSetting
	sta temp
	and #$F0
	sta volModeRAM
	lda temp
	and #$0F
	sta volume
noVolumeSetting:
;	lda volModeRAM
;	and #$F0
;	ora volume

	; wait for key to be released before returning
waitKeyRelease:
	lda 197
	cmp #64
	bne waitKeyRelease

	; reset volume (messed up from clicks)
	jsr setVolume

	lda #0
	sta helpMode
	jsr displayInit
	; X = low data address
	; Y = high data address
;	ldx #>hexEditHelp ;low/MSB
;	ldy #<hexEditHelp ;high/LSB
;	jsr displayPage
;	jsr showSidValues

	rts

	;************************************
showSidValues:
	ldy #2
sidDispLoop1:
	sty sidTemp1
	lda sidData,y
	sta sidTemp2
	tya
	asl
	clc
	adc #4
	tay
	lda sidTemp2
	ldx #6
	jsr displayHex
	ldy sidTemp1
	;---------------
	lda sidData+7,y
	sta sidTemp2
	tya
	asl
	clc
	adc #4
	tay
	lda sidTemp2
	ldx #10
	jsr displayHex
	ldy sidTemp1
	;---------------
	lda sidData+14,y
	sta sidTemp2
	tya
	asl
	clc
	adc #4
	tay
	lda sidTemp2
	ldx #14
	jsr displayHex
	ldy sidTemp1

	iny
	cpy #7
	bne sidDispLoop1
	;---------------
	;---------------
	ldy #$15
sidDispLoop2:
	sty sidTemp1
	lda sidData,y
	sta sidTemp2
	tya
	sec
	sbc #2
	tay
	lda sidTemp2
	ldx #5
	jsr displayHex
	ldy sidTemp1

	iny
	cpy #$19
	bne sidDispLoop2
	;---------------
	rts



	;=======================================================================
	; waits for user to press a key (0-F) and returns
	; the value in X and A
getHexKey
	;enable keyboard interrupt
	lda #129
	sta 56333

	lda 197
	cmp lastKey
	beq getHexKey
restartHexKey:
	ldx #0
	lda hexKeyMode ; this variable determines which key set is used
	beq noAltKeyMode
	ldx #16 ; use alternate keys for piano keyboard
noAltKeyMode:
	lda 197
keyCmpLoop:
	;stx 1025
	cmp keyData,x
	beq foundKey
	inx
	cpx #16
	beq restartHexKey
	cpx #32
	beq restartHexKey
	jmp keyCmpLoop
foundKey:
	sta lastKey

	jsr beep
;	jsr beep

	lda hexKeyMode
	beq noKeySubtract
	txa
	sec
	sbc #16
	tax
noKeySubtract:

	txa
	

	rts

beep:
;	lda #$10
;	sta SID1+SV1FH
	lda volModeRAM
	ora #$0F
	sta SID1+SVOLMODE
	sta SID2+SVOLMODE
	sta sidData+SVOLMODE
;	lda SID1+SV1WAVE
;	ora #$01
;	sta SID1+SV1WAVE
	jsr clickDelay
	lda volModeRAM
	and #$F0
	sta SID1+SVOLMODE
	sta SID2+SVOLMODE
	sta sidData+SVOLMODE
;	lda SID1+SV1WAVE
;	and #$FE
;	sta SID1+SV1WAVE
	rts
	

	; ------------------------------------
	; delay for click -- uses Y
clickDelay:
	ldy #$40
	sty temp
mainDelayLoop:
	ldy #0
innerDelayLoop:
	dey
	bne innerDelayLoop
	dec temp
	bne mainDelayLoop
	rts	
	
	
hexDisplay
	byte 48,49,50,51,52, 53,54,55,56,57, 1,2,3,4,5, 6

keyData ; numbers 0-9 and letters a-f
	byte 35,56,59,8,11, 16,19,24,27,32, 10,28,20,18,14, 21

	 ; key set for piano keyboard: black keys + middle A-F on the white keys
	byte 51,59,8,16,19, 24,32,35,43,48, 25,30,33,38,41, 46

	; array of LFO values
LFOArrL
	byte 0,1,2,3,  2,1,0,1,     2,3,4,5,         4,3,2,1
LFOArrH
	byte 0,0,0,0, 0,0,0,255, 255,255,255,255, 255,255,255,255 ; more LFO depth
LFOdisp
	byte 6,7,8,9,  8,7,6,5, 4,3,2,1, 2,3,4,5

; NTSC Note Table
NSoundLArr:                                                               ;  |      80     |      C-5      |     8583    |      33     |     135     | 
	byte 24,56,90,125, 163,203,246,35, 83,134,187,244	; octave 2       ;  |      81     |      C#-5     |     9094    |      35     |     134     |
	byte 48,122,180,251, 71,151,237,71, 167,12,119,233	; octave 3       ;  |      82     |      D-5      |     9634    |      37     |     162     |
	byte 97,225,104,247, 143,47,218,142, 77,24,238,210	; octave 4       ;  |      83     |      D#-5     |    10207    |      39     |     223     |
	byte 195,195,209,239, 31,96,181,30, 156,49,223,165	; octave 5       ;  |      84     |      E-5      |    10814    |      42     |      62     |
	byte 135,134,162,223, 62,193,107,60, 57,99,190,75	; octave 6       ;  |      85     |      F-5      |    11457    |      44     |     193     |
	byte 15,12,69,191, 125,131,214,121, 115,199,124,151 ; octave 7
                                                                         ;  |      86     |      F#-5     |    12139    |      47     |     107     |
NSoundHArr:                                                               ;  |      87     |      G-5      |    12860    |      50     |      60     |
	byte 2,2,2,2, 2,2,2,3, 3,3,3,3				; octave 2               ;  |      88     |      G#-5     |    13625    |      53     |      57     |
	byte 4,4,4,4, 5,5,5,6, 6,7,7,7				; octave 3               ;  |      89     |      A-5      |    14435    |      56     |      99     |
	byte 8,8,9,9, 10,11,11,12, 13,14,14,15		; octave 4               ;  |      90     |      A#-5     |    15294    |      59     |     190     |
	byte 16,17,18,19, 21,22,23,25, 26,28,29,31	; octave 5               ;  |      91     |      B-5      |    16203    |      63     |      75     |
	byte 33,35,37,39, 42,44,47,50, 53,56,59,63	; octave 6               ;  |      96     |      C-6      |    17167    |      67     |      15     |
	byte 67,71,75,79, 84,89,94,100, 106,112,119,126 ;octave 7

; PAL Note Table
PSoundLArr:                                                            
	byte 45,78,113,150, 190,231,20,66, 116,169,224,27
	byte 90,156,226,45, 123,207,39,133, 232,81,193,55
	byte 180,56,196,89, 247,158,78,10, 208,162,129,109
	byte 103,112,137,178, 237,59,157,20, 160,69,3,219
	byte 207,225,18,101, 219,118,58,39, 65,138,5,181
	byte 157,193,36,201, 182,237,115,78, 130,20,10,106
	byte 59,130,72,147, 107,218,231,156, 4,40,20
                                                                     
PSoundHArr:                                                           
	byte 2,2,2,2, 2,2,3,3, 3,3,3,4
	byte 4,4,4,5, 5,5,6,6, 6,7,7,8
	byte 8,9,9,10, 10,11,12,13, 13,14,15,16
	byte 17,18,19,20, 21,23,24,26, 27,29,31,32
	byte 34,36,39,41, 43,46,49,52, 55,58,62,65
	byte 69,73,78,82, 87,92,98,104, 110,117,124,131
	byte 139,147,156,165, 175,185,196,208, 221,234,248

	;------------------------------------------
	; update text
	;------------------------------------------
	; Show text out of the textData array.
	; x=textData, y=screen position
updateText
	clc
	lda #4
	sta textTemp
updateTextLoop:
	lda textData,x
	cmp #64
	bmi showSpaceU
	sbc #64
showSpaceU
	sta 1024,y
	inx
	iny
	dec textTemp
	bne updateTextLoop
	rts


	; ***************************
	; Display Setup
	; ***************************
displayInit:

	; draw static text at the top of the screen
	ldx #>mainColorText ;low/MSB
	ldy #<mainColorText ;high/LSB
	jsr displayPage
	
	; Draw bottom text (version number+PAL/NTSC setting)
BOTTOMTEXT equ 40*24+29

	; choose which text to show from PAL/NTSC test at startup
	ldx #0
	ldy #0
	lda NTSCmode
	beq showPAL
	ldx #12
showPAL:

TextLoop2:
	lda bottomText,x
	beq endText2
	cmp #64
	bmi showSpace2
	sbc #64
showSpace2
	sta 1024+BOTTOMTEXT,y
	lda #11
	sta 55296+BOTTOMTEXT,y ; color non-static text
notBlank2:
	inx
	iny
	bne TextLoop2
endText2:

	;---------------------------------------------
	; display current values

	; set tuning text
	ldy tuneSetting 
	jsr ksetTune

	; Video Mode
	lda videoMode
	ldy videoText
	jsr setVideoMode ;********************************

	lda LFODepth
	jsr setLFODepth ;********************************
	lda LFORate
	jsr setLFORate ;********************************

	lda paddle
	jsr setPaddles
	
	lda filter
	jsr setFilter
	
	jsr setPort
	jsr setFifthsText
	
	lda attack
	jsr showAttack
	
	lda release
	jsr showRelease
	
	ldy filterStatus
	jsr showFiltOnOff
	
	jsr setVolume
	
	lda octave
	jsr setOctave

	jsr showPatchName
	
	lda paddle2
	jsr ksetPad2

	rts
	;---------------------------------------------------

	; ***************************
	; Variable Setup
	; ***************************
variableInit:
	lda #0
	sta paddle2
	sta filterDisable
	sta filterStatus
	sta keyTimer
	sta bender
	sta helpMode
	sta fifths
	sta patPtr
	sta Frame
	sta FrameH
	sta EchoCur
	sta customPatchSaved
	sta paddleTop
	sta paddleBottom

	lda #2
	sta lastPad2 ; default paddle2 setting is LFO Depth

	lda #4 		; set normal tuning
	sta tuneSetting 

	lda #0
	jsr setLFODepth ;********************************
	lda #0
	jsr setLFORate ;********************************

	; set up tuning array pointers
	lda #<tuningL4
	sta tunePtrL
	lda #>tuningL4
	sta tunePtrL+1

	lda #<tuningH4
	sta tunePtrH
	lda #>tuningH4
	sta tunePtrH+1

	lda #2
	sta VICMode
	lda #0
	sta portOn
	lda #40
	sta portSpd

	; Video Mode
	lda #3
	ldy #2
	jsr setVideoMode ;********************************

	; Default full volume
	lda #$0F
	sta volume

	; Echo?
	lda #$70
	sta EchoPtr

	ldy #0
	jsr setPatch ;set bass sound

	; Set up starting portamento values
	ldx #12
	lda NSoundLArr,x
	sta pitchLA
	sta pitchLB
	sta pitchLC
	sta lastKeyA
	sta lastKeyB
	sta lastKeyC
	lda NSoundHArr,x
	sta pitchHA
	sta pitchHB
	sta pitchHC
	;----------
	rts


	;************************************
	; clrScr - Clear Screen
	;************************************	
clrScr:
	ldx #0
	lda #32
clrScrLoop:
	sta $400,x
	sta $500,x
	sta $600,x
	sta $700,x
	dex
	bne clrScrLoop
	rts


	;************************************
	; DisplayHex - draw hex value on screen
	;************************************
	; note uses helpWritePointer so can't be
	; used within displayPage routine
	; X = horizontal location
	; Y = vertical location
	; A = value
displayHex
	; figure out screen location
	; and store in word pointer
	stx hexDispTemp
	tax ; save the hex value in X
	lda lineOffsetL,y
	clc
	adc hexDispTemp
	sta helpWritePointerL
	sta helpColorPointerL

	lda lineOffsetM,y
	adc #0
	sta helpWritePointerM
	clc
	adc #$D4
	sta helpColorPointerM
	;--------------------
	stx hexDispTemp ; save the hex value to be displayed
	txa
	and #$0F ; isolate the LS nibble
	tax
	lda hexDisplay,x ; get character to display
	ldy #1
	sta (helpWritePointerL),y
	lda #$E
	sta (helpColorPointerL),y
	;-------------------
	lda hexDispTemp
	lsr	; get the MS nibble
	lsr
	lsr
	lsr
	tax
	lda hexDisplay,x ; get character to display
	ldy #0
	sta (helpWritePointerL),y
	lda #$E
	sta (helpColorPointerL),y
	;-------------------
	rts
	

	;************************************
	; DisplayPage - display an entire
	; page of help info with color support
	;************************************
	; X = LSB of data address
	; Y = MSB of data address
	; data format:
	; line_number, color, text, 0
	; line_number, color, text, 0
	; 255
displayPage:
	sty helpReadPointerL
	stx helpReadPointerM
	lda #1
	sta helpColor ; default to white
helpLoop:
	ldy #0
	; get line number and set up output pointers
	lda (helpReadPointerL),y
	cmp #255
	beq quitHelp
	tax
	lda lineOffsetM,x
	sta helpWritePointerM
	clc
	adc #$D4
	sta helpColorPointerM
	lda lineOffsetL,x
	sta helpWritePointerL
	sta helpColorPointerL
	;--------------------
	ldy #1
	sty helpYIn
	ldy #0
	sty helpYOut
helpTextLoop:
	; get the first character
	ldy helpYIn
	lda (helpReadPointerL),y
	beq quitTextLoop
	iny 
	sty helpYIn
	; see if it's a color command
	cmp #128
	bpl setColor ;---
	cmp #64
	bmi showSpaceHelp
	sbc #64
showSpaceHelp:
	; write the color and character to the screen
	ldy helpYOut
	sta (helpWritePointerL),y
	lda helpColor
	sta (helpColorPointerL),y
	iny
	sty helpYOut
	jmp helpTextLoop
setColor:
	sec
	sbc #128
	sta helpColor
	jmp helpTextLoop
	;---------------
quitTextLoop:
	; update the input pointer
	iny
	tya
	clc
	adc helpReadPointerL
	sta helpReadPointerL
	lda helpReadPointerM
	adc #0
	sta helpReadPointerM
	jmp helpLoop ;/\/\/\/\/\
quitHelp:
	rts
	;---------------------------- displayPage
	
	; This chart references each line
	; on the standard character based screen.
	; = value x 40 + 0x400
lineOffsetL
	byte $00
	byte $28,$50,$78, $A0,$C8,$F0
	byte $18,$40,$68, $90,$B8,$E0
	byte $08,$30,$58, $80,$A8,$D0
	byte $F8,$20,$48, $70,$98,$C0
	byte $E8
lineOffsetM
	byte $04
	byte $04,$04,$04, $04,$04,$04
	byte $05,$05,$05, $05,$05,$05
	byte $06,$06,$06, $06,$06,$06
	byte $06,$07,$07, $07,$07,$07
	byte $07

hexEditHelp:
	byte  6,$82,"ALL OS1 OS2 OS3  7  6  5  4  3  2  1  0 ",0
	byte  7,$81,"$22 $02 $09 $10",$87,"  +---PULSE-WIDTH-LOW---+",0
	byte  8,$8E," VAL $   $   $",0
	byte  9,$81,"$23 $03 $0A $11",$87,"  .  .  .  .  +PULS-HIGH+",0
	byte 10,$8E," VAL $   $   $",0
	byte 11,$81,"$24 $04 $0B $12",$87,"  NO SQ SA TR TE RI SY GT",0
	byte 12,$8E," VAL $   $   $",0
	byte 13,$81,"$25 $05 $0C $13",$87,"  +-ATTACK--+ +--DECAY--+",0
	byte 14,$8E," VAL $   $   $",0
 	byte 15,$81,"$26 $06 $0D $14",$87,"  +-SUSTAIN-+ +-RELEASE-+",0
	byte 16,$8E," VAL $   $   $",0
 	byte 17,$82,"----------------------------------------",0
	byte 18,$82,"ALL VAL  7  6  5  4  3  2  1  0 ",0
 	byte 19,$81,"$15 ",$8E,"$    ",$87,".  .  .  .  . +FILT-LO+",0
 	byte 20,$81,"$16 ",$8E,"$    ",$87,"+-FILTER-CUTOFF-HIGH--+",0
 	byte 21,$81,"$17 ",$8E,"$    ",$87,"+FILT-RES-+ FX F1 F2 F3",0
 	byte 22,$81,"$18 ",$8E,"$    ",$87,"XX HP BP LP +-VOLUME--+",0
 	byte 255

normalHelp
 	byte 5,$82,"----------------------------------------",0
	byte 6,$8d,"ASD",$8F,"=RELEASE  ",$8d,":;=",$8F,"=VOLUME",0
	byte 7,$8d,"ZXCVBNMM,.",$8F,"=PATCH-SELECTION  ",$8d,"/",$8F,"=MUTE",0
	byte 8,$8d,"SPACE",$8F,"=BENDER",$8d,"  CURSOR-KEYS",$8F,"=FIFTHS",0
	byte 9,$8d,"FUNCTION-KEYS",$8F,"=OCTAVE",$8d,"  CTRL",$8F,"=CUSTOM-PATCH",0
 	byte 10,$82,"---------------------------------------",0
	byte 11,$81,"PRESS SHIFT +",0
	byte 12,$8A,"ASD",$8F,"=ATTACK  " ,$8A,"FGH",$8F,"=VID-STYLE" ,$8A,"  JKL",$8F,"=VID-MODE"
	byte 0
	byte 13,$8A,"ZXCV",$8F,"=LFO-RATE " ,$8A,"BNM,",$8F,"=LFO-DEPTH" ,$8A," ;:",$8F,"=VID-SIZE"
	byte 0
	byte 14,$8A,"CURSOR-KEYS",$8F,"=PADDLE1-ON/OFF",0
	byte 15, $8A,"FUNCTION-KEYS",$8F,"=PORTAMENTO",0
 	byte 16,$82,"---------------------------------------",0
	byte 17,$81,"PRESS COMMODORE-KEY +",0
	byte 18,$8E,"ASDFGHJK",$8F,"=FIXED-FILTER-CUTOFF"
	byte 0
	byte 19,$8E,"ZXC",$8F,"=FILT-ON/OFF/DISABLE",$8E," VBNM",$8F,"=PADDLE2"
	byte 0
 	byte 20,$82,"---------------------------------------",0
	byte 21,$81,"PRESS RUN-STOP +",0
	byte 22,$87,"ASDFGHJKL:;",$8F,"=TUNING" ,$87,"  ZX",$8F,"=PAL/NTSC"
	byte 0
	byte 23,$87,"F1",$8F,"=SID-EDIT-KB" ,$87,"  F3",$8F,"=SAVE-CUSTOM-PATCH"
	byte 0
	byte 24,$87,"F7",$8F,"=SID-EDIT-PIANO"
	byte 0
	byte 255

mainColorText
	byte 0,$82,"CYNTHCART  ",$81,"PATCH",$8F,"=                       ",0
	byte 1,$87,"           LFO:     RATE",$8F,"=     ",$87,"DEPTH",$8F,"=    ",0
	byte 2,$81,"FIFTHS",$8F,"=    ",$83,"ATTACK",$8F,"=  ",$83,"RELEASE",$8F,"=  ",$8A,"PORT",$8F,"=     ",0
	byte 3,$8D,"PAD1",$8F,"=      ",$8D,"CUTOFF",$8F,"=  ",$8D,"FILTR",$8F,"=    ",$81,"PITCH",$8F,"=    ",0
	byte 4,$81,"PAD2",$8F,"=      ",$81,"OCTAVE",$8F,"=  ",$81,"VIDEO",$8F,"=    ",$81,"VOL",$8F,"=      ",0
	byte 255


; contant pointers into the textData array
ON equ 0
OFF equ 4
SLOW equ 8
MED equ 12
FAST equ 16
TUNING equ 20
POLY equ 60
MONO equ 64
FREQ equ 68
FILT equ 72
VOL equ 76
VLOW equ 80
VMED equ 84
VHIGH equ 88
DISABLED equ 96
PAD2VALTEXT equ 100

textData ; can contain 64 four byte texts
	byte "OFF " ;0
	byte "ON  " ;4
	byte "SLOW" ;8
	byte "MED " ;12
	byte "FAST" ;16
	byte "-40 " ;20 tuning
	byte "-30 " ;24
	byte "-20 " ;28
	byte "-10 " ;32
	byte "0   " ;36
	byte "+10 " ;40
	byte "+20 " ;44
	byte "+30 " ;48
	byte "+40 " ;52
	byte "+50 " ;56
	byte "POLY" ;60 mode
	byte "MONO" ;64
	byte "FREQ" ;68 LFO dest
	byte "FILT" ;72
	byte "VOL " ;76
	byte "LOW " ;80
	byte "MED " ;84
	byte "HIGH" ;88
	byte "SID:" ;92
	byte "DIS " ;96
	byte "OFF " ;100 - paddle2 settings
	byte "PULS" ;104
	byte "XXXX" ;108 - this one for future...
	byte "LFO " ;112
	byte "BEND" ;116

bottomText
	byte " PAL V1.2.4",0
	byte "NTSC V1.2.4",0

; also: mode (poly/mono)

PATCHTEXT equ 17
SYNCTEXT equ 136
PADDLETEXT equ 125
PAD2TEXT equ 165
PORTTEXT equ 115
FIFTHSTEXT equ 87
VOLTEXT equ 194
RELTEXT equ 108
ATKTEXT equ 98
LFORATETEXT equ 65
LFODEPTHTEXT equ 76
TUNINGTEXT equ 156
VIDEOTEXT equ 186
FILTERTEXT equ 138
OCTAVETEXT equ 178
FILTERTEXT2 equ 146

tuneArrPtrLL
	byte <tuningL0, <tuningL1, <tuningL2, <tuningL3, <tuningL4, <tuningL5, <tuningL6, <tuningL7, <tuningL8, <tuningL9
tuneArrPtrLH
	byte >tuningL0, >tuningL1, >tuningL2, >tuningL3, >tuningL4, >tuningL5, >tuningL6, >tuningL7, >tuningL8, >tuningL9
tuneArrPtrHL
	byte <tuningH0, <tuningH1, <tuningH2, <tuningH3, <tuningH4, <tuningH5, <tuningH6, <tuningH7, <tuningH8, <tuningH9
tuneArrPtrHH
	byte >tuningH0, >tuningH1, >tuningH2, >tuningH3, >tuningH4, >tuningH5, >tuningH6, >tuningH7, >tuningH8, >tuningH9

LFODepthArray
	byte 0,2,5,15

	include "tuning.asm"

	IF MODE=KERNEL
	org $bfff
	byte 0
	ENDIF
