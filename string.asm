.var input_lo = $DC00
.var input_up = $DC01
.var sid = $D400

.macro invertfret(fret, keylen)
{
	lda #0
	clc
	sbc fret
	sta free
	lda #keylen
	clc
	adc free
	tax
}

.macro handlestring(keyrows, keycols, keylen, sidc, tunning, visoff)
{
	//init readloop
	lda #$ff
	sta gotfret
	ldy #0
	
	cmp joystate
	beq readloop
	jmp handlejoy
	
readloop:
	lda keycols,y
	beq endread
	sta input_lo
	lda input_up
	and keyrows,y
	bne notPressed
	tya
	clc
	sta gotfret
	jmp endread
notPressed:
	iny
	jmp readloop
	
endread:

	//catch joy noise
	lda #0
	sta $dc02
	lda $dc00
	sta joystate
	lda #$ff
	sta $dc02
	cmp joystate
	bne handlejoy
	
	//joy not holded
	lda #0
	sta joyhold
	
	lda gotfret
	cmp actfret
	bne fretchange
	jmp end
	
fretchange:
	
    lda actfret
	sta prevfret
	lda gotfret
	sta actfret
	
	:invertfret(prevfret, keylen)
	lda #32
	sta 1024+visoff,x
	
	lda actfret
	cmp #$FF
	bne touch
	
release:
	lda #$f0
	sta sid+sidc+6
	jmp end
	
touch:	
	:invertfret(actfret, keylen)
	lda #160
	sta 1024+visoff,x

	lda prevfret
	cmp #$FF
	bne hammer

land:
	jmp end
	
hammer:
	jmp sound
	
	
//variables storage and constructor
actfret:
	.byte $ff
prevfret:
	.byte $ff
gotfret:
	.byte $ff
soundtime:
	.byte 0, 0
joyhold:
	.byte 0

handlejoy:
	lda #0
	cmp joyhold
	beq pick
	jmp end
	
pick:
	lda actfret
	sta joyhold
	cmp #$FF
	bne sound
	jmp end
	
sound:
	:invertfret(actfret, keylen)
	lda #161
	sta 1024+visoff,x
	
	txa
	clc
	adc #tunning
	tax
	
	lda FreqTablePalLo,x
	sta sid+sidc
	lda FreqTablePalHi,x
	sta sid+sidc+1
	
	lda #$13
	sta sid+sidc+5
	
	lda #255
	sta sid+sidc+6
	
	lda #$21
	sta sid+sidc+4
	
	lda #$20
	sta sid+sidc+4
	
	inc 1401
	
end:
	lda joyhold
	sta 1400
	
	nop

}
 