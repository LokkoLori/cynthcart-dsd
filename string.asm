.var input_lo = $DC00
.var input_up = $DC01
.var sid = $D400

.var distorsionwf = %00100000
.var cleanwf = %01000000

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

.macro initsidchannel(sidc)
{	
	lda #$01
	sta sid+sidc+5
	lda #$F0
	sta sid+sidc+6
	lda #$FF
	sta sid+sidc+2
	lda #%00000011
	sta sid+sidc+3
}

.macro vibratechannel(sidc, low, high)
{
	lda low
	sta sid+sidc+2
	lda high
	sta sid+sidc+3
}

.macro handlestring(keyrows, keycols, keylen, sidc, data, visoff)
{
	.var basetune = data
	.var waveform = data + 1
	.var sidch = sid + sidc
	
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
	jsr readjoystate
	lda #$ff
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
	lda #$00
	sta sidch+4
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
	lda easepos
	and #$F0
	cmp #0
	bne fingerpick
	jmp sound
fingerpick:
	//rollback easepos
	lda #$0F
	sta easepos
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
	//fret num in x register
	lda #161
	sta 1024+visoff,x
	
	txa
	clc
	adc basetune
	tax
	
	lda FreqTablePalLo,x
	sta sidch
	lda FreqTablePalHi,x
	sta sidch+1
	
	lda waveform
	ora #$01
	sta 2000
	sta sidch+4
	
	inc 1401
	
end:
	lda joyhold
	sta 1400
	
	nop

}
 