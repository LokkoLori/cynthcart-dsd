.var input_lo = $DC00
.var input_up = $DC01

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

.macro handlestring(keyrows, keycols, keylen, tunning, sidchannel)
{
	//init readloop
	lda #$ff
	sta gotfret
	ldy #0
	
	//jump to pick if joysick
	cmp joystate
	bne skip
	jmp readloop
skip:
	jmp pick
	
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

	//catch joy trash
	lda #0
	sta $dc02
	lda $dc00
	sta joystate
	lda #$ff
	sta $dc02
	cmp joystate
	bne pick
	
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
	sta 1024+tunning,x
	
	lda actfret
	cmp #$FF
	bne touch
	
release:
	jmp end
	
touch:	
	:invertfret(actfret, keylen)
	lda #160
	sta 1024+tunning,x

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
free:
	.byte 0

pick:
	lda actfret
	cmp #$FF
	bne sound
	jmp end
	
sound:
	:invertfret(actfret, keylen)
	lda #161
	sta 1024+tunning,x
	
end:
	nop

}
 