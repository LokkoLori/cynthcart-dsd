.var input_lo = $DC00
.var input_up = $DC01


.macro handlestring(keyrows, keycols, tunning, sidchannel)
{
	//init readloop
	lda #$ff
	sta handlefret
	ldy #0
readloop:
	lda keycols,y
	beq endread
	sta input_lo
	lda input_up
	and keyrows,y
	bne notPressed
	tya
	clc
	sta handlefret
	jmp endread
notPressed:
	iny
	jmp readloop
	
endread:

	lda actfret
	sta prevfret
	lda handlefret
	sta actfret
	jmp end
	
//variables storage and constructor
actfret:
	.byte 0
prevfret:
	.byte 0
handlefret:
	.byte 0
soundtime:
	.byte 0, 0
end:
	
	ldx prevfret
	lda #32
	sta 1024+tunning,x
	ldx actfret
	lda #160
	sta 1024+tunning,x
	
	nop
}
 