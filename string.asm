.var inputbyte_lower = $DC00
.var inputbyte_upper = $DC01


.macro handlestring(keyrows, keycols, tunning, sidchannel)
{
	.var actbund = 0
	.var prevbund = 1
	.var soundtime = 2
	
readloop:
	ldy #0
	lda keycols,y
	beq endread
	sta inputbyte_lower
	lda inputbyte_upper
	and keyrows,y
	bne notPressed
	tya
	clc
	adc keyOffset
	bmi quitCheck
notPressed:
	iny
	jmp readloop
endread:
	jmp end
	

data:
	.byte 0, 0, 0, 0
	.macro ldval(offs){
		lda data+offs
	}
	.macro stval(offs){
		sta data+offs
	}
end:
	nop
}