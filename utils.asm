.var SCR_BORDER_COLOR = $d020
.var SCR_BACK_COLOR = $d021
.var KEY_PTR = $c3c2



.macro writeAddress(location,label)
{
	lda #<label
	sta location
	lda #>label
	sta location+1
}

.macro Add(OP1,OP2,RET,size)
{
	clc
	.for (var i=0;i<size;i++)
	{
		lda OP1+i
		adc OP2+i
		sta RET+i 
	}
}

.macro Sub(OP1,OP2,RET,size)
{
	sec
	.for (var i=0;i<size;i++)
	{
		lda OP1+i
		sbc OP2+i
		sta RET+i 
	}
}