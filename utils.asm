.var SCR_BORDER_COLOR = $d020
.var SCR_BACK_COLOR = $d021	

.macro writeAddress(location,label)
{
	lda #<label
	sta location
	lda #>label
	sta location+1
}