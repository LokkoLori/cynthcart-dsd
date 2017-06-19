.var input_lo = $DC00
.var input_up = $DC01
.var sid = $D400

.var distorsionwf = %00100000
.var cleanwf = %00100000

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

.macro vibratechannel(string, ch, cntr)
{
	lda cntr
	and #%00011111
	sta free
	lda #0
	sta free+1
	:Add(string+4, free, sid+ch, 2)
}

.macro handlestring(keyrows, keycols, keylen, sidc, data, visoff, qvint)
{
	.var basetune = data
	.var waveform = data + 1
	.var actfret = data + 2
	.var prevfret = data + 3
	.var freklow = data + 4
	.var frekhigh = data + 5
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
	beq readgot
	jmp handlejoy
	
readgot:
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
	.if (qvint!=0)
	{
		lda #0
		cmp qvint_mode
		bne qvint_mute
		jmp qvint_mute_end
qvint_mute:
		sta sid+qvint+4
qvint_mute_end:
	}
	inc 2002+sidc
	lda actfret 
	sta 2003+sidc
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
	and #$F8
	cmp #0
	bne fingerpick
	jmp sound
fingerpick:
	//rollback easepos
	lda #$08
	sta easepos
	:setvolumebyeasepos()
	jmp sound
	
	
//private variables storage and constructor
gotfret:
	.byte $ff
joyhold:
	.byte 0

handlejoy:
	lda #0
	cmp joyhold
	beq pick
	jmp end
	
pick:
	lda #1
	sta joyhold
	lda actfret
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
	sta data + 4
	lda FreqTablePalHi,x
	sta sidch+1
	sta data + 5
	
	lda waveform
	ora #$01
	sta 2000
	sta sidch+4
	
	.if (qvint!=0)
	{
		lda #0
		cmp qvint_mode
		bne qvint_sound
		jmp qvint_sound_end
qvint_sound:
		txa
		clc
		adc #7
		tax
		lda FreqTablePalLo,x
		sta sid+qvint
		lda FreqTablePalHi,x
		sta sid+qvint+1
		lda waveform
		ora #$01
		sta 2000
		sta sid+qvint+4
qvint_sound_end:
	}
	
	inc 1401
	
end:
	lda joyhold
	sta 1400

}
 