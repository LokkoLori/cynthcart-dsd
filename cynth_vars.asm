; *********************************************
; Constants
; *********************************************

KEYTIME equ 128

REL_SHORT equ $E0
REL_MED	equ $E9
REL_LONG equ $EE

ATK_SHORT equ $00
ATK_MED	equ $90
ATK_LONG equ $E0


VOLLOW equ $07
VOLMED equ $0B
VOLHIGH equ $0F

CURRENTKEY equ 197

BACK_COLOR equ 53280
BORD_COLOR equ 53281

SID1 equ $D400
SID2 equ $DE00

SID2OFFSET equ 4

SID_SYMPHONY_FILTER_OFFSET equ 10
	
SV1FL 		equ $00
SV1FH 		equ $01
SV1PWL 		equ $02
SV1PWH 		equ $03
SV1WAVE		equ $04
SV1AD 		equ $05
SV1SR 		equ $06

SV2FL 		equ $07
SV2FH 		equ $08
SV2PWL 		equ $09
SV2PWH 		equ $0A
SV2WAVE 	equ $0B
SV2AD 		equ $0C
SV2SR 		equ $0D

SV3FL 		equ $0E
SV3FH 		equ $0F
SV3PWL 		equ $10
SV3PWH 		equ $11
SV3WAVE 	equ $12
SV3AD 		equ $13
SV3SR 		equ $14
	
SFILTL		equ $15
SFILTH		equ $16
SFILTC		equ $17
SVOLMODE	equ $18

SPAD1		equ $19
SPAD2		equ $1A

SRAND 		equ $1B

PortA		equ $dc00
Ciddra		equ $dc02


; *********************************************
; RAM Variables
; *********************************************

; zero page
tunePtrL	equ 34 ;2 bytes 
copyPtrS	equ 34 ; (also used for RAM copying)
tunePtrH	equ 36 ;2 bytes (also used for RAM copying)
copyPtrD	equ 36 ; (also used for RAM copying)

lowTextPtr equ 43 ; 2 BYTES
lowColorPtr equ 45 ; 2 BYTES

helpReadPointerL equ 47
helpReadPointerM equ 48
helpWritePointerL equ 49
helpWritePointerM equ 50
helpColorPointerL equ 51
helpColorPointerM equ 52
 
portPtrL	equ 53 ;2 bytes
portPtrH	equ 71 ;2 bytes

keyPtrL equ 194 ; used to set up key command function calls
keyPtrH equ 195



;-------------------------------------

Trem		equ $7000

FrameH		equ $7002
Frame		equ $7003

EchoPtr		equ $7004
EchoCur		equ $7005

Buffer  	equ $7006 ; for paddle reading routine

MagicVoice	equ $7007

LastKey		equ $7008

Button		equ $7009

LFO			equ $700A

PatchTune	equ $700B

temp		equ $700C

KeyA	equ $700D
KeyB	equ $700E
KeyC	equ $700F

lastKeyA equ $7010
lastKeyB equ $7011
lastKeyC equ $7012

portDirA equ $7013
portDirB equ $7014
portDirC equ $7015

pitchLA	equ $7016
pitchLB	equ $7017
pitchLC	equ $7018

pitchHA	equ $7019
pitchHB	equ $701A
pitchHC	equ $701B

volume	equ $701C

portOn	equ $701D
portSpd	equ $701E

dispOn	equ $701F

VICMode	equ $7020

patPtr	equ $7021

keyOffset	equ $7022

fifths equ $7023

volModeRAM equ $7024

shiftL1 equ $7025
shiftH1 equ $7026
shiftL2 equ $7027
shiftH2 equ $7028

;$7025 - $7028 free

paddle	equ $7029 ; determines whether paddle controls filter or not

WaveType	equ $7030
WaveType2	equ $7037
WaveType3	equ $703E

LFObend		equ $703F
bender		equ $7040

pitchTmpL	equ $7043
pitchTmpH	equ $7044

videoMode	equ $7045

textTemp	equ $7046

patchSetY	equ $7047

LFORate		equ $7048
LFODepth	equ $7049

videoModeNum equ $7050

portLastNote equ $7051 ;-$7053   3 bytes
portLastDir	 equ $7054 ;-$7056	 3 bytes

filter	equ $7057

keyTimer equ $7058

bendSpd equ $7059

SIDeditAddr equ $705A
SIDeditValue equ $705B

filterDisable equ $705C

lastKey equ $705D

hexKeyMode equ $7060

NTSCmode equ $7061

temp16L equ $7062
temp16H equ $7063

saveX equ $7064

;EchoBuffer	equ 1184 ; 256 bytes

keyTemp equ $7065

lastOsc equ $7066

fullScreenMode equ $7067

videoText equ $7068

attack equ $7069
release equ $706A
octave equ $706B
filterStatus equ $706C

helpColor equ $706D
helpYIn equ $706E
helpYOut equ $706F

helpMode equ $7070

tuneSetting equ $7071

hexDispTemp equ $7072

sidTemp1 equ $7073
sidTemp2 equ $7074

customPatchSaved equ $7075

paddleTop equ $7076
paddleBottom equ $7077

paddleX equ $7078
paddleY equ $7079

paddle2 equ $707A
lastPad2 equ $707B

;-----------------------------------------------
sidData equ $7100 ; 32 bytes -- location to save SID register writes since it is read only

sidSaveData equ $7120 ; 32 bytes -- location to save edited SID patch
saveFifths equ $7140
savePaddle equ $7141
saveOctave equ $7142
savePortOn equ $7143
savePortSpeed equ $7144
saveLFODepth equ $7146
saveLFORate equ $7147
saveVolume equ $7148
saveVolMode equ $7149
saveFilter equ $714A

