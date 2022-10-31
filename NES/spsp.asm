.segment "HEADER"       ; HEADER SEGMENT -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
    .byte "NES"             ; System Definition Byte
    .byte $1a               ; System Definition Byte
    .byte $02               ; 2 * 16KB PRG ROM
    .byte $01               ; 1 * 8KB CHR ROM
    .byte %00000001         ; Mapper
    .byte $00, $00, $00     ; Filler Bytes
    .byte $00, $00, $00     ; Filler Bytes
    .byte $00, $00, $00     ; Filler Bytes

.segment "ZEROPAGE"     ; ZEROPAGE SEGMENT -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_

; QUICK MEMORY REFERENCE:
; $0200 - $02FF     =   Sprite Data (64 Total | Y, Index, Attribute, X)
; $0020             =   Controller 1 Input
; $0021             =   Controller 2 Input

.segment "STARTUP"      ; STARTUP SEGMENT -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
    Reset:                  ; Reset Vector Definition [Program Entry Point] -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
        ; Reset Start
        SEI                 ; Disable Interrupts During Initialization
        CLD                 ; Clear Decimal Mode, Since It Is Not Supported By The NES

        ; Disable The Sound Interrupt Request (IRQ)
        LDX #$40            ; Load The Binary Value 0100 0000 Into The X Register
        STX $4017           ; Disable Frame Interupt

        ; Initialize The Stack Register
        LDX #$FF            ; Load The Binary Value 1111 1111 Into The X Register
        TXS                 ; Store X Into The Stack Register

        ; Clear System Registers
        INX                 ; Increment X (FF + 01 = 00)
        STX $2000           ; Clear The PPU Controller Register
        STX $2001           ; Clear The PPU Mask Register
        STX $4010           ; Clear The APU DMC Register

        JSR VBlank          ; Wait For V-Blank

    CLEARMEM:           ; Clear System Working Memory
        LDA #$00            ; Load 0 Into The A Register
        STA $0000, X        ; Store A In Register ($0000 - $00FF = 0)
        STA $0100, X        ; Store A In Register ($0100 - $01FF = 0)
        STA $0300, X        ; Store A In Register ($0300 - $03FF = 0)
        STA $0400, X        ; Store A In Register ($0400 - $04FF = 0)
        STA $0500, X        ; Store A In Register ($0500 - $05FF = 0)
        STA $0600, X        ; Store A In Register ($0600 - $06FF = 0)
        STA $0700, X        ; Store A In Register ($0700 - $07FF = 0)
        LDA #$FF            ; Load The Binary Value 1111 1111 Into The A Register
        STA $0200, X        ; Store A In Sprite Register ($0200 - $02FF = FF)
        INX                 ; Increment X
        BNE CLEARMEM        ; Branch If The Above Operation Is Not Zero

        JSR VBlank          ; Wait For V-Blank

        ; Load Palette Data
        LDA $2002           ; Reset PPU Latch
        LDA #$3F            ; Load The Binary Value 0011 1111 Into The A Register
        STA $2006           ; Set The Upper Byte For The V-RAM Address To A
        LDA #$00            ; Load 0 Into The A Register
        STA $2006           ; Set The Lower Byte For The V-RAM Address To A
        LDX #$00            ; Load 0 Into The X Register
        
    LoadPalettes:       ; Load Palette Data
        LDA PaletteData, X  ; Load Into A The Palette Data Indexed At X
        STA $2007           ; Store A Into The PPU's V-RAM ($3F00 - $3F1F)
        INX                 ; Increment X
        CPX #$20            ; Compare X To Binary Value 0010 0000
        BNE LoadPalettes    ; Iterate Over The Palette Data If We Have Not Reached 0010 0000  
        
        JSR VBlank          ; Wait For V-Blank
        
        ; Load Background Data
        LDA	$2002           ; Reset PPU Latch
        LDA	#$20            ; Load The Binary Value 0010 0000 Into The A Register
        STA	$2006           ; Write The High Byte Of The Nametable Address
        LDA	#$00            ; Load 0 Into The A Register
        STA	$2006           ; Write The Low Byte Of The Nametable Address
        LDX #$00
        
    DrawBG:
        LDA	GameBG, X       ; Load Into A The Background Data Indexed At X
        STA	$2007		    ; Write To The PPU
        INX                 ; Increment X
        CPX #$40            ; Compare X To 40 Hexidecimal
        BNE	DrawBG		    ; Branch If The Above Operation Is Not 0
        
        ; Load Attribute Data
        LDA	$2002           ; Reset PPU Latch
        LDA	#$23            ; Load The Binary Value 0010 0011 Into The A Register
        STA	$2006           ; Write The High Byte Of The Attribute Address
        LDA	#$C0            ; Load The Binary Value 1100 0000 Into The A Register
        STA	$2006           ; Write The High Byte Of The Attribute Address
        LDX	#$00            ; Load 0 Into The X Register
        
    LoadAttribute:
        LDA	GameAtt, X      ; Load Into A The Attribute Data Indexed At X
        STA	$2007		    ; Write To The PPU
        INX			        ; Increment X
        CPX	#$08		    ; Compare X To 08 Hexidecimal
        BNE	LoadAttribute   ; Branch If The Above Operation Is Not 0
    
        ; Reset End
        LDA #%10010000          ; Load The Binary Value 1001 0000 Into The A Register
        STA $2000               ; Set NMI On V-Blank Start And Background Pattern Table Address To $1000
        LDA #%00011110          ; Load The Binary Value 0001 1110 Into The A Register
        STA $2001               ; Set Show Sprites, Background, And The Leftmost 8 Pixels Of The Screen
        CLI                     ; Enable Interrupts
    
    
    
    GMLP:               ; The Main Game Loop -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
        JSR ReadInput           ; Read Player Input
    JMP GMLP            ; Continue The Game Loop
    
    
    
    ReadInput:          ; Read In Data From The System's Controllers -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
        ; Read Setup
        LDA #$01                ; Load The Binary Value 0000 0001 Into The A Register
        STA $0020               ; Store The Value Of A In Register $0020
        STA $0021               ; Store The Value Of A In Register $0021
        
        ; Send The Latch Pulse To The Controller Hardware
        STA $4016               ; Set The Latch Pulse To High For Controller 1
        LDA #$00                ; Load 0 Into The A Register
        STA $4016               ; Clear The Latch Pulse To Controller 1
        
        ; Controller 1
        ReadLoop1:
            LDA $4016               ; Load The Value Of Register $4016 Into A
            LSR A                   ; Logical Shift Right For Register A
            ROL $0020               ; Rotate Register $0020 Left
            BCC ReadLoop1           ; If The Carry Flag Is Low, Repeat The Read Loop
            
    RTS                     ; Return From Subroutine
    
    
    
    VBlank:                 ; Wait For A V-Blank -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
        BIT $2002               ; Perform A Bit Test On The PPU Status Register
        BPL VBlank              ; Repeat If Not Positive
    RTS                     ; Return From Subroutine
    
    
    
    NMI:                    ; Non Maskable Interupt Vector Definition -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
        ; Load Graphical Data
        LDA #$00                ; Load 0 Into The A Register
        STA $2003               ; Set The Low Byte For Sprite Data
        LDA #$02                ; Store The Most Significant Bit For The Range Of Sprite Data ($0200 - $02FF)
        STA $4014               ; Load The Sprite Data In The Range Defined By A (256 Values)

    RTI                     ; Return From Interupt
    
    ; Variables
    PaletteData:
        ; Background Palettes
        .byte 	$27,$17,$07,$0F
        .byte	$27,$17,$07,$0F
        .byte	$27,$17,$07,$0F
        .byte	$27,$17,$07,$0F
        ; Sprite Palettes
        .byte	$00,$2D,$3D,$0F
        .byte	$00,$2D,$3D,$0F
        .byte	$00,$2D,$3D,$0F
        .byte	$00,$2D,$3D,$0F
        
    GameBG:
        ; Row 1
        .byte $00,$00,$01,$02,$02,$02,$02,$02,$02,$01,$00,$00,$00,$00,$00,$00
        .byte $00,$00,$00,$00,$00,$00,$01,$02,$02,$02,$02,$02,$02,$01,$00,$00

    GameAtt:
        ; Row 1 & 2
        .byte	%00000000, %00000000, %00000000, %00000000
        .byte	%00000000, %00000000, %00000000, %00000000
  
.segment "VECTORS"      ; VECTORS SEGMENT -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
    .word NMI               ; Non Maskable Interupt Vector
    .word Reset             ; Reset & Startup Vector
    
.segment "CHARS"        ; CHARS SEGMENT -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
    .incbin "ssg.chr"       ; Character Binary File