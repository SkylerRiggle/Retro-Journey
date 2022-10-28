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

.segment "STARTUP"      ; STARTUP SEGMENT -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
    Reset:                  ; Reset Vector Definition [Program Entry Point]
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
        
        ; Wait For V-Blank
    :                   ; Anonymous Label
        BIT $2002           ; Perform A Bit Test On The PPU Status Register
        BPL :-              ; Branch Back To The Anonymous Label ":" If The Above Operation Is Positive
        
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
        
        ; Wait For V-Blank
    :                   ; Anonymous Label
        BIT $2002           ; Perform A Bit Test On The PPU Status Register
        BPL :-              ; Branch Back To The Anonymous Label ":" If The Above Operation Is Positive
        
        ; Load Graphical Data
        LDA #$02            ; Store The Most Significant Bit For The Range Of Sprite Data ($0200 - $02FF)
        STA $4014           ; Load The Sprite Data In The Range Defined By A (256 Values)
        NOP                 ; Perform No Operation To Give The PPU Time To Read
        
        ; Set The PPU Virtual RAM Address
        LDA #$3F            ; Load The Binary Value 0011 1111 Into The A Register
        STA $2006           ; Set The Upper Byte For The V-RAM Address To A
        LDA #$00            ; Load 0 Into The A Register
        STA $2006           ; Set The Lower Byte For The V-RAM Address To A
        
        ; Load Palette Data
        LDX #$00            ; Load 0 Into The X Register
    LoadPalettes:       ; Load Palette Data
        LDA PaletteData, X  ; Load Into A The Palette Data Indexed At X
        STA $2007           ; Store A Into The PPU's V-RAM ($3F00 - $3F1F)
        INX                 ; Increment X
        CPX #$20            ; Compare X To Binary Value 0010 0000
        BNE LoadPalettes    ; Iterate Over The Palette Data If We Have Not Reached 0010 0000  
        
        ; Reset End
        CLI                     ; Enable Interrupts
        LDA #%10010000          ; Load The Binary Value 1001 0000 Into The A Register
        STA $2000               ; Set NMI On V-Blank Start And Background Pattern Table Address To $1000
        LDA #%00011110          ; Load The Binary Value 0001 1110 Into The A Register
        STA $2001               ; Set Show Sprites, Background, And The Leftmost 8 Pixels Of The Screen
        
        JSR LoadTitle           ; Load The Title Screen Data
    Loop:               ; The Main Game Loop
        ; Functionality For Each Loop
        JSR ReadInput           ; Read Player Input
    JMP Loop            ; Continue The Game Loop
    
    ReadInput:          ; Read In Data From The System's Controllers
        ; Read Setup
        LDA #$01                ; Load The Binary Value 0000 0001 Into The A Register
        STA $0021               ; Store The Value Of A In Register $0021
        
        ; Send The Latch Pulse To The Controller Hardware
        STA $4016               ; Set The Latch Pulse To High For Controller 1
        STA $4017               ; Set The Latch Pulse To High For Controller 2
        LDA #$00                ; Load 0 Into The A Register
        STA $4016               ; Clear The Latch Pulse To Controller 1
        STA $4017               ; Clear The Latch Pulse To Controller 2
        
        ; Read Button Data
        ReadLoop:
            ; Controller 1
            LDA $4016               ; Load The Value Of Register $4016 Into A
            LSR A                   ; Logical Shift Right For Register A
            ROL $0020               ; Rotate Register $0020 Left
            ; Controller 2
            LDA $4017               ; Load The Value Of Register $4017 Into A
            LSR A                   ; Logical Shift Right For Register A
            ROL $0021               ; Rotate Register $0021 Left
            BCC ReadLoop            ; If The Carry Flag Is Low, Repeat The Read Loop
            
        RTS                     ; Return From Subroutine
        
    LoadTitle:
    RTS                         ; Return From Subroutine
    
    NMI:                    ; Non Maskable Interupt Vector Definition
        ; Load Graphical Data
        LDA #$02                ; Store The Most Significant Bit For The Range Of Sprite Data ($0200 - $02FF)
        STA $4014               ; Load The Sprite Data In The Range Defined By A (256 Values)

    RTI                     ; Return From Interupt
        
    ;IRQ:                    ; Interupt Request Vector Definition
    
    ; Variables
    PaletteData:
        ; Background Palettes
        .byte $00               ; Universal Background Color [NOT USED]
        .byte $01, $02, $03, $04; BG P0
        .byte $05, $06, $07, $08; BG P1
        .byte $09, $0A, $0B, $0C; BG P2
        .byte $0D, $0E, $0F, $10; BG P3 [LAST VALUE IS ACTUAL BG COLOR]
        ; Sprite Palettes
        .byte $01, $02, $03, $04; SP P0
        .byte $05, $06, $07, $08; SP P1
        .byte $09, $0A, $0B, $0C; SP P2
        .byte $0D, $0E, $0F, $10; SP P3
  
.segment "VECTORS"      ; VECTORS SEGMENT -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
    .word NMI               ; Non Maskable Interupt Vector
    .word Reset             ; Reset & Startup Vector
    ;.word IRQ               ; Interupt Request Vector
    
.segment "CHARS"        ; CHARS SEGMENT -_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_
    .incbin "ssg.chr"       ; Character Binary File