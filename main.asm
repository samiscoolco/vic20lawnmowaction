.segment      "ZEROPAGE"
tempaddr:     .res 2
tempaddr2:    .res 2      
rand_seed:    .res 1

.segment "CODE"
; kernal routines
CHROUT = $ffd2
GETIN  = $ffe4
PLOT   = $fff0
READST = $ff8a

; char codes
CHR_CLR_HOME = 147

; color codes
COLOR_BLACK   = 0
COLOR_WHITE   = 1
COLOR_RED     = 2
COLOR_CYAN    = 3
COLOR_PURPLE  = 4
COLOR_GREEN   = 5
COLOR_BLUE    = 6
COLOR_YELLOW  = 7

GRASS_CHAR = 102

PLAYER_CHAR   = 81
PLAYER_COLOR  = 6

ROCK_COLOR   = 6
ROCK_CHAR    = 88
BARN_CHAR    = 35

; zero page variables

cur_color   = $0286  ; color for CHROUT

;*****************************************************************
; start prg
;*****************************************************************

  .byt $01,$10 ; PRG file header (starting address of the program)

  .org $1001   ; start of basic program

;*****************************************************************
; basic stub
;*****************************************************************

  ; stub basic program
  ;
  ; 2015 SYS4109
  ;
  .word bend            ; next line link
  .word 2015            ; line number
  .byte $9e,52,49,48,57 ; sys4109 (4096+13 = bytes of basic program)
  .byte 0               ; end of line
  bend:  .word 0          ; end of program

;*****************************************************************
; main program
;*****************************************************************

start:
  jsr clear_screen
  jsr mm
restart:
  jsr clear_screen
  jsr mmshop
  jsr clear_screen
  jsr grass_line

  ldx #255
  stx frame_time

  lda #1
  sta player_x
  sta player_y

  lda #0
  sta score
  sta grass_cut


  
  lda #COLOR_BLUE
  sta cur_color

  lda #69
  sta player_dir
gameloop:
  ; seed rand with low byte of VIA time
  lda $9004       
  sta rand_seed
  
  jsr check_input
  jsr move_player
  jsr score_line

  jsr frame

  jmp gameloop

eof:
  rts      
  
frame:
fts:
  ldx frame_timer
  dex
  stx frame_timer
  bne noftr
  ldx frame_time
  stx frame_timer
noftr:
  rts

mmshop:
  ldx #0
mmshoptext:
  ;last char in line
  lda mmmsg,x
  beq mmshoptext1

  sta $1FB8,x
  lda #COLOR_BLUE
  sta $97B8,x

  inx            
  jmp mmshoptext
mmshoptext1:
  ldx#0
mmshoptext2:
  ;last char in line
  lda shopmsg,x
  beq mmshopl

  sta $1E00,x
  lda #COLOR_BLUE
  sta $9600,x

  inx            
  jmp mmshoptext2
mmshopl:
  ; seed rand with low byte of VIA time
  lda $9004       
  sta rand_seed
  jsr GETIN
  cmp #0
  beq mmshopl
  rts 




mm:
mmtext:
  ;last char in line
  lda mmmsg,x
  beq mml

  sta $1FB8,x
  lda #COLOR_RED
  sta $97B8,x

  inx            
  jmp mmtext
mml:
  ; seed rand with low byte of VIA time
  lda $9004       
  sta rand_seed
  jsr GETIN
  cmp #0
  beq mml    
  rts


clear_screen:
  lda #CHR_CLR_HOME 
  jsr CHROUT        
  rts               
  

grass_line:
    ldx #0  ; row index

grass_row_loop:

    ; seed rand with low byte of VIA time
    lda $9004       
    sta rand_seed


    ; Get screen address
    lda row_lut_lo,x
    sta tempaddr
    lda row_lut_hi,x
    clc
    adc #>$1E00
    sta tempaddr+1

    lda row_lut_lo,x
    sta tempaddr2
    lda row_lut_hi,x
    clc
    adc #>$9600
    sta tempaddr2+1

    ldy #0
fill_row:
    ;; check for exit squre
    txa
    cmp #0
    bne not_start
    tya
    cmp#1
    bne not_start
    lda #BARN_CHAR
    sta (tempaddr),y
    lda #COLOR_RED
    sta (tempaddr2),y
    jmp next_tile

not_start:

    ;;if we on the edges, just always do a rock
    txa
    cmp #0
    beq rockme
    cmp #17
    beq rockme
    tya
    cmp #0
    beq rockme
    tya
    cmp #21
    beq rockme

    jsr rand8
    lda rand_seed
    cmp #10
    bcs not_rock
rockme:
    lda #ROCK_CHAR
    sta (tempaddr),y
    lda #ROCK_COLOR
    sta (tempaddr2),y
    jmp next_tile

not_rock:
    lda #GRASS_CHAR
    sta (tempaddr),y
    lda #COLOR_GREEN
    sta (tempaddr2),y

next_tile:
    iny
    cpy #22
    bne fill_row

    inx
    cpx #18
    bne grass_row_loop

    rts



score_line:
  lda score
  jsr bin_to_ascii
  ldx #0
scloop:
  ;last char in line
  lda scoremsg,x
  beq sexit

  sta $1FB8,x
  lda #COLOR_RED
  sta $97B8,x

  inx            
  jmp scloop
sexit:
  
  lda digits
  sta $1FB8,x
  lda #COLOR_RED
  sta $97B8,x

  lda digits+1
  sta $1FB9,x
  lda #COLOR_RED
  sta $97B9,x

  lda digits+2
  sta $1FBA,x
  lda #COLOR_RED
  sta $97BA,x

  ;done
  rts



;x=dir 0=left 1=right 2=up 3=down
check_input:
  ; get character from buffer (0 if none)
  jsr GETIN       

  cmp #0
   ; nothing pressed
  beq no_key      
 

  cmp #'W'
  bne check_a
  ; w is pressed
  ldx #2
  stx player_dir

  jmp no_key

check_a:
  cmp #'A'
  bne check_s
  ; a is pressed
  ldx #0
  stx player_dir

  jmp no_key

check_s:
  cmp #'S'
  bne check_d
  ; s is pressed
  ldx #3
  stx player_dir
  jmp no_key

check_d:
  cmp #'D'
  bne no_key
  ; d is pressed
  ldx #1
  stx player_dir

no_key:
  rts

;x=dir 0=left 1=right 2=up 3=down ;destroys temp4
move_player:
  ldx frame_timer
  cpx #1
  beq domoveplayer
  rts
domoveplayer:
  ldx player_dir
  stx temp4
  ;param for draw_player to do a clear
  ldx #1
  jsr draw_player
  ldx temp4
checkl:
  cpx #0
  bne checkr
  ;go left
  ldx player_x
  dex
  stx player_x
  jmp nomove
checkr:
  cpx #1
  bne checku
  ;go right
  ldx player_x
  inx
  stx player_x
  jmp nomove
checku:
  cpx #2
  bne checkd
  ;go up
  ldx player_y
  dex
  stx player_y
  jmp nomove
checkd:
  cpx #3
  bne nomove
  ;go up
  ldx player_y
  inx
  stx player_y
nomove:
  ;param to draw reg player
  ldx #0
  jsr draw_player
  rts

; x = 1 → clear mode, x = 0 → draw mode
draw_player:
    ; save mode (0=draw, 1=clear)
    stx temp2             
    ; compute screen pointer to $1E00 + y * 22 + x
    ldx player_y
    lda row_lut_lo,x
    sta tempaddr
    lda row_lut_hi,x
    clc
    adc #$1E
    sta tempaddr+1
    ldy player_x

    ; if drawing
    ldx temp2
    cpx #0
    bne dp_clear

    ; check if tile underneath is pain
    lda (tempaddr),y
    cmp #ROCK_CHAR
    beq pain

    ; check if tile underneath is pain
    lda (tempaddr),y
    cmp #BARN_CHAR
    beq restartme

    ; check if tile underneath is grass 
    lda (tempaddr),y
    cmp #GRASS_CHAR
    bne dp_nograss
    inc grass_cut
    lda grass_cut
    cmp #25
    bne dp_nograss
    lda #0
    sta grass_cut
    inc score
dp_nograss:
    lda #PLAYER_CHAR
    sta (tempaddr),y
    rts
dp_clear:
    lda #32
    sta (tempaddr),y
    rts
pain:
  jmp pain
restartme:
  jmp restart

;handy bin to ascii not written by me
bin_to_ascii:
  sta temp
  ldx #0
hund_loop:
  lda temp
  cmp #100
  bcc tens_loop
  sbc #100
  sta temp
  inx
  jmp hund_loop
tens_loop:
  stx digits
  ldx #0
ten_loop:
  lda temp
  cmp #10
  bcc ones
  sbc #10
  sta temp
  inx
  jmp ten_loop
ones:
  stx digits+1
  lda temp
  sta digits+2
  ; convert to ASCII
  lda digits
  clc
  adc #$30
  sta digits
  lda digits+1
  adc #$30
  sta digits+1
  lda digits+2
  adc #$30
  sta digits+2
  rts

rand8:
    lda rand_seed
    ; LCG: X(n+1) = (A * Xn + C) mod 256
    ; Using A=17, C=23 for quick demo (parameters can be tuned)
    clc
    asl         ; multiply by 2
    asl         ; multiply by 4
    asl         ; multiply by 8
    asl         ; multiply by 16
    adc rand_seed  ; rand_seed * 17
    adc #23        ; + 23
    sta rand_seed
    rts
  


;*****************************************************************
; data
;*****************************************************************
player_x:     .byte 0
player_y:     .byte 0
player_dir:   .byte 0
frame_timer:  .byte 0
frame_time:   .byte 0
temp:         .byte 0
temp2:        .byte 0
temp3:        .byte 0
temp4:        .byte 0
score:        .byte 0
grass_cut:    .byte 0
digits:       .res 3
holyhell:     .res 2000
;text
;                 s  h  o p
shopmsg:    .byte 19,8,15,16,0
;                 s  c  o  r  e  : 
scoremsg:   .byte 19,3 ,15,18,5 ,58,32,0
;                 p  r  e s  s     a n  y     k  e y     t  o     s  t  a r  t
mmmsg:      .byte 16,18,5,19,19,32,1,14,25,32,11,5,25,32,20,15,32,19,20,1,18,20,0

;lookup tables. high/low byte for screen coords
row_lut_lo:
  .byte <(0*22), <(1*22), <(2*22), <(3*22), <(4*22), <(5*22)
  .byte <(6*22), <(7*22), <(8*22), <(9*22), <(10*22), <(11*22)
  .byte <(12*22), <(13*22), <(14*22), <(15*22), <(16*22), <(17*22)

row_lut_hi:
  .byte >(0*22), >(1*22), >(2*22), >(3*22), >(4*22), >(5*22)
  .byte >(6*22), >(7*22), >(8*22), >(9*22), >(10*22), >(11*22)
  .byte >(12*22), >(13*22), >(14*22), >(15*22), >(16*22), >(17*22)
