;   © Dharmesh Malam 2003
;   Memory Addressed Display Software Stack

;    This program is free software: you can redistribute 
;    it and/or modify it under the terms of the 
;    GNU General Public License as published by the 
;    Free Software Foundation, either version 3 of 
;    the License, or (at your option) any later version.

;    This program is distributed in the hope that 
;    it will be useful, but WITHOUT ANY WARRANTY;
;    without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
;    See the GNU General Public License for more details.

;    A copy of the GNU General Public License is 
;    available here <http://www.gnu.org/licenses/>.

; routine table
outram             equ     0400h
find_pixel         equ     0441h
sdr8_cl            equ     048ah
put_pixel          equ     046ah
remove_pixel       equ     0472h
change_pixel       equ     047bh
test_pixel         equ     0483h
clear_vram         equ     051ch
put_char           equ     0529h
put_small          equ     0565h
rand               equ     0634h
binbcd             equ     0646h
put_num            equ     069ch
wait               equ     06b2h
pause              equ     06bch

sound              equ     0726h
sound_test         equ     0759h

menu               equ     0044h
logo               equ     0120h

vidram             equ     0800h
pause_temp         equ     8080h
clipflag           equ     0902h
sound_0            equ     0903h
sound_1            equ     0904h
sound_2            equ     0905h

puttext            equ     0950h
randseed           equ     0960h
bcd16              equ     0961h
bcd16string        equ     0966h

;-------------------------------------------------------

;Routine to update external ram with internal VRAM buffer
;© Dharmesh Malam 2003

;pin 0 = trisate port 1 and 2, when high, ports are high impedance, i.e. active low
;pin 1 = main display clock enable, active high, high for normal operation
;pin 2 = reset column/row select logic, active high
;pin 3 = reset ram counter, active high
;pin 4 = /rd    ,active low
;pin 5 = /wr    ,active low
;pin 6 =manual clock of ram
;pin 7 =/en2, low for normal operation

;normal mode
;76543210
;00100011

;routine stops mdc
;then resets column,row logic, disables screen
;then reset ram counter
;makes /rd high
;load data on ports 1 and 2
;enables port tristates
;pulse /wr
;pulse clock
;repeat until all ram is updated
;disable port tristates
;reset logic, and ram counter
;make /rw low
;start mdc
;return

#include zinc.asm

org outram

outram:
di                            ; disables interrupts, for speed
ex af,af'                     ;'
exx
                              ; 00100011
ld a,10110001b                ; make /rd high, and stop mdc, /en2 -> high
out (2),a                     ; output

ld a,10111101b                ; make reset on col/row logic and ram counter high
out (2),a                     ; output

ld a,10110100b                ; keeps col/row logic high, and makes ram counter low, and enables tristates
out (2),a                     ; output

ld hl,vidram                  ; point to vidram
ld b,56                       ; set counter 28*2=56

out_loop:
    ld c,0                    ; set port pointer to 0
    ld d,(hl)                 ; get byte
    out (c),d                 ; output to port 0
    inc hl                    ; increment pointer
    inc c                     ; increment port counter
    ld d,(hl)                 ; get next byte
    out (c),d                 ; output to port 1    
                              ; pulse /wr
    ld a,10010100b            ; make /wr low
    out (2),a                 ; output
    ld a,10110100b            ; make /wr high
    out (2),a                 ; output

                              ; pulse clock to ram
    ld a,11110100b            ; make clock high
    out (2),a                 ; output
    ld a,10110100b            ; make clock low
    out (2),a                 ; output
        inc hl                ; increment pointer
    djnz out_loop             ; loop until entire ram is refreshed

ld a,10111101b                ; reset ram counter, and disable tristates
out (2),a                     ; output

ld a,10100001b                ; pull above resets low, and make /rd low
out (2),a                     ; output

ld a,00100011b                ; enable mdc,and /en2
out (2),a                     ; output

ex af,af'                     ;'
exx

ei                            ; re-enable interrupts
ret

end

;-------------------------------------------------------
;© Dharmesh Malam 2003
; Boot code

org 000h
start:

ld sp,0fffh                    ; initialise stack
im 1
xor a
ld   (0903h),a
ld   (0904h),a
ld   (0905h),a
inc  a
ld   (0960h),a
ei
call clear_vram
jp logo

;--------------------------------------------------------
;© Dharmesh Malam 2003
; Clear video ram

org clear_vram

clear_vram:
push hl
xor a                        ; zero a
ld hl,vidram
ld b,112                     ; rows/8 * columns
    clear:
    ld (hl),a
    inc hl
djnz clear                   ; clear vidram
pop hl
ret

;if sound_x is zero, turn off sound
;if sound_x is non-zero, then turn on sound for that many 100ths sec

sound_0 equ 0903h
sound_1 equ 0904h
sound_2 equ 0905h

snd:
push bc
ld b,0

snd_0:
ld a,(sound_0)                ; get sound var
cp 0                          ; is it zero
jr z,snd_1                    ; if so end sound
dec a                         ; otherwise dec counter
ld (sound_0),a                ; save back
set 0,b                       ; set 0
jr snd_1

snd_1:
ld a,(sound_1)
cp 0
jr z,stop_1
dec a
ld (sound_0),a
set 0,b
jr snd_2

snd_2:
ld a,(sound_2)
cp 0
jr z,out_snd
dec a
ld (sound_0),a
set 0,b

out_snd:
ld a,b
out (3),a
pop bc
ret

;--------------------------------------------------------------

;© Dharmesh Malam 2003
; returns pseudo random 8 bit number in a.
; (r_seed) is a in ram byte must be initialised to a non zero value
;or this function will always return zero

rand_8:
    ld    a,(r_seed)               ; get seed
    and    184                     ; mask non feedback bits
    scf                            ; set carry
    jp    po,no_clr                ; skip clear if odd
    ccf                            ; complement carry (clear it)
no_clr:
    ld    a,(r_seed)               ; get seed back
    rla                            ; rotate carry into byte
    ld    (r_seed),a               ; save back for next prn
    ret                            ; done

r_seed:
    db,1                           ; prng seed byte (must not be zero)

;---------------------------------------------------------------

;© Dharmesh Malam 2003
;find pixel routine for 3,4x6, (21,28x30 dots)
;b=x c=y
;origin is top left corner

vidram equ 0fc00h

org 0000h

find_pixel:
push bc                           ; save bc

ld hl,vidram                      ; start of video ram
ld a,c                            ; a=y times y by how many bytes per row
sla a                             ; a=2y
sla a                             ; a=4y
ld l,a                            ; l=4y
ld a,b                            ; a=x
srl a                             ; a=int(x/2)
srl a                             ; a=int(x/4)
srl a                             ; a=int(x/8)
add a,l                           ; a=int(x/8) + 4y
ld l,a                            ; l=int(x/8) + 4y, so hl point to right byte

ld a,b                            ; a=x
and 7                             ; mask a,0000111,so which bit selected
ld bc,fp_bits                     ; bc point to first bit mask
add a,c                           ; simulated 16 bit addition,
ld c,a                            ; ..need even though 7 is max
adc a,b                           ; ..as fp-bits is a 16 bit address
sub c                             ; ..
ld b,a                            ; .., bc points to right mask
ld a,(bc)                         ; a=mask
pop bc                            ; restore bc
ret                               ; return to caller

fp_bits:     db 80h,40h,20h,10h,08h,04h,02h,01h

;----------------------------------------------------------

;basic pixel operations
;© Dharmesh Malam 2003
;all destroy a and need findpixel

#include zinc.asm

put_pixel:                        ; blit pixel at b,c
push hl
call findpixel
or (hl)
ld (hl),a
pop hl
ret

remove_pixel:                     ; remove pixel at b,c
push hl
call findpixel
cpl
and (hl)
ld (hl),a
pop hl
ret

change_pixel:                     ; change pixel at b,c
push hl
call findpixel
xor (hl)
ld (hl),a
pop hl
ret
test_pixel:                       ; tests pixel at b,c
push hl
call findpixel
and (hl)                          ; zero flag set if no pixel
pop hl
ret

;----------------------------------------------------------

;sprite drawing routine (sdr): 8 bit clipping version
;© Dharmesh Malam 2003
;inputs:
;    b=x coordinate
;    c=y coordinate
;    hl=pointer to sprite data
;outputs:
;    af=destroyed
;    bc=restored
;    de=restored
;    hl=restored
;    no other registers are changed
;;sprite data should be stored as follows:
;db (# rows/bytes/height of sprite)
;db (bitmap of sprite)
;;notes:
;; if x between -1 and -7, then some left clipping,
;......less than or equal to -8, then return
;if x bewtween 0 and 22, then no clipping
;if x between 23 and 29, then right clipping
;if x greater than 30, then return

;if y less than or equal to -(lenght), then return    t
;if y between -1 and -(lenght)+1, then top clippng    t
;if y bewtwenm 0 and (27-lenght), then no clipping    n
;if y between (27-lenght)+1 and 27, then bottom clipping    b
;if y greater than 28, then return                b

org sdr8_cl
clipflag: db,0

sdr8_cl:
push bc                                    ; save registers
push de
push hl

ld e,(hl)                                  ; e=number of rows in sprite
inc hl                                     ; next byte (points to sprite now)
xor a                                      ; zero a
ld (clipflag),a                            ; clear clipflag

; top clipping done

check_right:
ld a,b                                     ; a=x coor
bit 7,b                                    ; is x negative?
jr nz,sdr8cl_checklclip                    ; if so then jump to check left
cp 30                                      ; check if x is greater than or equal to 30
jr nc,endsdr                               ; if so, then carry is set, and jump
jr check_bot                               ; else, done l/r clipping (right clipping is "automatic")

sdr8cl_checklclip:                         ; now check left clipping
cp -7                                      ; check if starts too far left
jr c,endsdr                                ; if so, sprite won't show
add a,32                                   ; add 32 to x coor to make it start on right side and wrap
ld b,a                                     ; save into x coor
ld (clipflag),a                            ; and make flag non-zero

check_bot:                                 ; check for bottom clipping
bit 7,c
jr nz,check_top                            ; is y negative, then jump to check top

ld a,c                                     ; get y-coord
cp 28                                      ; check if greater than 28
jr nc,endsdr                               ; y is greater than 28, so end, as not viewable

ld a,28                                    ; number of rows in display
sub e                                      ; subtract rows in sprite
cp c                                       ; compare to y-coord
jr nc,sdr8cl_doneclip                      ; if no carry, then no bottom clipping
                                           ; so clip bottom
ld a,28                                    ; rows in display
sub c                                      ; subtract the y-coord
ld e,a                                     ; use above result as reduced row counter
                                           ; bottom clipping done
jr sdr8cl_doneclip

check_top:
                                           ; y is neg
ld a,c                                     ; a is -1 or less
add a,e                                    ; subtract the number of rows in sprite
jr nc,endsdr                               ; if no-carry, then not on screen
                                           ; y is between -1 and -(lenght)+1
push bc                                    ; save x,y coords
ld c,a                                     ; c has number of rows to put
sub e                                      ; make number inverse
neg
ld b,a                                     ; b has counter to shift hl by
ld e,c                                     ; c has e
sprite_shift:                              ; shift hl b times to clip it
    inc hl
djnz sprite_shift
pop bc                                     ; restore x,coords
ld c,0                                     ; y will always be o

sdr8cl_doneclip:
push hl                                    ; push pointer to sprite to stack
call find_pixel
ld c,e                                     ; c=number of rows in sprite
add a,a                                    ; -> sets all bits to the right of the set bit (sprite mask)
dec a                                      ; /
ld e,a                                     ; save sprite mask into e
ld a,b                                     ; get x coor
and 00000111b                              ; find remainder of x/8
inc a                                      ; increase to get range of 1-8
ld b,a                                     ; b=rotate counter

sdr8cl_newrow:
ex (sp),hl                                 ; hl(video location)<->(sp)(pointer to sprite)
ld a,(hl)                                  ; a=byte in sprite
inc hl                                     ; next byte
ex (sp),hl                                 ; hl<->(sp)
push bc                                    ; save rotate counter and row counter

rlca                                       ; rotate left once...
sdr8cl_prepbyte:
    rrca                                   ; ...then rotate right...
djnz sdr8cl_prepbyte                       ; ...b times

ld b,a                                     ; save rotated sprite byte into b
ld a,(clipflag)                            ; a=left clipping flag
or a                                       ; check if not 0
jr nz,sdr8cl_skipleft                      ; if left clipping, only display right half of sprite
ld a,b                                     ; else, load rotated sprite byte back into a
and e                                      ; mask off left bits
ld d,a                                     ; (check sdr8 to see how this works...)
ld a,e
cpl
and (hl)
or d
ld (hl),a
inc hl

ld a,l                                     ; a=lower byte of video location to put sprite
and 00000011b                              ; mask off upper nibble
jr z,sdr8cl_nextrow                        ; if zero, hl now points to a new row, so right clipping
jr sdr8cl_putright                         ; jump for normal drawing

sdr8cl_skipleft:
dec hl                                     ; decrease hl by 3 to point it to start of row for clipping
dec hl
dec hl

sdr8cl_putright:
ld a,e                                     ; a=sprite mask
cpl                                        ; switch every byte
and b                                      ; mask off right bits
ld d,a                                     ; save masked sprite byte into d
ld a,e                                     ; a=sprite mask (again)
and (hl)                                   ; mask off bits where sprite's going to be put
or d                                       ; set bits in sprite byte
ld (hl),a                                  ; and put right half of sprite

sdr8cl_nextrow:
ld bc,4h-1                                 ; bc=number of bytes in one row-1
ld a,(clipflag)                            ; see if clipflag is zero
or a
jr z,left_skip                             ; if not, then add 7, as clipping changes values
ld bc,7

left_skip:
add hl,bc                                  ; add to point hl to next row
pop bc                                     ; restore rotate counter and row counter
dec c                                      ; decrease row counter
jr nz,sdr8cl_newrow                        ; and repeat
pop hl                                     ; restore pointer to sprite (now points to byte after sprite)

endsdr:
pop hl                                     ; restore registers
pop de
pop bc
ret                                        ; return

;------------------------------------------------------------------------------

; bintobcd
;; these routines format a registers into their ascii string equivalents, ie:
; the number 143 would be turned into
; .db "143", 0
;; bintobcd16 does not format strings with leading zeroes, ie:
; the number 353 would be turned into
; .db "353",0
;     not
; .db "00353",0

;    bintobcd16
;   by joe pemberton
;size: 97 bytes
;;input:
;de=number to convert
;;returns (bcd16) as ten thousands, thousands, hundreds, tens, ones
;(bcd16string) contains the formatted bcd ascii string
;b = 0
;de = 0
;destroys af, bc, de, hl, ix
bintobcd16:
    push    de
    ld    hl,bcd16
    ld    de,bcd16+1
    ld    bc,10
    ld    (hl),0
    ldir
    pop    de
    ld    b,16
bcd16l:    ld    hl,bcd16
    push    bc
    ld    b,5
bcd16i:    ld    a,(hl)
    cp    5
    jr    c,bcd16a
    add    a,3
    ld    (hl),a
bcd16a:    inc    hl
    djnz    bcd16i
    dec    hl                    ;hl now points to the ones
    ld    b,5
    sla    e
    rl    d
bcd16b:    rl    (hl)
    bit    4,(hl)
    jr    z,bcd16c
    res    4,(hl)
    scf
bcd16c:    dec    hl
    djnz    bcd16b
    pop    bc
    djnz    bcd16l
    inc    hl
    ld    ix,bcd16string
    ld    bc,(5*256)+0
bcd16d:    ld    a,(hl)
    add    a,30h
    ld    (ix+0),a
    cp    30h
    jr    nz,bcd16e
    bit    1,c
    jr    z,bcd16f
bcd16e:    inc    ix
    set    1,c
bcd16f:    inc    hl
    djnz    bcd16d
    ret

bcd16:    db    0,0,0,0,0
bcd16string:
    db    0,0,0,0,0,0

;------------------------------------------------------------------------
;© Dharmesh Malam 2003
;put 16 bit number de, at bc with smallfont

put_num        
push de                ; save
push hl
push ix
push bc
push bc

call bintobcd16
pop bc                 ; restore x,y
ld hl,bcd16string      ; point to string
call putsmall          ; draw

pop bc                 ; restore
pop ix
pop hl
pop de

ret

;-------------------------------------------------------------
;© Dharmesh Malam 2003
; library of character/text routines

;       values and conversion of bases and ascii characters
;;1 = value in decimal
;2 = value in hexadecimal
;3 = value in binary
;4 = value in two's complement
;5 = ascii character if valid

; 1      2         3         4          5

; 32     20     00100000     32    space
; 33     21     00100001     33    !
; 34     22     00100010     34    "
; 35     23     00100011     35    #
; 36     24     00100100     36    $
; 37     25     00100101     37    %
; 38     26     00100110     38    &
; 39     27     00100111     39    '
; 40     28     00101000     40    (
; 41     29     00101001     41    )
; 42     2a     00101010     42    *
; 43     2b     00101011     43    +
; 44     2c     00101100     44    ,
; 45     2d     00101101     45    -
; 46     2e     00101110     46    .
; 47     2f     00101111     47    /
; 48     30     00110000     48    0
; 49     31     00110001     49    1
; 50     32     00110010     50    2
; 51     33     00110011     51    3
; 52     34     00110100     52    4
; 53     35     00110101     53    5
; 54     36     00110110     54    6
; 55     37     00110111     55    7
; 56     38     00111000     56    8
; 57     39     00111001     57    9
; 58     3a     00111010     58    :
; 59     3b     00111011     59    ;
; 60     3c     00111100     60    <
; 61     3d     00111101     61    =
; 62     3e     00111110     62    >
; 63     3f     00111111     63    ?
; 64     40     01000000     64    @
; 65     41     01000001     65    a
; 66     42     01000010     66    b
; 67     43     01000011     67    c
; 68     44     01000100     68    d
; 69     45     01000101     69    e
; 70     46     01000110     70    f
; 71     47     01000111     71    g
; 72     48     01001000     72    h
; 73     49     01001001     73    i
; 74     4a     01001010     74    j
; 75     4b     01001011     75    k
; 76     4c     01001100     76    l
; 77     4d     01001101     77    m
; 78     4e     01001110     78    n
; 79     4f     01001111     79    o
; 80     50     01010000     80    p
; 81     51     01010001     81    q
; 82     52     01010010     82    r
; 83     53     01010011     83    s
; 84     54     01010100     84    t
; 85     55     01010101     85    u
; 86     56     01010110     86    v
; 87     57     01010111     87    w
; 88     58     01011000     88    x
; 89     59     01011001     89    y
; 90     5a     01011010     90    z
; 91     5b     01011011     91    [
; 92     5c     01011100     92    \
; 93     5d     01011101     93    ]
; 94     5e     01011110     94    ^
; 95     5f     01011111     95    _
; 96     60     01100000     96    `
; 97     61     01100001     97    a
; 98     62     01100010     98    b
; 99     63     01100011     99    c
; 100    64     01100100     100   d
; 101    65     01100101     101   e
; 102    66     01100110     102   f
; 103    67     01100111     103   g
; 104    68     01101000     104   h
; 105    69     01101001     105   i
; 106    6a     01101010     106   j
; 107    6b     01101011     107   k
; 108    6c     01101100     108   l
; 109    6d     01101101     109   m
; 110    6e     01101110     110   n
; 111    6f     01101111     111   o
; 112    70     01110000     112   p
; 113    71     01110001     113   q
; 114    72     01110010     114   r
; 115    73     01110011     115   s
; 116    74     01110100     116   t
; 117    75     01110101     117   u
; 118    76     01110110     118   v
; 119    77     01110111     119   w
; 120    78     01111000     120   x
; 121    79     01111001     121   y
; 122    7a     01111010     122   z
; 123    7b     01111011     123   {
; 124    7c     01111100     124   |
; 125    7d     01111101     125   }
; 126    7e     01111110     126   ~
; 127    7f     01111111     127   delete

;put_string/char takes the ascii value, subtracts 32, 
;so the first character is 'space', times by 8 and adds it to the font?_rom address.
;this will find the right character.

;bc points to x,y point to put string/char
;e has which font 1,2,3,4, being namco,coleco,speccy,and bbc
;hl, points to the string in put_string
;d, contains the character to display in put_char

org put_char

put_char:          ; put the character in d, with small font, at bc
push hl            ; save registers
push de
push bc
ld hl,small

char_start:
ld a,d            ; load a with ascii code of character
sub 32            ; subtract 32, so first letter is 00, as per in rom
ld e,a            ; put e with ascii code - 32
ld d,0            ; load d with 0, so de is the character needed

add hl,de
add hl,de
add hl,de         ; so hl = offset + 3(ascii - 32)
                  ; need to create 8*6 sprite from condensed data
ld a,6d           ; length of sprite
ld (0950h),a      ; load to ram
ld ix,0951h

ld b,3
sprite_out:
    ld d,(hl)     ; get first byte
    ld a,0f0h     ; split in two
    and d         ; by masking
    ld (ix),a     ; save to ram
    inc ix        ; next location in ram
    ld a,0fh      ; next mask
    and d         ; gets second half
    rlca          ; rotate left
    rlca          ; 4 times
    rlca
    rlca
    ld (ix),a     ; save next half
    inc ix
    inc hl            
djnz sprite_out

ld hl,0950h       ; pointer to sprte in ram
pop bc            ; restore x,y coords
call sdr8_cl      ; draw the character

pop de            ; restore registers
pop hl

ret

put_small:        ; put at bc, (x,y), string pointed to by hl, with small font,end at 0
                  ; before: hl points to string, bc = xy
                  ; after:  hl points to end of string, , bc = (x+4chars),y,
                  ; a=0, d=last byte

ld a,(hl)         ; fetch  byte
cp 0              ; is it zero
ret z             ; if so, then return to caller
ld d,a            ; put character into d for put_char
call put_char
ld a,b            ; get x-coord
add a,4           ; shift x-coord by 5, as small font
ld b,a            ; save
inc hl            ; next byte
jr put_small      ; jump to top,hence repeat until all char's are done

;-----------------------------------------------------------------------------
;© Dharmesh Malam 2003
org pause
pause:
in a,(0)          ; get keys of pad 1
bit 0,a           ; see if start was pressed
ret z             ; return if it wasn't
ld c,2
call wait
in a,(0)
ret z

push hl           ; save registers
push de
push bc

ld hl,vidram      ; save screen, hl points to current screen
ld de,pause_temp  ; temp screen
ld bc,112         ; size of screen
ldir              ; save data

call clear_vram   ; clear screen

ld hl,pause_text  ; "pause"
ld b,6            ; off centre
ld c,8
call put_small    ; put text
ld de,0           ; zero counter
text:
ld b,6            ; coors
ld c,16
call put_num
call outram

ld b,100
key_loop:
    in a,(0)
    bit 0,a
    jr nz,unpause
    ld c,2
    call wait     ; wait for a few ms
djnz key_loop     ; check pad 100 time, then inc counter
inc de
jr text

unpause:
ld c,2
call wait
in a,(0)
bit 0,a
jr z,key_loop
        
ld hl,pause_temp  ; opposite to above
ld de,vidram      ; restore data
ld bc,112
ldir
call outram

pop bc            ; restore registers
pop de
pop hl

ret               ; then return

pause_text:
db "pause",0

;---------------------------------------------
;© Dharmesh Malam 2003

wait:             ; call with c as multiplier
push bc           ; save
wai2:
    ld b,255
    wai:          ; inner loop
    djnz wai
    dec c        
jr nz,wai2        ; outer loop
pop bc
ret

;© Dharmesh Malam 2003
;each font is 4x6, stored condensed
;------------------------------------

org font1_rom
; character 0x20 (space)

db 00000000b
db 00000000b
db 00000000b

; character 0x21 !

db 01000100b
db 01000000b
db 01000000b

; character 0x22 "

db 10101010b
db 10000000b
db 00000000b

; character 0x23 #

db 10101110b
db 10101110b
db 10100000b

; character 0x24 $

db 01001110b
db 01001110b
db 01000000b

; character 0x25 %

db 10100010b
db 01001000b
db 00100000b

; character 0x26 &

db 00001000b
db 10000000b
db 10100000b

; character 0x27 '

db 10001000b
db 00000000b
db 00000000b

; character 0x28 (

db 01001000b
db 10001000b
db 01000000b

; character 0x29 )

db 01000010b
db 00100010b
db 01000000b

; character 0x2a *

db 10100100b
db 11100100b
db 10100000b

; character 0x2b +

db 00000100b
db 11100100b
db 00000000b

; character 0x2c ,

db 00000000b
db 00000100b
db 10000000b

; character 0x2d -

db 00000000b
db 11100000b
db 00000000b

; character 0x2e .

db 00000000b
db 00000000b
db 10000000b

; character 0x2f /

db 00100100b
db 01001000b
db 00000000b

; character 0x30 0

db 01001010b
db 11101010b
db 01000000b

; character 0x31 1

db 01001100b
db 01000100b
db 11100000b

; character 0x32 2

db 01001010b
db 00100100b
db 11100000b

; character 0x33 3

db 11100010b
db 01000010b
db 11000000b

; character 0x34 4

db 10101010b
db 11100010b
db 00100000b

; character 0x35 5

db 11101000b
db 11000010b
db 11000000b

; character 0x36 6

db 01001000b
db 11001010b
db 01000000b

; character 0x37 7

db 11100010b
db 01001000b
db 10000000b

; character 0x38 8

db 01001010b
db 01001010b
db 01000000b

; character 0x39 9

db 01001010b
db 01101010b
db 01000000b

; character 0x3a :

db 00001000b
db 00000000b
db 10000000b

; character 0x3b ;

db 00001100b
db 00000100b
db 10000000b

; character 0x3c <

db 00100100b
db 10000100b
db 00100000b

; character 0x3d =

db 00000000b
db 11100000b
db 11100000b

; character 0x3e >

db 10000100b
db 00100100b
db 10000000b

; character 0x3f ?

db 01001010b
db 00100100b
db 01000000b

; character 0x40 @

db 11001000b
db 11001000b
db 11000000b

; character 0x41 a

db 01001010b
db 11101010b
db 10100000b

; character 0x42 b

db 11001010b
db 11001010b
db 11000000b

; character 0x43 c

db 01101000b
db 10001000b
db 01100000b

; character 0x44 d

db 11001010b
db 10101010b
db 11000000b

; character 0x45 e

db 11101000b
db 11001000b
db 11100000b

; character 0x46 f

db 11101000b
db 11001000b
db 10000000b

; character 0x47 g

db 01101000b
db 10101010b
db 01100000b

; character 0x48 h

db 10101010b
db 11101010b
db 10100000b

; character 0x49 i

db 11100100b
db 01000100b
db 11100000b

; character 0x4a j

db 11100010b
db 00101010b
db 01100000b

; character 0x4b k

db 10101010b
db 11001010b
db 10100000b

; character 0x4c l

db 10001000b
db 10001000b
db 11100000b

; character 0x4d m

db 10101110b
db 10101010b
db 10100000b

; character 0x4e n

db 00001010b
db 11101010b
db 10100000b

; character 0x4f o

db 01001010b
db 10101010b
db 01000000b

; character 0x50 p

db 11001010b
db 11001000b
db 10000000b

; character 0x51 q

db 01001010b
db 10100100b
db 01100000b

; character 0x52 r

db 11001010b
db 11001010b
db 10100000b

; character 0x53 s

db 01101000b
db 01000010b
db 11000000b

; character 0x54 t

db 11100100b
db 01000100b
db 01000000b

; character 0x55 u

db 10101010b
db 10101010b
db 01000000b

; character 0x56 v

db 10101010b
db 10101010b
db 11000000b

; character 0x57 w

db 10101010b
db 10101110b
db 10100000b

; character 0x58 x

db 10101010b
db 01001010b
db 10100000b

; character 0x59 y

db 10101010b
db 01000100b
db 01000000b

; character 0x5a z

db 11100010b
db 01001000b
db 11100000b

; character 0x5b [

db 11101000b
db 10001000b
db 11100000b

; character 0x5c \

db 00001000b
db 01000100b
db 00100000b

; character 0x5d ]

db 11100010b
db 00100010b
db 11100000b

; character 0x5e ^

db 01001010b
db 00000000b
db 00000000b

; character 0x5f (underscore)

db 00000000b
db 00000000b
db 11110000b

;© Dharmesh Malam 2003
;menu routine
org 0044h

menu:
call clear_vram
ld b,3
ld c,1
ld hl,title
call put_small
ld b,4*8
ld hl,vidram
inv:
    ld a,(hl)
    cpl
    ld (hl),a
    inc hl
djnz inv

ld b,1
ld c,8
ld hl,ent_1
call put_small
inc hl
ld b,1
ld c,14
call put_small
inc hl
ld b,1
ld c,20
call put_small

ld c,8
call invert_cur
call outram

menul:
in a,(0)
bit 7d,a                    ; did they press up?
jr nz,menuup                ; move menu bar up
bit 6d,a                    ; or was it down?
jr nz,menudown              ; then move the bar down
bit 2d,a                    ; did they press x
jr nz,menuselect            ; jump to that selection
jr menul                    ; they're just sitting there, loop again

invert_cur
push bc
ld hl,vidram
ld b,0
sla c
sla c
add hl,bc
ld b,4*6

invert:
    ld a,(hl)
    cpl
    ld (hl),a
    inc hl
djnz invert
pop bc
ret

menuup:
in a,(0)
bit 7,a
jr z,menul
call invert_cur              ; invert current selection back to normal
ld a,c
sub 6d
ld c,a                       ; move up
cp 2d
jr nz,menubar                ; if it's zero, we're at the top
ld c,20                      ; so move to the bottom
jr menubar                   ; handle menu bar

menudown:
in a,(0)
bit 6,a
jr z,menul

call invert_cur              ; same as up, go back to normal text
ld a,c                       ; get current entry for check
cp 20                        ; at the bottom?
jr z,menudownt               ; then move to the top
add a,6d                     ; next line
ld c,a                       ; save
jr menubar                   ; handle menu bar

menudownt:
ld c,8d                      ; move to the top

menubar:
call invert_cur              ; invert the new selection
call outram
push bc
ld c,00aah
call wait
pop bc
jr menul                     ; now it's time to loop

menuselect:

push bc
ld c,00aah
call wait
pop bc

ld a,b
cp 8
jr nz cp2
ld hl,jm_1
jr load

cp2:
cp 14
jr nz cp3
ld hl,jm_2
jr load

cp3:
ld hl,jm_3

load:
ld a,(hl)                    ; get lower byte of pointer
inc hl                       ; next byte
ld h,(hl)                    ; get upper byte of pointer
ld l,a                       ; complete pointer with lower byte
jp (hl)                      ; jump to the address from the table--hl, not (hl)

title:
db "*select*"
ent_1:
db "game",0
db "sound",0
db "scroler",0
jm_1:
db 0
db 0
jm_2:
db 0
db 0
jm_3:
db 0
db 0
;----------------------------------------------------------------
;© Dharmesh Malam 2003
;scrolls user inputted text across the screen
;at random y values
;© dharmesh malam 2002
;#include zinc.asm

org 021ah
scroller:

ld b,32
ld hl,temp_string
zero_temps:
    xor a
    ld (hl),a
    inc hl
djnz zero_temps
ld e,a

from_disp:
call clear_vram

ld bc,0000                  ; reset coords    
ld hl,enter1                ; point to 'enter' text
call put_small              ; draw text
ld bc,7                     ; inc y coord
ld hl,enter2                ; point to 'text'
call put_small              ; draw

ld d,32                     ; load that character into d
normal:
ld b,22                     ; put char in bot right
ld c,22    
ld hl,temp_string
call put_small
call outram

input_wait:                 ; loop to edit text
push bc
ld c,00ffh
call wait
pop bc

input_wat
in a,(0)

bit 7,a                     ; zero flag set, if not pressed
jr nz,up                    ; therefore zero flag not set, if pressed

bit 6,a                     ; down
jr nz,down

bit 1,a                     ; button z
jr nz,enter

bit 3,a                     ; button x
jr nz,delete

bit 2,a                     ; start
jr nz,disp

jr input_wat
    up:
    ld a,96
    inc d
    cp d
    jr nz,here1
    ld d,32
    here1:
    push bc
    ld b,25
    ld c,15
    call put_char
    pop bc
    call outram
jr input_wait

down:
ld a,31
dec d
cp d
jr nz, here1
ld d,95
jr here1

enter:
ld a,32                    ; max number of chars
cp e
jr z,input_wait            ; jump if at this num

ld (hl),d                  ; save character
inc e

ld a,b
sub 4
ld b,a

push bc
ld hl,temp_string
call put_small              ; draw string
pop bc

call outram
jr input_wait

delete:
xor a
cp e
jr z,input_wait
dec hl
ld (hl),a                   ; delete char
dec e

push de
ld d," "
call put_char               ; clear bit on left

ld a,b
add a,4
ld b,a
push bc                     ;s ave x,y
ld hl,temp_string
call put_small
pop bc
pop de
call outram
jr input_wait

disp:                       ; we have a string to display
call clear_vram
ld c,0                      ; y, ie top line

loop_init:
ld d,e                      ; number of chars
ld a,30                     ; one display
sla d                       ; x2
sla d                       ; x4
add a,d                     ; add together
ld d,a                      ; this is the counter

ld b,30                     ; x, just off screen

disp_loop:
ld hl,temp_string           ; pointer to string
push bc
push de
call put_small              ; draw with clipping at bc
pop de

call outram                 ; output
call clear_vram             ; clear internal vram

ld c,00aah
call wait                   ; wait 1 tenth
pop bc

in a,(0)
bit 1,a
jr nz,from_disp
bit 2,a
jr nz,menu

dec b
dec d
jr nz,disp_loop             ; loop 30+4(num_char)

ld a,r
and 00011111b
sub 2                       ; get random number between -2 and 30
ld c,a            
jr loop_init                ; loop

enter1:    db,"enter",0
enter2:    db,"text:",0

org 096ah
temp_string:    db,0,0,0,0,0,0,0,0
        db,0,0,0,0,0,0,0,0
        db,0,0,0,0,0,0,0,0,0

end

;-----------------------------------------------------------------

;© Dharmesh Malam 2003
org 120h

logo:                      ;does intro screen
ld b,0
ld c,4
ld hl,text1                ;darmesh (not enough space for correct spelling on screen)
call put_small
ld b,0
ld c,12
inc hl
call put_small             ;malam
call outram
ld c,00ffh
call wait

ld b,0
ld c,20
inc hl                      ;presents
call put_small
call outram
ld c,00ffh
ld b,4
wait_l:
    call wait
djnz wait_l

call clear_vram
ld b,0
ld c,4
inc hl
call put_small              ;memory

ld b,0
ld c,10
inc hl
call put_small              ;adresed  (not enough space for correct spelling on screen)

ld b,0
ld c,19
inc hl
call put_small              ;display
call outram

ld c,00ffh
ld b,4
wait_ll:
    call wait
djnz wait_ll

ld hl,vidram
ld b,112
invert:
    ld a,(hl)
    cpl
    ld (hl),a
    call outram
    inc hl
    ld c,10
    call wait
djnz invert

call clear_vram
ld d,18

ld b,0
ld c,0
ld hl,mad
slide:
    call put_small
    call outram
    push bc
    ld c,0aah
    call wait
    pop bc
    inc b
    inc c
    dec d
jr nz,slide

ld b,3
ld c,19
ld hl,face
call sdr8_cl
call outram

ld c,00ffh
ld b,2
wait_lll:
    call wait
djnz wait_lll

ld b,112
shift:
    push bc
    ld hl,0870h-4
    ld de,0870h
    ld bc,112
    lddr
    call outram

    ld c,90h
    call wait
    pop bc
djnz shift            ;shift down the display

ld c,00ffh
call wait
jp 0044h
                      ;resources
text1:
db "darmesh",0
db "malam",0
db "presents",0
db "memory",0
db "adresed",0
db "display",0
mad:
db "mad",0
face:
db 8
db 00111100b
db 01000010b
db 10100101b
db 10000001b
db 10100101b
db 10011001b
db 01000010b
db 00111100b

;end
