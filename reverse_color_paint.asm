*=$1000
main
        jsr write_byte
        rts
write_byte
        ldx #0
loop
        lda #160
        sta $400,x
        lda string_a,x
        sta $d800,x
        lda #160
        sta $4C8,x
        lda string_b,x
        sta $d8cb,x
        lda #160
        sta $590,x
        lda string_c,x
        sta $d993,x
        lda #160
        sta $658,x
        lda string_d,x
        sta $da5b,x
        lda #160
        sta $720,x
        lda string_e,x
        sta $db23,x
        inx
        cpx #200
        bne loop
        rts

*=$2000
string_a
            byte 0,0,0,0,0,0,0,0,0,0,9,9,9,9,9,9,2,15,15,2,0,0,0,0,0,0,9,9,9,9,11,11,11,11,11,11,11,11,11,11
            byte 9,9,0,9,0,0,0,0,0,0,0,0,0,0,9,11,15,15,15,10,2,9,0,0,0,0,9,11,11,9,11,11,11,11,11,11,11,11,11,11
            byte 9,9,9,9,9,0,9,0,0,0,0,0,0,0,0,11,15,15,10,10,10,2,9,9,9,0,11,11,11,11,11,11,11,11,11,11,11,11,11,11
            byte 0,0,0,0,0,0,0,0,0,0,0,0,0,9,0,10,10,10,15,15,10,2,2,2,9,0,11,11,11,11,11,11,11,11,11,11,11,11,11,11
            byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,2,10,2,10,10,2,9,9,0,0,11,11,11,11,11,11,11,11,11,11,11,11,11,11
string_b
            byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,9,15,10,2,2,2,2,11,0,0,0,0,0,11,11,11,11,11,11,11,11,11,11,11
            byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12,2,2,15,15,11,0,0,0,0,0,0,0,0,11,11,11,11,11,11,11,11
            byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11,12,1,12,15,1,15,11,0,0,0,0,0,0,0,0,11,11,11,11,11,9,11
            byte 9,0,9,9,9,0,0,0,9,9,9,0,0,0,0,0,9,12,1,1,12,1,15,1,11,11,0,0,0,0,11,0,9,0,0,11,11,9,9,11
            byte 9,9,9,9,9,0,0,0,9,9,9,0,0,0,0,11,11,15,15,12,15,12,15,1,2,11,11,0,0,0,11,11,11,0,0,0,11,11,0,11
string_c
            byte 0,0,0,9,9,0,0,9,9,9,9,0,0,0,0,11,11,15,15,12,15,15,15,15,11,12,3,0,0,0,11,11,0,0,0,0,11,11,0,11
            byte 0,0,9,9,0,0,9,9,0,0,11,11,0,0,11,12,11,12,1,12,15,15,12,12,12,12,12,0,0,0,0,0,0,0,11,0,0,11,0,0
            byte 9,0,9,0,0,0,9,9,0,0,0,11,0,0,0,15,12,12,11,15,15,15,11,12,12,11,11,9,0,0,0,0,0,11,12,12,0,0,9,11
            byte 0,9,9,0,0,0,11,12,0,0,0,0,0,0,0,11,3,15,11,12,15,12,11,11,11,9,9,0,0,0,0,0,12,12,12,12,11,9,0,0
            byte 0,9,9,9,0,0,2,15,15,11,0,0,0,0,0,9,11,9,11,11,2,12,11,11,11,11,11,11,0,0,11,12,1,15,12,2,9,9,0,0
string_d
            byte 0,0,9,9,0,11,15,15,1,1,1,15,12,0,0,9,9,12,12,11,9,12,11,11,11,11,11,11,11,12,1,15,15,15,12,11,0,0,0,0
            byte 0,0,0,0,0,11,15,1,1,1,1,1,1,15,11,9,9,9,12,9,2,11,11,11,0,11,15,15,1,1,1,15,15,12,12,11,9,9,9,9
            byte 8,8,8,8,8,8,2,15,15,15,15,1,15,15,10,2,9,9,0,0,11,2,12,11,12,1,1,15,15,1,15,12,12,2,11,9,9,9,9,9
            byte 10,10,10,8,10,10,10,10,10,10,8,9,12,15,10,10,2,9,0,0,0,0,11,12,1,15,15,15,15,12,2,11,11,11,0,0,0,9,9,9
            byte 10,15,15,9,9,8,9,8,9,9,8,9,0,2,2,2,2,2,10,10,10,12,1,15,15,12,12,12,2,11,9,0,0,0,0,0,0,9,9,9
string_e
            byte 10,10,10,8,9,8,9,9,9,9,9,9,0,0,2,2,2,10,2,2,2,2,11,12,11,11,9,0,0,0,0,0,0,0,0,0,0,0,9,9
            byte 10,10,10,8,9,8,9,9,9,9,9,9,0,0,0,9,9,11,11,9,9,9,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte 10,10,10,8,9,9,9,9,9,9,9,9,0,0,0,0,0,0,0,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte 8,8,2,9,9,9,9,9,9,9,9,9,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte 8,2,10,8,9,8,9,9,9,9,9,9,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
