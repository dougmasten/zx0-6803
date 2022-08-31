; zx0-6803-turbo.asm - ZX0 decompressor for M6803 - 164 bytes
; Written for the DASM assembler, https://github.com/dasm-assembler/dasm
;
; Copyright (c) 2022 Doug Masten and Simon Jonassen
; ZX0 compression (c) 2021 Einar Saukas, https://github.com/einar-saukas/ZX0
;
; This software is provided 'as-is', without any express or implied
; warranty. In no event will the authors be held liable for any damages
; arising from the use of this software.
;
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute it
; freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must not
;    claim that you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation would be
;    appreciated but is not required.
; 2. Altered source versions must be plainly marked as such, and must not be
;    misrepresented as being the original software.
; 3. This notice may not be removed or altered from any source distribution.


;------------------------------------------------------------------------------
; Variables - Use On-chip RAM
;------------------------------------------------------------------------------
zx0_bit         equ     $fc             ; bit stream
zx0_elias1      equ     $fd             ; used for fast load of elias (LSB) = 1
zx0_length      equ     $fe             ; copy length (2 bytes)


;------------------------------------------------------------------------------
; Function    : zx0_decompress
; Entry       : Reg X = start of compressed data
;             : Reg D = start of decompression buffer
; Exit        : Reg X = end of compressed data + 1
;             : zx0_dest = end of decompression buffer + 1
; Description : Decompress ZX0 data (version 1)
;------------------------------------------------------------------------------
zx0_decompress
                std     zx0_dest        ; init destination address
                ldd     #$ffff
                std     zx0_offset      ; init offset = -1
                ldd     #$8001
                std     zx0_bit         ; init bit stream plus elias (LSB) = 1
                bra     zx0_literals    ; start with literals

;------------------------------------------------------------------------------
; 1 - copy from new offset (repeat N bytes from new offset)
;------------------------------------------------------------------------------
zx0_new_offset  bsr     zx0_elias       ; obtain offset MSB
                negb                    ; adjust for negative offset and 
                                        ; set carry for RORA below
                beq     zx0_rts         ; eof? (length = 256) if so exit
                tba                     ; transfer to MSB position
                ldab    ,x              ; obtain LSB offset
                inx                     ; increment source pointer
                rora                    ; last offset bit becomes first length bit
                rorb                    ;  "     "     "    "      "     "      "
                std     zx0_offset      ; preserve new offset
                ldd     #1              ; set elias = 1
                bcs     zx0_inc_len     ; branch if we have elias value
                ldaa    zx0_bit         ; load bit stream
                bsr     zx0_elias_bt    ; get length
zx0_inc_len     addd    #1              ; length = length + 1
zx0_copy        std     zx0_length      ; save length
                ldd     zx0_dest        ; get current destination
                addd    #$ffff          ; calculate offset address
zx0_offset      equ     *-2             ; *** SMC ***
                stx     zx0_oldx        ; save reg X
                std     *+4             ; transfer reg D to X (SMC)
                ldx     #$ffff          ;    "      "     "
                ldab    zx0_length+1    ; load length LSB
                bsr     zx0_copy_bytes  ; copy match
                ldx     #$ffff          ; restore reg X
zx0_oldx        equ     *-2             ; *** SMC ***
                bcs     zx0_new_offset  ; branch if next block is new offset

;------------------------------------------------------------------------------
; 0 - literal (copy next N bytes from compressed data)
;------------------------------------------------------------------------------
zx0_literals    bsr     zx0_elias       ; obtain length
                staa    zx0_length      ; save length MSB
                bsr     zx0_copy_bytes  ; copy literals
                bcs     zx0_new_offset  ; branch if next block is new-offset

;------------------------------------------------------------------------------
; 0 - copy from last offset (repeat N bytes from last offset)
;------------------------------------------------------------------------------
                bsr     zx0_elias       ; obtain length
                bra     zx0_copy        ; go copy last offset block


;------------------------------------------------------------------------------
; interlaced elias gamma coding
;------------------------------------------------------------------------------
zx0_elias_loop  bcs     zx0_elias_a0    ; done? then exit
zx0_elias_bt    lsla                    ; get next bit
                rolb                    ; rotate bit into elias
zx0_elias       lsla                    ; get next bit
                bne     zx0_elias_loop  ; if bit stream is not empty, loop again

; reload bit stream and continue processing (8-bit) elias
                ldaa    ,x              ; load another group of 8 bits
                inx                     ; bump source pointer
                rola                    ; get next bit
                bcs     zx0_elias_a0    ; done? then exit
                lsla                    ; get next bit
                rolb                    ; rotate bit into elias
                lsla                    ; get next bit
                bcs     zx0_elias_a0    ; done? then exit
                lsla                    ; get next bit
                rolb                    ; rotate bit into elias
                lsla                    ; get next bit
                bcs     zx0_elias_a0    ; done? then exit
                lsla                    ; get next bit
                rolb                    ; rotate bit into elias
                lsla                    ; get next bit
                bcs     zx0_elias_a0    ; done? the exit

; process long (16-bit) elias
                staa    zx0_bit         ; save bit stream
                clra                    ; set elias MSB = 0
zx0_elias_lp2   lsl     zx0_bit         ; get next bit
                rolb                    ; rotate bit into elias
                rola                    ;   "     "    "   "
                lsl     zx0_bit         ; get next bit
                bne     zx0_elias_skip  ; branch if bit stream is not empty
                psha                    ; save reg A
                ldaa    ,x              ; load another group of 8 bits
                inx                     ; increment source pointer
                rola                    ; get next bit
                staa    zx0_bit         ; save bit stream
                pula                    ; restore reg A
zx0_elias_skip  bcc     zx0_elias_lp2   ; loop again until done
zx0_rts         rts                     ; return

; exit for 8-bit elias
zx0_elias_a0    staa    zx0_bit         ; save bit stream
                clra                    ; set elias MSB = 0
                rts                     ; return


;------------------------------------------------------------------------------
; copy length bytes from X to dest and get next bit
;------------------------------------------------------------------------------
zx0_copy_msb    dec     zx0_length      ; decrement loop counter MSB
zx0_copy_bytes  ldaa    ,x              ; copy byte
                staa    $ffff           ;  "    "
zx0_dest        equ     *-2             ; *** SMC ***
                inx                     ; increment source pointer
                inc     zx0_dest+1      ; increment dest pointer
                bne     zx0_dest_inc    ;     "      "      "
                inc     zx0_dest        ;     "      "      "
zx0_dest_inc    decb                    ; decrement loop counter LSB
                bne     zx0_copy_bytes  ; loop again for 8-bit block
                ldaa    zx0_length      ; test loop counter MSB
                bne     zx0_copy_msb    ; loop again until all done
                ldd     zx0_bit         ; load bit stream and set elias LSB = 1
                lsla                    ; get next bit
                rts                     ; return

