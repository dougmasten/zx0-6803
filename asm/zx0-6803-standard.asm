; zx0-6803-standard.asm - ZX0 decompressor for M6803 - 116 bytes
; Written for the DASM assembler, https://github.com/dasm-assembler/dasm
;
; Copyright (c) 2022 Simon Jonassen and Doug Masten
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
zx0_bit         equ     $fd             ; bit stream
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
                ldd     #$80ff
                staa    zx0_bit         ; init bit stream
                tba                     ; reg A = $FF
                std     zx0_offset      ; init offset = -1

;------------------------------------------------------------------------------
; 0 - literal (copy next N bytes from compressed data)
;------------------------------------------------------------------------------
zx0_literals    bsr     zx0_elias       ; obtain length in D reg
                bsr     zx0_copy_loop   ; copy literals (D is stored 1st)
                bcs     zx0_new_offset  ; branch if next block is new-offset

;------------------------------------------------------------------------------
; 0 - copy from last offset (repeat N bytes from last offset)
;------------------------------------------------------------------------------
                bsr     zx0_elias       ; obtain length
zx0_copy        std     zx0_length      ; setup length
                ldd     zx0_dest        ; get current destination
                addd    #$ffff          ; calculate offset address
zx0_offset      equ     *-2             ; *** SMC ***
                pshx                    ; save reg X
                pshb                    ; transfer reg D to X
                psha                    ;   "       "     "
                pulx                    ;   "       "     "
                bsr     zx0_copy_bytes  ; copy match
                pulx                    ; restore reg X
                bcc     zx0_literals    ; branch if next block is literals

;------------------------------------------------------------------------------
; 1 - copy from new offset (repeat N bytes from new offset)
;------------------------------------------------------------------------------
zx0_new_offset  bsr     zx0_elias       ; obtain offset MSB
                negb                    ; adjust for negative offset (set carry for RORA below)
                beq     zx0_eof         ; eof? (length = 256) if so exit
                tba                     ; transfer to MSB position
                ldab    ,x              ; obtain LSB offset
                inx                     ; bump source pointer
                rora                    ; last offset bit becomes first length bit
                rorb                    ;  "     "     "    "      "     "      "
                std     zx0_offset      ; preserve new offset
                ldd     #1              ; set elias = 1
                bcs     zx0_inc_len
                bsr     zx0_elias_loop
zx0_inc_len     addd    #1              ; length = length + 1
                bra     zx0_copy        ; copy new offset match

;------------------------------------------------------------------------------
; interlaced elias gamma coding
;------------------------------------------------------------------------------
zx0_elias       ldd     #1              ; set elias = 1
                bra     zx0_elias_start ; goto start of elias gamma coding

zx0_elias_loop  lsl     zx0_bit         ; get next bit
                rolb                    ; rotate elias value
                rola                    ;   "     "     "
zx0_elias_start lsl     zx0_bit         ; get next bit
                bne     zx0_elias_bt    ; branch if bit stream is not empty
                psha                    ; save reg A
                ldaa    ,x              ; load another 8-bits
                inx                     ; bump source pointer
                rola                    ; get next bit
                staa    zx0_bit         ; save bit stream
                pula                    ; restore reg A
zx0_elias_bt    bcc     zx0_elias_loop  ; loop until done
zx0_eof         rts                     ; return

;------------------------------------------------------------------------------
; copy length bytes from X to dest and get next bit
;------------------------------------------------------------------------------
zx0_copy_loop   std     zx0_length      ; save loop counter
zx0_copy_bytes  ldab    ,x              ; copy byte
                stab    $ffff           ;  "    "
zx0_dest        equ     *-2             ; *** SMC ***
                inx                     ; bump source pointer
                inc     zx0_dest+1      ; increment dest ptr
                bne     zx0_dest_inc    ;   "        "    "
                inc     zx0_dest        ;   "        "    "
zx0_dest_inc    ldd     zx0_length      ; decrement loop counter
                subd    #1              ;    "       "      "
                bne     zx0_copy_loop   ; loop until done
                lsl     zx0_bit         ; get next bit
                rts

