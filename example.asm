; This write routine assumes the following:
;
; 1. A valid starting address (the least significant bits = ‘00’)is loaded in ADDRH:ADDRL
; 2. The 8 bytes of data are loaded, starting at the address in DATADDR
; 3. ADDRH, ADDRL and DATADDR are all located in shared data memory 0x70 - 0x7f
;
BSF STATUS, RP1 ;
BCF STATUS, RP0 ; Bank 2
MOVWF EEADRH ;
MOVWF EEADR ;
MOVWF FSR ;
MOVWF EEDATA ;

MOVLW 0x55 ; Start of required write sequence:
MOVWF EECON2 ; Write 55h
MOVLW 0x22 ;
MOVWF EECON2 ; Write AAh
BSF EECON1, 0 ; Set WR bit to begin write
