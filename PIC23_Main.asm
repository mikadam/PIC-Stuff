;----------------------------------------------------------;
; Program title: 8-bit Multiplexing Shift Register         ;
;----------------------------------------------------------;
; Written by: Michal Adamkiewicz                           ;
;----------------------------------------------------------;
; Date:  19th September 2014                               ;
;----------------------------------------------------------;
; Version:          1.0                                    ;
;----------------------------------------------------------;
; Device:  PIC16F627                                       ;
;----------------------------------------------------------;
; Oscillator: Internal 4 MHz                               ;
;----------------------------------------------------------;
  LIST  P=PIC16F627A ;select device
    ;Tells MPLAB what processor IC is being used
  INCLUDE  c:\program files (x86)\microchip\MPASM Suite\P16F627A.inc
    ;include header file
    ;from default location
    ;tells the MPLAB where to find the files

  __config 0x3F10     ;sets config to; internal  I/O, no watchdog,Power
    ;up timer on, master Reset off,
    ;no brown-out, no LV program, no read protect,
    ;no code protect
;----------------------------------------------------------;
; DEFINE REGISTERS                                         ;
;----------------------------------------------------------;

cblock  0x20
     dig_one ;Three registers store what to be displayed on the 3 characters      dig_two      dig_three      loop ;This stores the current loop time endc



init    
     MOVLW d'07'
     MOVWF CMCON         ;Disable comparators
     BSF STATUS, RP0     ;select bank1 for setup
     BSF PCON, OSCF      ;select 4 MHz
     MOVLW b'01110000'
     MOVWF TRISA         ;set PortA as inputs on designated pins
     MOVLW b'00000000'
     MOVWF TRISB         ;set PortB all outputs
     BCF STATUS, RP0     ;return to bank0 for program operation

     CLRF dig_one        ;clear registers to prevent ghosting
     CLRF dig_two
     CLRF dig_three

main
     BTFSC PORTA,5       ;Test if Programing mode (shift in) was activated
     GOTO prog

     BCF PORTA,2         ;Prepare pin to sink current from diodes
     MOVFW dig_three     ;Move stored values onto PORTB
     MOVWF PORTB
     MOVLW b'00011111'   ;Delay amount established experimentally
     MOVWF loop
     CALL delay          ;Delay ensures display is visible to human eye
     BSF PORTA,2

     BCF PORTA,1         ;Prepare pin to sink current from diodes
     MOVFW dig_two       ;Move stored values onto PORTB
     MOVWF PORTB
     MOVLW b'00011111'   ;Delay amount established experimentally
     MOVWF loop
     CALL delay          ;Delay ensures display is visible to human eye
     BSF PORTA,1

     BCF PORTA,0         ;Prepare pin to sink current from diodes
     MOVFW dig_one       ;Move stored values onto PORTB
     MOVWF PORTB
     MOVLW b'00011111'   ;Delay amount established experimentally
     MOVWF loop
     CALL delay
     BSF PORTA,0         ;Delay ensures display is visible to human eye
     GOTO main           ;Loop back to top

prog                ;Programing mode - PIC accepts serial data
     BTFSS PORTA,5       ;Test if Programing mode disabled
     GOTO main           ;return to multiplexing
     BTFSC PORTA,4       ;Wait for Clock pin to go low
     GOTO prog
wait BTFSS PORTA,4  ;Wait for it to go back to high
     GOTO wait

     RRF dig_three, F    ;Shift register over by one bit
     BCF dig_three,7     ;Ensures register has bit
     BTFSC dig_two,0     ;carried from next register
     BSF dig_three,7
 
     RRF dig_two, F      ;Shift register over by one bit
     BCF dig_two,7       ;Ensures register has bit
     BTFSC dig_one,0     ;carried from next register
     BSF dig_two,7

     RRF dig_one, F      ;Shift register over by one bit
     BCF dig_one,7       ;Take the new bits value from
     BTFSC PORTA,6       ;the Data Pin
     BSF dig_one,7

     BCF PORTA,7         ;Copy last bit to Output pin
     BTFSC dig_three,0   ;for chaining to other modules
     BSF PORTA,7

     GOTO prog           ;Wait for another bit shift

delay DECFSZ loop, F;Count down until zero
      GOTO delay
     RETURN              ;end delay

     END                 ;Opcode need for compilation. PIC should never reach this