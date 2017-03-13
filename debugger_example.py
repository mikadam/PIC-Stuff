'''
File name: debugger_example.py
Author: Michal Adamkiewicz
Date: 2014

example usage of PIC_debugger.py
'''

reg=['temp',
'temp_value_lo',
'temp_value_hi',
'store',
'operator',
'value_hi',
'value_lo',
'old_value_hi',
'old_value_lo',
'out_value_lo',
'out_value_hi',
'update_flag',
'rep_value_lo',
'rep_value_hi',
'porta',
'portb',
]

i=""" 
main; wait for till input loop - multiplexes buttons

 BCF PORTA,0 ; display values

 CLRF PORTB; sets all multiplex columns off

 BTFSC PORTA,7; check if clear button was pressed
 goto no_clear_calc
clear_debounce BTFSS PORTA,7
 goto clear_debounce
 BTFSC update_flag,3; test how many times clear button was pressed
 goto all_clear
 BSF update_flag,3 ;count one button press
 CLRF value_lo ;if yes clear all relevant registers
 CLRF value_hi
 clrf operator
 BCF update_flag,0
 call write_value ;will leave 0 character
 goto no_clear_calc
all_clear; second time button was pressed
 CLRF value_lo ;if yes clear all relevant registers
 CLRF value_hi
 CLRF old_value_lo
 CLRF old_value_hi
 CLRF rep_value_lo
 CLRF rep_value_hi
 clrf operator
 clrf update_flag
 clrf store ;Artificially prepare for write_value
 BSF PORTA,0
 call no_negative; Clear all characters
no_clear_calc; no clear took place

 BSF PORTB,0; Enable left column for multiplexing
 MOVF PORTB,W; copy inputs to temp
 MOVWF temp
 XORLW D'1'
 BTFSC STATUS, Z; check if button was pressed
 goto bone; button not pressed
bpone MOVF PORTB,W ; debouncing loop- wait till unpress
 XORLW D'1'
 BTFSs STATUS, Z; button unpressed?
 goto bpone

 goto button_decode; decode button code funcionality
bone BCF PORTB,0; Disable left column for multiplexing



 BSF PORTB,1; Enable centre column for multiplexing
 MOVF PORTB,W; copy inputs to W
 MOVWF temp
 XORLW D'2'
 BTFSC STATUS, Z; check if button was pressed
 goto btwo; button not pressed
bptwo MOVF PORTB,W; debouncing loop- wait till unpress
 XORLW D'2'
 BTFSs STATUS, Z; button unpressed?
 goto bptwo

 goto button_decode; decode button code funcionality
btwo BCF PORTB,1; Disable centre column for multiplexing



 BSF PORTB,2; Enable centre column for multiplexing
 MOVF PORTB,W; copy inputs to W
 MOVWF temp
 XORLW D'4'
 BTFSC STATUS, Z; check if button was pressed
 goto bthr; button not pressed
bpthr MOVF PORTB,W; debouncing loop- wait till unpress
 XORLW D'4'
 BTFSs STATUS, Z; button unpressed?
 goto bpthr


 goto button_decode; decode button code funcionality
bthr BCF PORTB,2; Disable right column for multiplexing



 goto main ; loop while awaiting input




segment_look ADDWF PCL,F; Appearances of characters on screen
;7-Decimal Point; 6,5,4- Horizontal bars top to bottom; 3,2,1,0-vertial bars TL, TR, BL, BR
 RETLW b'01011111';0
 RETLW b'00000101';1
 RETLW b'01110110';2
 RETLW b'01110101';3
 RETLW b'00101101';4
 RETLW b'01111001';5
 RETLW b'01111011';6
 RETLW b'01000101';7
 RETLW b'01111111';8
 RETLW b'01111101';9
 goto unexpected_error; something unpredicted happened


button_decode; subroutine decodes button codes

 BCF update_flag,3; resets clear counter
 BTFSS update_flag,0; test if previous button press was operator and value needs clearing
 goto no_update

 BCF update_flag,0; clear flag
 CLRF value_hi;clear value
 CLRF value_lo

no_update

 MOVF temp,w
 XORLW D'132'
 BTFSC STATUS,Z; does temp match the buttons code?
 GOTO sub_act

 MOVF temp,w
 XORLW D'130'
 BTFSC STATUS,Z; does temp match the buttons code?
 GOTO div_act

 MOVF temp,w
 XORLW D'129'
 BTFSC STATUS,Z; does temp match the buttons code?
 GOTO mul_act

 MOVF temp,w
 XORLW D'68'
 BTFSC STATUS,Z; does temp match the buttons code?
 GOTO add_act

 MOVF temp,w
 XORLW D'66'
 BTFSC STATUS,Z; does temp match the buttons code?
 GOTO zero_act

 MOVF temp,w
 XORLW D'65'
 BTFSC STATUS,Z; does temp match the buttons code?
 GOTO equ_act

 MOVF temp,w
 XORLW D'36'
 BTFSC STATUS,Z; does temp match the buttons code?
 GOTO one_act

 MOVF temp,w
 XORLW D'34'
 BTFSC STATUS,Z; does temp match the buttons code?
 GOTO two_act

 MOVF temp,w
 XORLW D'33'
 BTFSC STATUS,Z; does temp match the buttons code?
 GOTO three_act

 MOVF temp,w
 XORLW D'20'
 BTFSC STATUS,Z; does temp match the buttons code?
 GOTO four_act

 MOVF temp,w
 XORLW D'18'
 BTFSC STATUS,Z; does temp match the buttons code?
 GOTO five_act

 MOVF temp,w
 XORLW D'17'
 BTFSC STATUS,Z; does temp match the buttons code?
 GOTO six_act

 MOVF temp,w
 XORLW D'12'
 BTFSC STATUS,Z; does temp match the buttons code?
 GOTO seven_act

 MOVF temp,w
 XORLW D'10'
 BTFSC STATUS,Z; does temp match the buttons code?
 GOTO eight_act

 MOVF temp,w
 XORLW D'9'
 BTFSC STATUS,Z; does temp match the buttons code?
 GOTO nine_act

 GOTO multi_key; more than 1 button was pressed, display error code


equ_act

 BTFSC update_flag,5; was equals pressed twice?
 goto equ_act_rep

 BSF update_flag,5; notify that previous button press was equals

 MOVFW value_lo; store current argument for future double press
 movwf rep_value_lo
 MOVFW value_hi
 movwf rep_value_hi

 CALL equ_function; run appropriate calculation
 CALL write_value; push value to display
 goto main; return to main loop

equ_act_rep; Equals double press
 MOVFW value_lo; use value as old argument
 movwf old_value_lo
 MOVFW value_hi
 movwf old_value_hi

 MOVFW rep_value_lo; used rep_value as current argument
 movwf value_lo
 MOVFW rep_value_hi
 movwf value_hi

 CALL equ_function; run appropriate calculation
 CALL write_value; push value to display
 goto main; return to main loop

mul_act
 BTFSC update_flag,5 ;stops in auto-calculating repeated operation
 goto mul_rep_skip
 CALL equ_function ;If yes calculate them
 call write_value ;And display them
mul_rep_skip
 BCF update_flag,5 ;Resets repetition flag

 MOVLW D'2'; remember which operator is active in register
 MOVWF operator
 MOVF value_hi, W ;move value to old_value to be used a previous argument
 MOVWF old_value_hi
 MOVF value_lo, W
 MOVWF old_value_lo
 BSF update_flag,0; clear value next time button is presed
 goto main ;wait for button input

div_act
 BTFSC update_flag,5 ;stops in auto-calculating repeated operation
 goto div_rep_skip
 CALL equ_function ;If yes calculate them
 call write_value ;And display them
div_rep_skip
 BCF update_flag,5 ;Resets repetition flag

 MOVLW D'3'; remember which operator is active in register
 MOVWF operator
 MOVF value_hi, W ;move value to old_value to be used a previous argument
 MOVWF old_value_hi
 MOVF value_lo, W
 MOVWF old_value_lo
 BSF update_flag,0; clear value next time button is presed
 goto main ;wait for button input

sub_act
 BTFSC update_flag,5 ;stops in auto-calculating repeated operation
 goto sub_rep_skip
 CALL equ_function ;If yes calculate them
 call write_value ;And display them
sub_rep_skip
 BCF update_flag,5 ;Resets repetition flag

 MOVLW D'1'; remember which operator is active in register
 MOVWF operator
 MOVF value_hi, W ;move value to old_value to be used a previous argument
 MOVWF old_value_hi
 MOVF value_lo, W
 MOVWF old_value_lo
 BSF update_flag,0; clear value next time button is pressed
 goto main ;wait for button input

add_act
 BTFSC update_flag,5 ;stops in auto-calculating repeated operation
 goto add_rep_skip
 CALL equ_function ;If yes calculate them
 call write_value ;And display them
add_rep_skip
 BCF update_flag,5 ;Resets repetition flag

 MOVLW D'0'; remember which operator is active in register
 MOVWF operator
 MOVF value_hi, W ;move value to old_value to be used a previous argument
 MOVWF old_value_hi
 MOVF value_lo, W
 MOVWF old_value_lo
 BSF update_flag,0; clear value next time button is pressed
 goto main ;wait for button input


one_act MOVLW D'1'; copy digit to store and append to end of value base10
 MOVWF store
 goto shift_digits

two_act MOVLW D'2'; copy digit to store and append to end of value base10
 MOVWF store
 goto shift_digits

three_act MOVLW D'3'; copy digit to store and append to end of value base10
 MOVWF store
 goto shift_digits

four_act MOVLW D'4'; copy digit to store and append to end of value base10
 MOVWF store
 goto shift_digits

five_act MOVLW D'5'; copy digit to store and append to end of value base10
 MOVWF store
 goto shift_digits

six_act MOVLW D'6'; copy digit to store and append to end of value base10
 MOVWF store
 goto shift_digits

seven_act MOVLW D'7'; copy digit to store and append to end of value base10
 MOVWF store
 goto shift_digits

eight_act MOVLW D'8'; copy digit to store and append to end of value base10
 MOVWF store
 goto shift_digits

nine_act MOVLW D'9'; copy digit to store and append to end of value base10
 MOVWF store
 goto shift_digits

zero_act MOVLW D'0'; copy digit to store and append to end of value base10
 MOVWF store
 goto shift_digits



shift_digits

 ;Multiply by 10 by Multiplying by 2,8 and adding

 ;Multiply by 2, by register rotation
 CLRF temp
 RLF value_lo, F;2*
 BCF value_lo,0; ensures zero is shifted in
 BTFSC STATUS, C; uses temp as carry flag holder
 BSF temp,0
 RLF value_hi, F;2*
 BCF value_lo,0; ensures zero is shifted in
 BTFSC STATUS,C; test to ensure number is in 16bit range
 goto overflow
 BTFSC temp,0;carry bit in temporary flag
 BSF value_hi,0;

 ;keep temp_value for later addition
 MOVF value_lo, W
 MOVWF temp_value_lo
 MOVF value_hi, W
 MOVWF temp_value_hi

 ;Multiply by 4, by register rotation
 CLRF temp
 RLF value_lo, F;2*
 BCF value_lo,0; ensures zero is shifted in
 BTFSC STATUS, C; uses temp as carry flag holder
 BSF temp,0
 RLF value_hi, F;2*
 BCF value_hi,0; ensures zero is shifted in
 BTFSC STATUS,C; test to ensure number is in 16bit range
 goto overflow
 BTFSC temp,0;carry bit in temporary flag
 BSF value_hi,0

 ;Multiply by 8, by register rotation
 CLRF temp
 RLF value_lo, F;2*
 BCF value_lo,0; ensures zero is shifted in
 BTFSC STATUS, C; uses temp as carry flag holder
 BSF temp,0
 RLF value_hi, F;2*
 BCF value_hi,0; ensures zero is shifted in
 BTFSC STATUS,C; test to ensure number is in 16bit range
 goto overflow
 BTFSC temp,0;carry bit in temporary flag
 BSF value_hi,0

 ;Add x*2 + x*8 to make 10*x
 MOVF temp_value_lo,W
 ADDWF value_lo,F; Add lowest bits
 BTFSS STATUS,C; Test for carry to higher bits
 goto shift_digits_no_overflow; No carry
 INCF temp_value_hi, F;increment hi from carry
 BTFSC STATUS,Z;if carry out occurred temp_value_hi==0
 goto overflow; display overflow error
shift_digits_no_overflow
 MOVF temp_value_hi,W; Add higher byte
 ADDWF value_hi,F
 BTFSC STATUS,C; test for overflow
 goto overflow

 ;Add digit stored in store
 MOVF store,W; Add store to value
 ADDWF value_lo,F
 MOVLW d'0'; Trick for conditional increment and test without labels
 BTFSC STATUS,C; If carry, add 1 insteed of 0
 MOVLW d'1'
 ADDWF value_hi,F; Add 1 or 0 to execute or not execute carry
 BTFSC STATUS,C; Test for overflow
 goto overflow

 CALL write_value; update displayed value
 goto main; return to main loop

equ_function ;Actually does the calling of calculating functions

 MOVF operator,W; Tests if operator matches + code
 XORLW D'0'
 BTFSC STATUS, Z; If yes go to designated subroutine
 goto actual_add

 MOVF operator,W; Tests if operator matches - code
 XORLW D'1'
 BTFSC STATUS, Z; If yes go to designated subroutine
 goto actual_sub

 MOVF operator,W; Tests if operator matches * code
 XORLW D'2'
 BTFSC STATUS, Z; If yes go to designated subroutine
 goto actual_mul

 MOVF operator,W; Tests if operator matches / code
 XORLW D'3'
 BTFSC STATUS, Z; If yes go to designated subroutine
 goto actual_div

 goto unexpected_error; Abnormal operator code, display error code

back_to_equ_function return; After calculations return to call point

actual_add ;subroutine for adding old_value and value
 MOVF old_value_lo,W; Add low bytes
 ADDWF value_lo,F
 BTFSS STATUS, C; Test if carry out of low byte occurred
 goto add_no_carry
 INCF old_value_hi, F; Execute carry
 BTFSC STATUS,Z; If hi byte carried out it==0
 goto overflow; display overflow code

add_no_carry
 MOVF old_value_hi,W; Add hi bytes
 ADDWF value_hi,F
 BTFSC STATUS, C; Test if overflow occurred
 goto overflow
 goto back_to_equ_function;equ_function for display and further return



actual_mul ;Multiplication via binary algorithm
 movfw value_lo ;keep value in temp_value for manipulation
 movwf temp_value_lo
 movfw value_hi
 movwf temp_value_hi
 clrf value_hi ; clear value - it will be used as output
 clrf value_lo
 clrf temp; temp will count number of bit shifts executed - target 16
 BCF update_flag,2
mul_loop_bit
 incf temp,F; increment loop counter

 BTFSS temp_value_lo,0; Tests lowest bit to see if value is to be added
 goto mul_no_add
 ;Adds old_value to value
 BTFSC update_flag,2
 goto overflow
 MOVF old_value_lo,W; Adds lower bytes
 ADDWF value_lo,F
 BTFSS STATUS, C; Tests for carry
 goto mul_no_carry
 INCF value_hi, F; Executes carry to hi byte
 BTFSC STATUS,Z; Tests if 16bit overflow occurred
 goto overflow
mul_no_carry
 MOVF old_value_hi,W; Added high bytes
 ADDWF value_hi,F
 BTFSC STATUS, C; Tests if 16bit overflow occurred
 goto overflow

mul_no_add
 ;Multiplies old_value by 2
 RLF old_value_lo,F; rotate lower byte
 RLF old_value_hi,F; rotate higher byte - carry handled by STATUS,C
 BTFSC STATUS,C; Test if 16bit overflow occurred
 BSF update_flag,2
 BCF old_value_lo,0; Ensure new bit is==0

 RRF temp_value_hi,F; Prepare new bit for conditional addition
 RRF temp_value_lo,F
 BCF temp_value_hi,7; Ensure new bit is=0-not necessary by keeps code nice

 movfw temp; Testing if loop run 16 times
 xorlw d'16'
 BTFSS STATUS,Z; If no repeat
 goto mul_loop_bit

end_mul goto back_to_equ_function;equ_function for display and further return

actual_sub;

 BCF update_flag,7; clears the minus sign flag

 MOVF value_lo,W; subtracts low bytes
 SUBWF old_value_lo, F
 BTFSC STATUS, C; tests if borrow occurred
 goto sub_no_carry

 DECF old_value_hi, F; borrows bit from high byte
 MOVFw old_value_hi
 XORLW d'255'; Tests if borrow there was something to borrow from
 BTFSC STATUS,Z
 BSF update_flag,7; If no then set minus sign

sub_no_carry
 MOVF value_hi,W; Subtracts high bytes
 SUBWF old_value_hi, F
 BTFSS STATUS, C; If borrow occurred - nothing to borrow from
 BSF update_flag,7; Set minus sign

 BTFSS update_flag,7; Test if numbers need to be adjusted for minus
 goto sub_no_neg

 ;Execute two's compliment conversion if minus is present
 movfw old_value_lo; XOR with 11111111 == flip all bits
 XORLW d'255'; this is twos compliment notation
 movwf old_value_lo

 movfw old_value_hi; XOR with 11111111 == flip all bits
 XORLW d'255'; this is twos compliment notation
 movwf old_value_hi

 INCF old_value_lo,F; Compensate for zero not being negative by adding 1
 BTFSC STATUS,Z
 INCF old_value_hi

sub_no_neg
 MOVF old_value_lo, W; Store result in value so it will be displayed
 MOVWF value_lo
 MOVF old_value_hi, W
 MOVWF value_hi

sub_end goto back_to_equ_function;equ_function for display and further return


actual_div ;Division via repeated subtraction

 MOVFW value_lo ;Test if lower byte of right operand is ==0
 XORLW d'0'
 BTFSS STATUS,Z
 goto no_div_zero
 MOVFW value_hi ;Test if higher byte of right operand is ==0
 XORLW d'0'
 BTFSC STATUS,Z
 goto unexpected_error ;Output an Error as division by 0 is undefined
 
no_div_zero
 movfw value_lo; copy value to temp_value for manipulation
 movwf temp_value_lo
 movfw value_hi
 movwf temp_value_hi

 clrf value_lo ;value will be used as output
 clrf value_hi

div_sub_loop
 movfw temp_value_lo ;Subtract lower bytes
 SUBWF old_value_lo,F
 BTFSC STATUS,C; Check for a borrow from high byte
 goto div_no_borrow

 DECF old_value_hi,F ;Borrow from high byte
 movfw old_value_hi
 XORLW d'255' ;Test if high byte went negative
 BTFSC STATUS,Z
 goto div_sub_end ;If yes goto ending

div_no_borrow
 movfw temp_value_hi ;Subtract higher bytes
 SUBWF old_value_hi,F
 BTFSS STATUS,C ;Check if result went negative - borrow from non existent byte
 goto div_sub_end

 INCF value_lo,F ;Increment counter on number of successful subtractions
 BTFSC STATUS,Z
 INCF value_hi,F ;Carry into high byte
 goto div_sub_loop

div_sub_end
 movfw temp_value_lo ;fix offset in low byte for one too many subtractions
 addwf old_value_lo,F
 BTFSC STATUS,C ;carry addition to high byte
 INCF old_value_hi,F
 movfw temp_value_hi ;fix offset in high byte for one too many subtractions
 addwf old_value_hi,F

 BCF update_flag,4 ;Remainder flag=0
 movfw old_value_lo 
 XORLW d'0' ;Test if division was exact in low byte
 BTFSS STATUS,Z
 BSF update_flag,4 ;Remainder flag=1

 movfw old_value_hi
 XORLW d'0' ;Test if division was exact in high byte
 BTFSS STATUS,Z
 BSF update_flag,4
end_div goto back_to_equ_function


write_value; writes value_hi,value_lo to the display
 BSF PORTA,0 ; prepare display modules for data transfer

 MOVF value_hi, W ;use temp_value to prevent changing value as it will be used in calculations
 MOVWF temp_value_hi
 MOVF value_lo, W
 MOVWF temp_value_lo
 CLRF store; store will count the number of characters already displayed target-6

 BTFSS update_flag,4; Was division result exact or with a remainder?
 goto no_remainder

 INCF store,F; Increment number of characters displayed
 MOVLW b'10100010' ; Display code for reminder -looks like a r.
 MOVWF out_value_lo
 CLRF temp; Temp will count number of bits shifted - target 8 (7 segments + decimal point)

shift_loop_remainder
 INCF temp,F; Increment counter of bits shifted out
 BCF PORTA,2 ; Set data pin to 0
 BTFSC out_value_lo,0; If data pin is meant to be 1
 BSF PORTA,2; make it 1

 BCF PORTA,1 ; raise the clock - shift value
 RRF out_value_lo,F; prepare to shift out next bit; extra function - delay so display module can respond
 BSF PORTA,1 ; fall the clock

 MOVFW temp; Tests if all 8 bits were shifted out
 XORLW d'8'
 BTFSS STATUS,Z; If yes continue to next step
 goto shift_loop_remainder

no_remainder
 CLRF out_value_lo; out_value will store integer part of division by 10
 CLRF out_value_hi; temp will store remainder of division by 10

write_loop
 INCF out_value_lo, F;counts the number of 10s subtracted
 BTFSC STATUS,Z; carry when counting 10s
 INCF out_value_hi, F; Division ensures no 16bit overflow

 MOVLW D'10'
 SUBWF temp_value_lo,F; actually subtracts the 10 from temp
 BTFSC STATUS,C; checks if a borrow occurred
 goto write_loop; Subtract another 10
 MOVF temp_value_hi,W
 BTFSC STATUS,Z; If all temp_value is <0 division finishes. Must be zero at one point as 10<255
 goto write_digit_calc; Process done, move to next step
 DECF temp_value_hi,F ;borrow from hi byte
 goto write_loop ;Subtract another 10

write_digit_calc
 MOVLW D'10' ; Compensates for 10 subtracted to make temp_value negative
 ADDWF temp_value_lo,F

 DECF out_value_lo,F; Compensates counter for added 10
 MOVFW out_value_lo
 XORLW d'255' ;check borrow from hi byte
 BTFSC STATUS,Z
 DECF out_value_hi,F ;Execute borrow from high

 INCF store,F ;Tracks number of characters written to display

 MOVFW temp_value_lo ;Look up code for calculated character
 CALL segment_look ;Access table
 MOVWF temp_value_lo

 CLRF temp; Temp will count number of bits shifted - target 8 (7 segments + decimal point)

shift_loop_main
 INCF temp,F; Count number of bits shifted out

 BCF PORTA,2 ;Copy temp_value_lo,0 into PORTA,2
 BTFSC temp_value_lo,0
 BSF PORTA,2

 BCF PORTA,1 ; Raise the clock - shift value
 RRF temp_value_lo,F ;Prepare next bit in temp_value_lo while waiting for display value
 BSF PORTA,1 ; Fall the clock

 MOVFW temp; Test if 8 bits already shifted out
 XORLW d'8'
 BTFSS STATUS,Z
 goto shift_loop_main; If no shift out next bit

 BCF update_flag,6; Clean bit for temporary bit flag
 MOVF out_value_hi,W
 MOVWF temp_value_hi; Copy low byte of integer part of division by 10 for next division
 BTFSC STATUS, Z ;If ==0 to copy remember in temporary flag
 BSF update_flag,6

 MOVF out_value_lo, W
 MOVWF temp_value_lo; Copy hi byte of integer part of division by 10 for next division
 BTFSS STATUS, Z ;Test if ==0 for finishing condition
 goto no_remainder ;Run division loop again
 BTFSS update_flag,6 ;If both hi and low byte ==0 continue to next section
 goto no_remainder ;Run division loop again

 BTFSS update_flag,7 ;If minus sign flag clear then
 goto no_negative; Don't display a minus

 INCF store,F ;Count number of characters writen
 MOVLW b'00100000' ;Display code for a minus sign -
 MOVWF temp_value_lo

 CLRF temp; Temp will count number of bits shifted - target 8 (7 segments + decimal point)

shift_loop_neg
 INCF temp,F; Count number of bits shifted out

 BCF PORTA,2 ;Copy temp_value_lo,0 into PORTA,2
 BTFSC temp_value_lo,0
 BSF PORTA,2

 BCF PORTA,1 ; Raise the clock - shift value
 RRF temp_value_lo,F ;Prepare next bit in temp_value_lo while waiting for display value
 BSF PORTA,1 ; Fall the clock

 MOVFW temp; Test if 8 bits already shifted out
 XORLW d'8'
 BTFSS STATUS,Z
 goto shift_loop_neg; If no shift out next bit

no_negative

 BCF update_flag,7 ;Clear minus sign flag; value=abs(value)
 BCF update_flag,4 ;Clear reminder flag; value =floor(value)

 MOVFW store ;Tests if all 6 characters are written to
 XORLW d'6'
 BTFSC STATUS,Z
 return ;If yes: return to call point

 INCF store,F; Count number of characters written to display

 BCF PORTA,2 ;Shift outs b'00000000' - an empty character
 CLRF temp; Temp will count number of bits shifted - target 8 (7 segments + decimal point)

shift_loop_null
 INCF temp,F ;Count number of bits shifted out
 BCF PORTA,1 ; Raise the clock - shift value
 ;Delay not need as display PIC runs with a faster clock speed
 BSF PORTA,1 ; Fall the clock

 MOVFW temp; Test if 8 bits already shifted out
 XORLW d'8'
 BTFSS STATUS,Z
 goto shift_loop_null; If no shift out next bit

 goto no_negative

;Subroutines for handling exceptions in the code

multi_key ;triggers when two buttons are pressed simultaneously and PIC can't distinguish them
 BSF PORTA,0; Prepare display modules for data

 MOVLW b'10001111'; multi key press code. Two Parallel line followed by a period
 MOVWF temp_value_lo
 CLRF temp; Temp will count number of bits shifted - target 8 (7 segments + decimal point)

shift_loop_multi
 INCF temp,F ;Count number of bits shifted out

 BCF PORTA,2; Copy temp_value_lo,0 into PORTA,2
 BTFSC temp_value_lo,0
 BSF PORTA,2

 BCF PORTA,1 ; Raise the clock - shift value
 RRF temp_value_lo,F; Move to next bit in temp_value_lo
 BSF PORTA,1 ; Fall the clock

 MOVFW temp; Test if 8 bits already shifted out
 XORLW d'8'
 BTFSS STATUS,Z
 goto shift_loop_multi; If no shift out next bit

 goto main; resume normal operation


overflow ;Indicates program encountered a 16bit overflow while preforming large calculation
 BSF PORTA,0; Prepare display modules for data

 MOVLW b'11101010'; Display a F. symbol to indicate operation Failed
 MOVWF temp_value_lo
 CLRF temp; Temp will count number of bits shifted - target 8 (7 segments + decimal point)

shift_loop_overflow
 INCF temp,F ;Count number of bits shifted out

 BCF PORTA,2; Copy temp_value_lo,0 into PORTA,2
 BTFSC temp_value_lo,0
 BSF PORTA,2

 BCF PORTA,1 ; Raise the clock - shift value
 RRF temp_value_lo,F; Move to next bit in temp_value_lo
 BSF PORTA,1 ; Fall the clock

 MOVFW temp; Test if 8 bits already shifted out
 XORLW d'8'
 BTFSS STATUS,Z
 goto shift_loop_overflow; If no shift out next bit

 goto main; resume normal operation


unexpected_error ;Signifies a unclassified error occurred. EG division by 0
 BSF PORTA,0; Prepare display modules for data

 MOVLW b'11111010'; Display a E. symbol to indicate and Error
 MOVWF temp_value_lo
 CLRF temp; Temp will count number of bits shifted - target 8 (7 segments + decimal point)

shift_loop_error
 INCF temp,F ;Count number of bits shifted out

 BCF PORTA,2; Copy temp_value_lo,0 into PORTA,2
 BTFSC temp_value_lo,0
 BSF PORTA,2

 BCF PORTA,1 ; Raise the clock - shift value
 RRF temp_value_lo,F; Move to next bit in temp_value_lo
 BSF PORTA,1 ; Fall the clock

 MOVFW temp; Test if 8 bits already shifted out
 XORLW d'8'
 BTFSS STATUS,Z
 goto shift_loop_error; If no shift out next bit

 goto main; resume normal operation

 END; Opcode need for compilation. PIC should never reach this
    """

if __name__=='__main__':
	from PIC_Debugger import run_PIC
	run_PIC(reg,i)

