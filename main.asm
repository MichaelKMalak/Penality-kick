;;=============================================================================;;
;;                 Multiplayer Penality shooting game                          ;;
;;                 Tested on DOSBox and emu8086                                ;;
;;=============================================================================;;

include macros.inc

.MODEL SMALL
.STACK 64    
.DATA  
 	GameStart db '              ====================================================',0ah,0dh
	db '             ||                                                  ||',0ah,0dh                                        
	db '             ||       *    Penality Shooting Game      *         ||',0ah,0dh
	db '             ||                                                  ||',0ah,0dh
	db '             ||--------------------------------------------------||',0ah,0dh
	db '             ||                                                  ||',0ah,0dh
	db '             ||                                                  ||',0ah,0dh
	db '             ||                                                  ||',0ah,0dh          
	db '             ||     Use up and down key to move goalkeeper bar   ||',0ah,0dh
	db '             ||          and enter button to shoot               ||',0ah,0dh
	db '             ||                                                  ||',0ah,0dh
	db '             ||            Two players switch every turn         ||',0ah,0dh
	db '             ||            Each player Shoots 5 times            ||',0ah,0dh
	db '             ||                                                  ||',0ah,0dh
	db '             ||            Press 2 to start playing              ||',0ah,0dh 
	db '             ||            Press 1 to start chatting             ||',0ah,0dh
	db '             ||                                                  ||',0ah,0dh
	db '             ||            Press ESC to Exit                     ||',0ah,0dh
	db '              ====================================================',0ah,0dh
	db '$',0ah,0dh
	
	GameEnd db '        		 *    GAMEOVER      *  		       ',0ah,0dh
	db '              ====================================================',0ah,0dh     
	db '             ||            Press 1 to restart the game           ||',0ah,0dh
	db '             ||            Press ESC to Exit                     ||',0ah,0dh
	db '              ====================================================',0ah,0dh
	db '$',0ah,0dh
	
	
 ask_p1_name db 'Player 1 Name: ','$'
 ask_p2_name db 'Player 2 Name: ','$'
 score_msg db "'s score:$"
 p1_win db 'Player 1 Wins','$'
 p2_win db 'Player 2 Wins','$'
 Draw   db 'It is A Draw','$'  
 Turn1  db 'Player 1 Turn','$'
 Turn2  db 'Player 2 Turn','$'
 Exiting   db 'Exiting The Game','$'  
 Goal1 db 'Player 1 Scored A Goal','$'
 Goal2 db 'Player 2 Scored A Goal','$'
 Save db 'The Keeper Saved The Ball','$'
 Outside db 'The Ball is Out !','$'
 FinalString db 30 dup('$')
 p1_name db 15, ?,  15 dup('$')
 p2_name db 15, ?,  15 dup('$')
 
 RBCenterU   db 7
 RBCenterD   db 8
 p1_score db 0  			;Intially player 1 score is 0
 p2_score db 0  			;Intially player 2 score is 0
 GoalDim db 71, 3, 75, 11 	        ;X1,Y1, X2,Y2
 current_player db 1                        
 Hr_Line_Color db 0c0h
 Goal_Color db 0c0h
 Goalkeeper_Color db 20h
 Player_Color db 02h
 Ball_Color db 0ch
 Coordinate_BallCurve dw  0205h , 6     
 
 temp dw ? 
 Shoot_Key  db  1Ch		 	;Enter scan code   
 Total_Shoots db 1       	        ;inc until 10 (5 for each player)
 GameOver db 0     
 
 ver db ?         			;Last Vertical Ball Postion
 hor db 72        			;Last Horizontal Ball Postion
 
;==================================================
.CODE   
MAIN    PROC   
    mov ax, @DATA
    mov ds, ax
    
  RestartProc:				;Called to restart the game
    mov ax, 0
    
    CALL ResetAll			;Resets positions and player names
    CALL GetNames 			;Gets player names
    CALL MainMenu   	         	;Displays the main menu
    CALL DrawInterface   	        ;Draws the intial game interface
    CALL DrawBottomSection	        ;Draws the in-game chat
    
    ;Wait for key from keyboard to shoot
    mov Ah,03h
    int 10h    
    mov Bh, 00
    mov Cx, 01  
    mov ah,1
    mov cx,2b0bh
    int 10h 

 CHECK:
    call Delay				
    call Delay 
    Call Write_P1_P2		;Draw + sign and p1 or p2 based on the current_player byte
 
    mov ah, 2h                  ;Setting the Status Bar
    mov bh, 0      
    mov dl, 0 
    mov dh, 14
    int 10h
     
    mov ah,9                    ;Clear The Status Bar
    mov bh,0 
    mov al,32                   ;Printing Space
    mov cx,80                   ;80 Times to cover all Horizontal Line
    mov bl ,0AFh                ;Set Status Bar Color -> white on Green Background
    int 10h
    
    
    cmp current_player,1	;check the current player
    JE p1
    
    jmp p2
  
        p1: 
          mov ah, 2h
          mov bh, 0      
          mov dl, 0 
          mov dh, 14
          int 10h
          
          mov ah, 9h
	  mov dx, offset Turn1
	  int 21h
		  
          jmp Cont
    
        p2: 
          mov ah, 2h
          mov bh, 0      
          mov dl, 0 
          mov dh, 14
          int 10h
          
          mov ah, 9h
          mov dx, offset Turn2
          int 21h
		  
          jmp Cont  
    
    Cont:
        ;Get Curve co
        mov bl,0 
        mov cx,Coordinate_BallCurve[2] ; radius 
          
        
        
    FirstHalfCycle:  
                      
       mov ah,2
       mov dx, Coordinate_BallCurve[0]
       add dl,bl                ;inc X Coordinate --> right  
       
       add dh,bl                ;inc Y Coordinate --> down
       int 10h      
       push dx
       
       ;Draw 'o'
       Call DrawBall 
       
       Call Delay
       
       pop dx
       mov si,dx    
       ;To clear the last 'o'       
       mov ah,2
       mov dx,si
       int 10h      
       ;Draw space
       mov ah,2
       mov dl,' '
       int 21h  
       
       inc bl
         
       MOV AH,1             	  ;Get key pressed
       INT 16H  
   
       JNE Key_Pressed1
       JMP NO_Key_Pressed1       
                     
    Key_Pressed1:   
       MOV AH,0             	  ;clear used scan code from buffer
       INT 16H  
   
       cmp ah,Shoot_Key   
       JE Shoot     
       cmp ah,1H                  ;Esc to terminate the Game
       JE Exit_1
       push ax
       push bx
       push cx
       push dx
         
       Call Move 
        
       pop dx
       pop cx
       pop bx
       pop ax
       
    NO_Key_Pressed1:    
                          
    loop FirstHalfCycle

  
       mov bl,1
       mov cx,Coordinate_BallCurve[2] ; radius 
       dec cx 
       mov temp,si               
    
    SecondHalfCycle:   
       mov ah,2
       mov dx, temp
       sub dl,bl       			;dec X Coordinate -->left
       dec dl
       add dh,bl        		;inc Y Coordinate -->down
       int 10h    
       
       push dx
       
       ;Draw 'o'
       Call DrawBall 
       
       Call Delay
       
       pop dx
       mov si,dx
       
       ;To clear the last 'o'       
        mov ah,2
        mov dx,si
        int 10h      
        
        ;Draw space
        mov ah,2
        mov dl,' '
        int 21h  
            
        inc bl     
        
        MOV AH,1              		;Get key pressed
        INT 16H  
        
        JNE Key_Pressed2
        JMP NO_Key_Pressed2               
   
    Key_Pressed2:   
        MOV AH,0             		;clear used scan code from buffer
        INT 16H  
        cmp ah,Shoot_Key   
        JE Shoot     
        cmp ah,1H                  	;Esc to terminate the Game
        JE Exit
		
        push ax
        push bx
        push cx
        push dx
           
        Call Move 
        
        pop dx
        pop cx
        pop bx
        pop ax
CHECK_1:
JMP CHECK               
    NO_Key_Pressed2:    
                     
    loop SecondHalfCycle
                   
CMP ah,Shoot_Key 

JNE CHECK_1   

Exit_1:
JMP Exit

;si contains co-ordinate of start of shooting line

Shoot:    

        mov cx,12 ;Horizontal shoot  
        mov bl,1   
        mov temp,si
    Horizontal:  
           
        MOV AH,1              ;Get key pressed
        INT 16H  
       
        JNE Key_Pressed3
        JMP NO_Key_Pressed3               
       
    Key_Pressed3:   
        MOV AH,0          	 ;clear used scan code from buffer
        INT 16H      
        
        push ax
        push bx
        push cx
        push dx
       
        Call Move 
       
        pop dx
        pop cx
        pop bx
        pop ax
        
    NO_Key_Pressed3:
        mov ah,2
        mov dx,temp
        add dl,bl    		 ;inc X horizonatally -->right
        int 10h   
        
        push dx
        
        ;Draw
        Call DrawBall
        
        Call Delay
        
        pop dx
        
        mov si,dx    
        ;To clear the last 'o'       
        mov ah,2
        mov dx,si
        int 10h      
        
        ;Draw space
        mov ah,2
        mov dl,' '
        int 21h  
     
        add bl ,3 
        
	mov ah,3h			      ;check the ball current position
	mov bh,0h
	int 10h
         
	cmp dl,68                             ;X is Greater than or Equal the Goal Line
	JAE GoToCheckScores  		      ;It Reached the Line
		
    Loop Horizontal         
	
    GoToCheckScores:
	Call CheckScores
	cmp GameOver, 0
    	JE CHECK_1 
	
    call delay
    call GameOverMenu				;When game is over, display Gameover menu
    
    jmp RestartProc				;If didn't call Exit, jump to Restart the game
	
Exit: 
    ClearScreen
    mov ah,4CH
    int 21H    


MAIN        ENDP 
   
;==================================================
; Move procedure moves the goal keeper and is called when a key is pressed
; The goalkeeper is a two colored charachters on top of each other
; Move procedure directs the action depending on the key press
; possible actions are only up or down or no action

Move PROC
   RightU:
    CMP Ah,48h
    JNE RightD    						; If the up arrow is not pressed jump to RightD
    Call RightUp						; If pressed, Call RightUp Procedure
   RightD:    
    CMP Ah,50h			
    JNE ENDD            				; If the down arrow is not pressed jump to ENDD
    CALL RightDown						; If pressed, Call RightDown
   ENDD:ret
Move ENDP    
;--------------------------
RightUp PROC
   
    cmp RBCenterU,0						;Check if the goalkeeper is at the very top of screen
    JE ENDRU							;if yes, end the procedure (no action)
     
    dec RBCenterU						;Decrement Column of the upper charachter so you go up 
    print RBCenterU,70,Goalkeeper_Color	;Print the new coordinates
    Delete RBCenterD,70 				;delete the other (third) charachter at the bottom
    DEC RBCenterD						;Decrement Column of the lower charachter but don't print
   
    ENDRU:ret
RightUp ENDP
;--------------------------
RightDown PROC
    cmp RBCenterD,13					;Check if the goalkeeper is at the top of the chat screen
    JE ENDRD							;if yes, end the procedure (no action)
    
    inc RBCenterD						;increment Column of the lower charachter so you go down 
    print RBCenterD,70,Goalkeeper_Color ;Print the new coordinates
    Delete RBCenterU,70 				;delete the other (third) charachter at the top
    INC RBCenterU						;increment Column of the upper charachter but don't print
   
    ENDRD:ret
RightDown ENDP

;==================================================
GetNames    PROC  
    push ax
    push bx
    push cx
    push dx
    push ds 
    
    mov ah, 0
    mov al, 3
    int 10h
    
    ClearScreen
    
	ask_p1_name_loop:
		;Clear the line
		Call ClearLine
		;Show the user a message to enter player 1 name
		mov ah, 9
		mov dx, offset ask_p1_name
		int 21h
		
		;Receive player 1 name from the user
		mov ah, 0Ah
		mov dx, offset p1_name
		int 21h
		
	cmp p1_name[1], 0	;Check that input is not empty
	jz ask_p1_name_loop
	
	;Checks on the first letter to ensure that it's either a capital letter or a small letter
	cmp p1_name[2], 40h
	jbe ask_p1_name_loop
	cmp p1_name[2], 7Bh
	jae ask_p1_name_loop
	cmp p1_name[2], 5Bh
	jae p1_check_in_range
	
	jmp Continue_To_Player_2
	
	p1_check_in_range:
	cmp p1_name[2], 60h
	jbe ask_p1_name_loop
	
	Continue_To_Player_2:
	
	;Move to a new line
    mov ah, 2
    mov dl, 10
    int 21h
    
	;Go to the beginning of the line
    mov ah, 2
    mov dl, 13
    int 21h
    
	ask_p2_name_loop:
		;Clear the line
		Call ClearLine
		;Show the user a message to enter player 2 name
		mov ah, 9
		mov dx, offset ask_p2_name
		int 21h
		
		;Receive player 2 name from the user
		mov ah, 0Ah
		mov dx, offset p2_name
		int 21h
		
	cmp p2_name[1], 0	;Check that input is not empty
	jz ask_p2_name_loop
	
	;Checks on the first letter to ensure that it's either a capital letter or a small letter
	cmp p2_name[2], 40h
	jbe ask_p2_name_loop
	cmp p2_name[2], 7Bh
	jae ask_p2_name_loop
	cmp p2_name[2], 5Bh
	jae p2_check_in_range
	
	jmp END_GetNames
	
	p2_check_in_range:
	cmp p2_name[2], 60h
	jbe ask_p2_name_loop
	
	END_GetNames:
      
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax  
    
    RET
     
GetNames    ENDP
;==================================================

DrawInterface   PROC
    
    push ax
    push bx
    push cx
    push dx
    push ds
    
    ClearScreen	;Clear all the screen    
    
    mov ax, 0
    mov bx, 0
    mov cx, 0
    mov dx, 0
        
    Call Write_P1_P2	;Draw + sign and p1 or p2 based on the current_player byte
    
    ;Draw colored spaces indicating the goal
    
        ;Draw the goal top
        
        mov cl, GoalDim[0]
        
        loop0:
			push cx
			Print GoalDim[1], cl, Goal_Color
			pop cx
                
			inc cl  
			mov bl, GoalDim[2]
			inc bl
			cmp cl, bl 
        JNE loop0 
        
        
        ;Draw the back of the goal
        mov cl, GoalDim[1]
        inc cl 
        loop1:
            push cx   
            Print cl, GoalDim[2], Goal_Color   
            pop cx
            inc cl
            cmp cl, GoalDim[3]
        JNE loop1
        
        
        ;Draw the bottom of the goal
        mov cl, GoalDim[2]
        
        loop2:
			push cx
			Print GoalDim[3], cl, Goal_Color
			pop cx
			 
			dec cl
			mov bl, GoalDim[0]
			dec bl
			cmp cl, bl        
        JNE loop2 
		
		;Draw the goal keeper in the intial position
		Print RBCenterU,70,Goalkeeper_Color     
		Print RBCenterD,70,Goalkeeper_Color 
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax 
    
    RET                
                    
DrawInterface   ENDP
;==================================================

ClearLine	Proc
	
	push ax
	push cx
	push dx
	
	;Go to the line beginning
	
	mov ah, 2
	mov dl, 13
	int 21h
	;Draw 79 spaces
	mov cx, 79
	Clear_Line_loop:
		mov ah, 2
		mov dl, ' '
		int 21h
	loop Clear_Line_loop
	
	;Go to the line beginning
	
	mov ah, 2
	mov dl, 13
	int 21h
	
	pop dx
	pop cx
	pop ax
	
	RET
ClearLine	ENDP
;==================================================

DrawBottomSection PROC
    push ax
    push bx
    push cx
    push dx
    push ds
	
	mov ax, 0
	mov bx, 0
	mov cx, 0
	mov dx, 0

;Draw the last section in the screen
    
    ;Draw Horizontal Line
    
    loop3:
        push cx
        Print 15, cl, Hr_Line_Color      
        pop cx      
        inc cl
        cmp cl, 80
    jne loop3
    
    ;Move to the next line
    
    mov ah, 2      
    mov dl, 0 ;Move to position X=0
    mov dh, 16 ;Move to position Y=16
    int 10h 
    
    
    ;Writing players' names and scores
    
    ;Player 1 Info  
    
    push bx
    push cx

    mov cx, 0 
    mov bx, 2
    mov cl, p1_name[1]

    p1_loop:
		mov ah,2
		mov dl, p1_name[bx]
		int 21h
		inc bx
    loop p1_loop
   
    pop cx
    pop bx
	
	;Printing a colon between name and score
    mov ah, 2
    mov bx, 0
    mov dl, ':'
    int 21h
    
    ;Print player 1 score
    mov bl, p1_score
    add bl, '0'
    mov ah, 2
    mov dl, bl
    int 21h
    
    ;Set cursor to write player 2 info
    mov ah, 2      
    mov dh, 16 ;Move to position Y=16 
    mov dl, 78 ;put in dl (80-2), 2=> the colon and the score
    sub dl, p2_name[1]	;Subtract the actual player name length
	;the resulted X in dl will align both the name and the score to the right most of the screen
    int 10h
    
    ;Player 2 Info
   
    push bx
    push cx 
	
    mov bx, 2
    mov cx, 0
    mov cl, p2_name[1]
    
	p2_loop:
		mov ah,2
		mov dl, p2_name[bx] 
		int 21h
		inc bx
    loop p2_loop
    
	pop cx
    pop bx

   	;Printing a colon between name and score
    mov ah, 2
    mov dl, ':'
    int 21h
    
    ;Print player 2 score	
    mov bl, p2_score
    add bl, '0'
    mov ah, 2
    mov dl, bl
    int 21h  
    
        
    ;Draw Horizontal Line
    mov cx, 0
    
    loop6:               
        push cx
        Print 17, cl, Hr_Line_Color      
        pop cx
        inc cl
        cmp cl, 80
    jne loop6
    
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax 
    
    RET

DrawBottomSection ENDP 
;==================================================

Delay  PROC
    
    push ax
    push bx
    push cx
    push dx
    push ds
    
    ;80 ms delay        
    mov cx, 1h
    mov dx, 3880h
    mov ah, 86h
    int 15h
    
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax
    
    RET
Delay  ENDP
;==================================================

DrawBall PROC

	push ax
	push bx
	push cx
	push dx
	
	mov bh, 0
	mov dx, 0
	mov al, 'o'
	mov bl, Ball_Color
	mov ah, 9
	mov cx, 1
	int 10h
	
	pop dx
	pop cx
	pop bx
	pop ax

	RET
DrawBall ENDP
;==================================================

Write_P1_P2 PROC
	push ax
	push bx
	push cx
	push dx
	
	;Draw + (Player)
    mov ah, 2
    mov dl, 5 ;Move to position X=5
    mov dh, 7 ;Move to position Y=7
    int 10h  
    
    mov bh, 0
    mov dx, 0
	mov al, '+'
	mov bl, Player_Color
    mov ah, 9
    mov cx, 1
    int 10h
    
    ;Write P1 or P2 under the + sign
    mov al, 0
    mov ah, 2
    mov dl, 5 ;Move to position X=5
    mov dh, 8 ;Move to position Y=8
    int 10h 
    
    mov bh, 0
    mov dx, 0
	mov al, 'P'
	mov bl, Player_Color
    mov ah, 9
    mov cx, 1
    int 10h
                            
    cmp current_player, 1  ;Check if the current player is player1
    je write_1
    
    ;if player 2 => Print 2
	mov al, 0
    mov ah, 2
    mov dl, 6 ;Move to position X=6
    mov dh, 8 ;Move to position Y=8
    int 10h
	
    mov bh, 0
    mov dx, 0
	mov al, '2'
	mov bl, Player_Color
    mov ah, 9
    mov cx, 1
    int 10h
    
    jmp Exit_Write_P1_P2
    
    write_1:
	;if player 1 => Print 1
	mov al, 0
    mov ah, 2
    mov dl, 6 ;Move to position X=6
    mov dh, 8 ;Move to position Y=8
    int 10h
	
    mov bh, 0
    mov dx, 0
	mov al, '1'
	mov bl, Player_Color
    mov ah, 9
    mov cx, 1
    int 10h

	Exit_Write_P1_P2:
	pop dx
	pop cx
	pop bx
	pop ax
	
	RET
Write_P1_P2 ENDP
;==================================================

ChangeScore PROC   ;taken from write in fifth of screen ;Update the Score Every Shoot
    
	push ax
	push bx
	push cx
	push dx
	
    mov ah, 2
    mov bh,0      
    mov dl, 0 			;Move to position X=0
    mov dh, 16 			;Move to position Y=16
    int 10h 
    
    
    ;Writing players' names and scores
    
    ;Player 1 Info  
    
   ;Move to write player 2 info
    mov ah, 2
    mov bh,0      
    mov dh, 16 			;Move to position Y=16 
    mov dl, p1_name[1] 
    inc dl
    int 10h
	
    mov bl, p1_score
    add bl, '0'
    mov ah, 2
    mov dl, bl
    int 21h
    
    ;Move to write player 2 info
    mov ah, 2
    mov bh,0      
    mov dh, 16 			;Move to position Y=16 
    mov dl, 79 
    int 10h
               
    mov bl, p2_score
    add bl, '0'
    mov ah, 2
    mov dl, bl
    int 21h  
    
	pop dx
	pop cx
	pop bx
	pop ax
	
    RET
ChangeScore ENDP 
;==================================================

CheckScores PROC			;Check The Ball Position
	mov AX,DS
    	mov ES,AX   
	push ax
	push bx
	push cx
	push dx
	
	Passed: 
        ;Check if it is saved
        cmp dh,RBCenterU
        JE Saved
        
        cmp dh,RBCenterD
        JE Saved
        
    
        ;Between the Goal's Region 
        cmp dh,3			;upper outside
        JBE Outline
        cmp dh,11			;lower outside
        JAE Outline
        
        mov ver ,dh
        
        JMP Goal
        
    Outline:
        mov ah, 2h
        mov bh, 0      
        mov dl, 0 
        mov dh, 14
        int 10h
          
        mov ah, 9h
        mov dx, offset Outside
        int 21h
        JMP Switch   
        
    Saved: 
        mov ah, 2h
        mov bh, 0      
        mov dl, 0 
        mov dh, 14
        int 10h
          
        mov ah, 9h
        mov dx, offset Save
        int 21h
        JMP Switch
          
    
    Goal: 
      mov ah, 2h
      mov bh, 0      
      mov dl, hor 
      int 10h  
      
      Call DrawBall
    
      cmp current_player,1
      JE incp1
      
      cmp current_player,2 
      JE incp2
    
    
    ;Incrementing The Scores
    incp1: 				;Player 1 Score
      mov ah, 2h
      mov bh, 0      
      mov dl, 0 
      mov dh, 14
      int 10h
          
      mov ah, 9h
      mov dx, offset Goal1
      int 21h
      inc p1_score      
      jmp Switch  
      
    incp2:				;Player 2 Score
      mov ah, 2h
      mov bh, 0      
      mov dl, 0 
      mov dh, 14
      int 10h
          
      mov ah, 9h
      mov dx, offset Goal2
      int 21h
      inc p2_score      
      jmp Switch 
      
      
      
    Switch:				;Switching the Player for the next shooting 
       call Delay
       ;Switch the players
       cmp current_player ,1
       JE Setp1
                            
       cmp current_player ,2                             
       JE Setp2
       
     
    Setp1:
          mov current_player ,2 
          
          jmp Final
    
    Setp2:
          mov current_player ,1 

          jmp Final  
    
      
   Final: 
      Delete ver , hor
      call Delay                     
      call ChangeScore  
      
      cmp Total_Shoots,10		;check if each player done the 5 shoots
      JE Result
        
      call Delay  
      inc Total_Shoots
      call Delay          
                
      jmp Exit_CheckScores         
                                          
    
    Result:
        mov ah, 0
        mov al, 3h
        int 10h 
    
        mov dh,p1_score
        mov dl,p2_score
	
        cmp dl,dh			;compare the scores and print the winner
        JB P1Win			;Player 1 Won
        
        cmp dl,dh
        JA P2Win			;Player 2 Won
        
        
        JMP Draws			;It is a Draw
    
    
    P1Win:
        mov ah,2
        mov dh,12
        mov dl,32
        int 10h
    
        mov ah, 9
        mov dx, offset p1_win
        int 21h
		
	mov GameOver, 1
		
	CLD    
	mov si,offset p1_win
	mov di,offset FinalString
	mov cx,13
	REP MOVSB
		
        jmp Exit_CheckScores
        
    P2Win:
        mov ah,2
        mov dh,12
        mov dl,32
        int 10h
    
        mov ah, 9
        mov dx, offset p2_win
        int 21h 
		
	mov GameOver, 1
		
	CLD    
	mov si,offset p2_win
	mov di,offset FinalString
	mov cx,13
	REP MOVSB
		
        jmp Exit_CheckScores         
        
     Draws:
        mov ah,2
        mov dh,12
        mov dl,32
        int 10h
    
        mov ah, 9
        mov dx, offset Draw
        int 21h
		
	mov GameOver, 1
						
	CLD    
	mov si,offset Draw
	mov di,offset FinalString
	mov cx,12
	REP MOVSB
		


	Exit_CheckScores:
		pop dx
		pop cx
		pop bx
		pop ax
	
	RET
CheckScores ENDP 
;==================================================

MainMenu Proc
    
    push ax
    push bx
    push cx
    push dx
    push ds 
    
    mov ah, 0				;Clearing the Screen
    mov al, 3
    int 10h
    
    mov ax, 0600h
    mov bh, 07
    mov cx, 0
    mov dx, 184fh
    int 10h
    
    
    ;Printing the Menu Selections by setting course and use the Macro
    PrintText 0,0,GameStart
    
    mov ch,0
    mov cl,0 
    MM:  				;Printing the Lower line to be used for Notification
        push cx
        Print 20, cl, Hr_Line_Color
        pop cx            
        inc cl
        cmp cl, 80
        JNE MM
    checkforinput1:
    mov AH,0            		 ;clear used scan code from buffer
    int 16H 
    
    cmp al,49              		  ;1 to Start Chat
    JE  ExitMain
    ;Call Chatting In Phase 4


    cmp al,50                		  ;2 to Start Game   
    JE Gaming
    
    
    cmp ah,1H                 		 ;Esc to exit the game
    JE ExitMain
    JNE checkforinput1
    
    ExitMain:
        mov ah,4CH
        int 21H
    
        
    Gaming: 
        pop ds
        pop dx
        pop cx
        pop bx
        pop ax 
        RET
MainMenu ENDP
;==================================================
; GameOver procedure called when the game is over
; The String the displays who won is displayed
; The user has two options:
; 1- To restart the game
; 2- To exit the game

GameOverMenu Proc
    
    push ax
    push bx
    push cx
    push dx
    push ds 
    
    mov ah, 0
    mov al, 3
    int 10h
    
    mov ax, 0600h
    mov bh, 07
    mov cx, 0
    mov dx, 184fh
    int 10h
    

	;Print the options the user has
    PrintText 0,0,GameEnd
	
	;print who won
    PrintText 10,32,FinalString
	
    mov ch,0
    mov cl,0 
    RM:  
        push cx
        Print 20, cl, Hr_Line_Color
        pop cx            
        inc cl
        cmp cl, 80
        JNE RM
    checkforinput2:
    mov AH,0             	  ;clear used scan code from buffer
    int 16H 
    
    cmp al,49                 ;1 to Restart
    JE  RestartGame
    
    cmp ah,1H                  ;Esc
    JE ExitMenu
    JNE checkforinput2
    
    ExitMenu:
        mov ah,4CH
        int 21H
		
	RestartGame:	
        pop ds
        pop dx
        pop cx
        pop bx
        pop ax 
        RET
GameOverMenu ENDP
;==================================================
; ResetAll procedure is really needed when restart is called
; The procedure is called at the beginning of the game
; The procedure does the following:
; 1- clears the players' names string
; 2- resets the scores to zeros
; 3- resets the position of the goalkeeper
; 4- resets the postion of the ball
; 5- resets the turn of current players and total number of shoots

ResetAll Proc
	mov AX,DS
    mov ES,AX  
	
	;resets scores to zeros
	mov ax,0
	mov p1_score, al  
	mov p2_score, al  
	mov GameOver, al 
	
	;resets the turn of current players and total number of shoots
	mov bl,1
	mov current_player,bl
	mov Total_Shoots,bl
	
	;Goalkeeper reset position
	mov bl,7			
	mov RBCenterU,bl
	mov bl,8			
	mov RBCenterD,bl
	
	;Last Horizontal Ball Postion
	mov bl,72
	mov hor,bl         
	
	;Clear players' names
	mov dl,'$'			
	mov cx,30
	CLD
	ClearNames:     
	mov si,offset p1_name
	mov di,offset p2_name
	mov bx,offset FinalString
	mov [si],dl
	mov [di],dl
	mov [bx],dl
	inc si
	inc di
	inc bx
	loop ClearNames

	RET
ResetAll ENDP

END MAIN    
