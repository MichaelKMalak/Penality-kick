;;=============================================================================;;
;;                 Multiplayer Penality shooting game                          ;;
;;                 Tested on DOSBox and emu8086                                ;;
;;=============================================================================;;
;;         Developed by:                         						       ;;
;;                 1-Michael Khalil Malak                                      ;;
;;                 2-Bassel Akmal Elerian                                      ;;
;;                 3-Fady Nasser Fawzy                                         ;;
;;                 4-Kerelos Diaa	                    		               ;;
;;=============================================================================;;

include macros.inc

.MODEL SMALL
.STACK 64    
.DATA  
 	GameStart	db 0ah,0dh, 0ah,0dh
				db '              ====================================================== ',0ah,0dh
				db '             ||                                                    ||',0ah,0dh                                        
				db '             ||          *    Penality Shooting Game     *         ||',0ah,0dh
				db '             ||                                                    ||',0ah,0dh
				db '             ||----------------------------------------------------||',0ah,0dh
				db '             ||                                                    ||',0ah,0dh
				db '             ||                                                    ||',0ah,0dh
				db '             ||                                                    ||',0ah,0dh          
				db '             ||       Use up and down key to move goalkeeper bar   ||',0ah,0dh
				db '             ||              and enter button to shoot             ||',0ah,0dh
				db '             ||                                                    ||',0ah,0dh
				db '             ||             Two players switch every turn          ||',0ah,0dh
				db '             ||               Each player Shoots 5 times           ||',0ah,0dh
				db '             ||                                                    ||',0ah,0dh
				db '             ||            Press F2 to send chat request           ||',0ah,0dh 
				db '             ||            Press F3 to send game request           ||',0ah,0dh
				db '             ||                                                    ||',0ah,0dh
				db '             ||                  Press ESC to Exit                 ||',0ah,0dh
				db '              ====================================================== ',0ah,0dh
				db '$'
	
	GameEnd db '                               *    GAMEOVER      *  		       ',0ah,0dh
			db '              ==================================================== ',0ah,0dh     
			db '             ||             Press Any Key To Continue            ||',0ah,0dh
			db '              ==================================================== ',0ah,0dh
			db '$'
			
			
	LevelMsg	db '                              *    Game Level    *                ',0ah,0dh
				db '              =================================================== ',0ah,0dh     
				db '             ||               Press 1 For Level 1               ||',0ah,0dh
				db '             ||               Press 2 For Level 2               ||',0ah,0dh
				db '              =================================================== ',0ah,0dh
				db '$'
	
	
											;****************************;
											;****** Game Variables ******;							 
											;****************************;
								;*****************************************************;
	
	
 ask_MyName db 'Your Name: ','$'
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
 FinalString db 16 dup('$')
 
 MyName db 16, ?,  16 dup('$')
 OpponentName db 16, ?,  16 dup('$')
 
 RBCenterU   db 7
 RBCenterD   db 8
 MyScore db 0  			;Intially player 1 score is 0
 OpponentScore db 0  			;Intially player 2 score is 0
 GoalDim db 71, 3, 75, 11 	;X1,Y1, X2,Y2
 current_player db 1     
 game_level db ?			;level 1: Delay 40ms * 2 , level 2: Delay 40ms 
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

								;*****************************************************;
								
								
											;****************************;
											;****** Chat Variables ******;							 
											;****************************;
								;*****************************************************;
	
	MyRow             	db ?
    MyCol             	db 0
    ChatRow           	db ?
    ChatCol           	db 0
    
    ChatAtt           	equ 4fh
	myAtt				equ 030h
    
    GetChar           	db 31h
    ToSendChar        	db 31h
    
    Buffer            	db 79 dup('$')
    BufferSize        	db 0
    
    intialMyRow       	equ 7				;not zero (bec of instrcution)
    intialChatRow     	equ 8				;25>intialChatRow>intialMyRow>0
    
	write_status		db	?	;if 1=> I'm sending text, if 0=> I'm receiving text
								;*****************************************************;
								
											;****************************;
											;***** Module Variables *****;							 
											;****************************;
								;*****************************************************;
	chat_invited db 0	;if there is a pending chat invitation value will be 1, otherwise it's 0
	game_invited db 0	;if there is a pending game invitation value will be 1, otherwise it's 0
	
	invitation_type  db 0	;it's 1 if a chat invitation, And 2 if a game invitation
	
	chat_host db 0	;if I'm the host it will be 1, if opponent is the host it will be 2, otherwise it is 0
	game_host db 0	;if I'm the host it will be 1, if opponent is the host it will be 2, otherwise it is 0
	
	chat_invitation_key db 88h	;the key sent to the other player indicating a chat invitation
	game_invitation_key db 99h	;the key sent to the other player indicating a game invitation
	
	accept_key db 11h		;the key if sent, the invitation (Game Or Chat) is accepted
	refuse_key db 00h		;the key if sent, the invitation (Game Or Chat) is rejected/refused
	
	chat_invitation_msg db 'You have a pending chat invitation. Do you want to accept it? y or n$'
	game_invitation_msg db 'You have a pending game invitation. Do you want to accept it? y or n$'
	
	chat_instruction_msg db 'To send a chat invitation click on F2.$'
	game_instruction_msg db 'To send a game invitation click on F3.$'
								;*****************************************************;
	
 
;==================================================
.CODE   
MAIN    PROC  
    mov ax, @DATA
    mov ds, ax
	
	mov ax, 0
	mov bx, 0
	mov cx, 0
	mov dx, 0
			
	Call GetName 			;Gets player name
	
	Restart_Game:
	Call ResetAll			;Resets positions and strings
	Call InvitationModule	;Before starting the main program, we will handle the invitations first to be like a separate module from the program itself.
	
	mov ax, 0
	mov bx, 0
	mov cx, 0
	mov dx, 0
	
	
	 ;If you are the host of either game or chat you will send first your name then receive the opponent's name, otherwise you will first receive the opponent's name then send your name
	 cmp game_host, 1
	 jnz check_chat_host
	 Call SendName
	 Call ReceiveName
	 Call ChooseLevel
	 Call SendLevel
	 jmp main_continue
	 
	 check_chat_host:
	 cmp chat_host, 1
	 jnz NotHost
	 Call SendName
	 Call ReceiveName
	 jmp main_continue
	 
	 NotHost:
	 Call ReceiveName
	 Call SendName
	 
	 main_continue:
	 cmp game_host, 0
	 jz start_chat
	 ;Start Game
	 cmp game_host, 1
	 jz start_game
	 
	 Call DrawInterface		;Freeze the screen on the game start after accepting the invitation
	 Call DrawBottomSection	;Freeze the screen on the game start after accepting the invitation
	 
	 Call ReceiveLevel
	 
	 start_game:
	 Call StartGame
	 Call GameOverMenu
	 jmp Restart_Game
	 
	 start_chat:
	;Start Chat
	
	Call StartChat
	
	jmp Restart_Game
	
	Call Exit 

MAIN  ENDP 
   
;==================================================
; Move procedure moves the goal keeper and is called when a key is pressed
; The goalkeeper is a two colored charachters on top of each other
; Move procedure directs the action depending on the key press
; possible actions are only up or down or no action

Move PROC
	push ax
	push bx
	push cx
	push dx
	push ds
	
   RightU:
    CMP Ah,48h
    JNE RightD    						; If the up arrow is not pressed jump to RightD
    Call RightUp						; If pressed, Call RightUp Procedure
   RightD:    
    CMP Ah,50h			
    JNE ENDD            				; If the down arrow is not pressed jump to ENDD
    CALL RightDown						; If pressed, Call RightDown
   
   ENDD:
	pop ds
	pop dx
	pop cx
	pop bx
	pop ax
   
   ret
Move ENDP    
;--------------------------
RightUp PROC
   
    cmp RBCenterU,0						;Check if the goalkeeper is at the very top of screen
    JE ENDRU							;if yes, end the procedure (no action)
     
    dec RBCenterU								;Decrement Column of the upper charachter so you go up 
    print RBCenterU,70,Goalkeeper_Color, ' '	;Print the new coordinates
	Delete RBCenterD,70 				;delete the other (third) charachter at the bottom
	DEC RBCenterD						;Decrement Column of the lower charachter but don't print
   
    ENDRU:ret
RightUp ENDP
;--------------------------
RightDown PROC
    cmp RBCenterD,13					;Check if the goalkeeper is at the top of the chat screen
    JE ENDRD							;if yes, end the procedure (no action)
    
    inc RBCenterD								;increment Column of the lower charachter so you go down 
    print RBCenterD,70,Goalkeeper_Color, ' '	;Print the new coordinates
    Delete RBCenterU,70 						;delete the other (third) charachter at the top
    INC RBCenterU								;increment Column of the upper charachter but don't print
   
    ENDRD:ret
RightDown ENDP

;==================================================
GetName    PROC  
    push ax
    push bx
    push cx
    push dx
    push ds 
    
    mov ah, 0
    mov al, 3
    int 10h
    
    ClearScreen
    
	ask_MyName_loop:
		;Clear the line
		Call ClearLine
		;Show the user a message to enter player 1 name
		mov ah, 9
		mov dx, offset ask_MyName
		int 21h
		
		;Receive player 1 name from the user
		mov ah, 0Ah
		mov dx, offset MyName
		int 21h
		
	cmp MyName[1], 0	;Check that input is not empty
	jz ask_MyName_loop
	
	;Checks on the first letter to ensure that it's either a capital letter or a small letter
	cmp MyName[2], 40h
	jbe ask_MyName_loop
	cmp MyName[2], 7Bh
	jae ask_MyName_loop
	cmp MyName[2], 5Bh
	jae p1_check_in_range
	
	jmp END_GetName
	
	p1_check_in_range:
	cmp MyName[2], 60h
	jbe ask_MyName_loop
	
	END_GetName:
      
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax  
    
    RET
     
GetName    ENDP
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
			Print GoalDim[1], cl, Goal_Color, ' '
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
            Print cl, GoalDim[2], Goal_Color , ' '  
            pop cx
            inc cl
            cmp cl, GoalDim[3]
        JNE loop1
        
        
        ;Draw the bottom of the goal
        mov cl, GoalDim[2]
        
        loop2:
			push cx
			Print GoalDim[3], cl, Goal_Color, ' '
			pop cx
			 
			dec cl
			mov bl, GoalDim[0]
			dec bl
			cmp cl, bl        
        JNE loop2 
        
	;Draw the goal keeper in the intial position
	Print RBCenterU,70,Goalkeeper_Color, ' '     
	Print RBCenterD,70,Goalkeeper_Color, ' ' 
	
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
	push bx
	push cx
	push dx
	push ds
	
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
	
	pop ds
	pop dx
	pop cx
	pop bx
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
    
    PrintHorizontalLine 15, Hr_Line_Color      
    
	Call PrintScores
    
    ;Draw Horizontal Line
    
    PrintHorizontalLine 17, Hr_Line_Color
    
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax 
    
    RET

DrawBottomSection ENDP 
;==================================================

PrintScores	Proc
	
	push ax
    push bx
    push cx
    push dx
    push ds
	
	;Move cursor to the scores line
    
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
    mov cl, MyName[1]

    p1_loop:
		mov ah,2
		mov dl, MyName[bx]
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
    mov bl, MyScore
    add bl, '0'
    mov ah, 2
    mov dl, bl
    int 21h
    
    ;Set cursor to write player 2 info
    mov ah, 2      
    mov dh, 16 ;Move to position Y=16 
    mov dl, 78 ;put in dl (80-2), 2=> the colon and the score
    sub dl, OpponentName[1]	;Subtract the actual player name length
	;the resulted X in dl will align both the name and the score to the right most of the screen
    int 10h
    
    ;Player 2 Info
   
    push bx
    push cx 
	
    mov bx, 2
    mov cx, 0
    mov cl, OpponentName[1]
    
	p2_loop:
		mov ah,2
		mov dl, OpponentName[bx] 
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
    mov bl, OpponentScore
    add bl, '0'
    mov ah, 2
    mov dl, bl
    int 21h  

	pop ds
    pop dx
    pop cx
    pop bx
    pop ax
	
	RET
PrintScores	ENDP
;==================================================

Delay  PROC
	cmp game_level, 2
	jz level2
	
    Call Delay40ms
	level2:
	Call Delay40ms
    RET
Delay  ENDP

Delay40ms Proc

	push ax
    push bx
    push cx
    push dx
    push ds
    mov ax, 0
	mov bx, 0
	mov cx, 0
	mov dx, 0
	
    ;40 ms delay        
    mov dx, 9C40h
    mov ah, 86h
    int 15h
	
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax
    
    RET
Delay40ms ENDP
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

CheckScores PROC			;Check The Ball Position
	mov AX,DS
    mov ES,AX   
	push ax
	push bx
	push cx
	push dx
	push ds
	
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
      inc MyScore      
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
      inc OpponentScore      
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
      call PrintScores  
      
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
    
        mov dh,MyScore
        mov dl,OpponentScore
	
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
		pop ds
		pop dx
		pop cx
		pop bx
		pop ax
	
	RET
CheckScores ENDP 

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
    
   ClearScreen    

	;Print the options the user has
    PrintText 12,0,GameEnd
	
	;print who won
    PrintText 10,32,FinalString
	
    PrintHorizontalLine 20, Hr_Line_Color
	
	;Wait keypress
    mov AH,0
    int 16H 
	
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
	
	;Clear Screen
	ClearScreen
	
	;resets scores to zeros
	mov ax,0
	mov MyScore, al  
	mov OpponentScore, al  
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
	
	;Reset Opponent actual size and maximum size
	mov OpponentName[0], 16
	mov OpponentName[1], 0
	;Reset Final String and Opponent Name
	mov dl,'$'			
	mov cx,16
	CLD
	
	mov bx,offset FinalString
	mov si, offset OpponentName[2]
	
	ClearString:     
	mov [bx],dl
	mov [si],dl
	inc bx
	inc si
	loop ClearString
	
	;Reset Module Variables
	mov game_host, 0
	mov chat_host, 0
	mov game_level, 0
	mov write_status, 0
	mov invitation_type, 0
	mov game_invited, 0
	mov chat_invited, 0
	
	mov MyCol, 0
	mov MyRow, 0
	
	RET
ResetAll ENDP

StartGame PROC
    
   
	push ax
	push bx
	push cx
	push dx
	push ds
	
	mov ax, 0
    mov bx, 0
	mov cx, 0
	mov dx, 0
	ClearScreen
	CALL DrawInterface   	;Draws the intial game interface
    CALL DrawBottomSection	;Draws the in-game chat
    
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
       
       ;Draw 'o'
       Call DrawBall 
       
       Call Delay
       
       mov si,dx   ;keep the position of the ball in SI in case a player shoots it 
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
       INT 16H  		  ;ZF=1-->NO KEY ,,, ZF=0-->KEY AVAILABLE
   
       JNE Key_Pressed1
       JMP NO_Key_Pressed1       
                     
    Key_Pressed1:   
       MOV AH,0             	  ;clear used scan code from buffer
       INT 16H  
   
       cmp ah,Shoot_Key   
       JE Shoot     
       cmp ah,1H                  ;Esc to terminate the Game
       JE Exit_1
         
       Call Move 
       
       
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
       
       ;Draw 'o'
       Call DrawBall 
       
       Call Delay
       
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
        INT 16H  			;ZF=1-->NO KEY ,,, ZF=0-->KEY AVAILABLE
        
        JNE Key_Pressed2
        JMP NO_Key_Pressed2               
   
    Key_Pressed2:   
        MOV AH,0             		;clear used scan code from buffer
        INT 16H  
        cmp ah,Shoot_Key   
        JE Shoot 
	 jmp Continuechecking
        CHECK_1:
        JMP CHECK       
       
       Continuechecking:
        cmp ah,1H                  	;Esc to terminate the Game
        JE Exit2
		
        Call Move 
       
    NO_Key_Pressed2:    
                     
    loop SecondHalfCycle
                   
CMP ah,Shoot_Key 

JNE CHECK_1   

Exit_1:
JMP Exit2

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
        
       
        Call Move 
       
        
    NO_Key_Pressed3:
        mov ah,2
        mov dx,temp
        add dl,bl    		 ;inc X horizonatally -->right
        int 10h   
        
        ;Draw
        Call DrawBall
        
        Call Delay
        
        
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
	    
	Exit2: 
	pop ds
	pop dx
	pop cx
	pop bx
	pop ax
	RET
StartGame ENDP

;*********************************************************************************************************************************************;

												 ;**************************;
												 ;****Invitations Module****;
												 ;**************************;
;**********************;
;**** Module Start ****;							 
;**********************;

SendName PROC

	push ax	
	push bx
	push cx
	push dx
	push ds

	mov bx, 0
	SendName_Loop:
	push bx
	;Check that Transmitter Holding Register is Empty
	mov dx , 3FDH		; Line Status Register

	In al , dx 			;Read Line Status
	test al , 00100000b		;Checking Transmitter Holding Register bit 
	pop bx

	JZ SendName_Loop
		push bx	
	;Now the Transmitter Holding Register is Empty and ready to send a character

	mov dx , 3F8H		; Transmit data register

	mov al, MyName[bx]

	out dx , al ;Send charachter in AL

	pop bx
	inc bx	
	cmp al, '$'
	jnz SendName_Loop

	END_SendName:
	pop ds
	pop dx
	pop cx
	pop bx
	pop ax

	RET
SendName ENDP

ReceiveName Proc
	push ax
	push bx
	push cx
	push dx
	push ds

	mov bx, 0

	ReceiveName_Loop:

	push bx
	;Check that Data Ready
	mov dx , 3FDH		; Line Status Register
	in al , dx 
	test al , 1		;Checking Data Ready bit 

	pop bx
	JZ ReceiveName_Loop	;If not ready end
	push bx
	;If data ready, fetch the data
	mov dx , 03F8H
	in al , dx 
	pop bx
	mov OpponentName[bx], al
	inc bx

	cmp al, '$'
	jnz ReceiveName_Loop

	pop ds
	pop dx
	pop cx
	pop bx
	pop ax
	RET
ReceiveName ENDP

SendLevel PROC

	push ax	
	push bx
	push cx
	push dx
	push ds

	SendLevel_Loop:
	;Check that Transmitter Holding Register is Empty
	mov dx , 3FDH		; Line Status Register

	In al , dx 			;Read Line Status
	test al , 00100000b		;Checking Transmitter Holding Register bit 

	JZ SendLevel_Loop
			
	;Now the Transmitter Holding Register is Empty and ready to send a character

	mov dx , 3F8H		; Transmit data register
	mov al, game_level

	out dx , al ;Send charachter in AL

	END_SendLevel:
	pop ds
	pop dx
	pop cx
	pop bx
	pop ax

	RET
SendLevel ENDP

ReceiveLevel Proc
	push ax
	push bx
	push cx
	push dx
	push ds

	ReceiveLevel_Loop:

	;Check that Data Ready
	mov dx , 3FDH		; Line Status Register
	in al , dx 
	test al , 1		;Checking Data Ready bit 

	JZ ReceiveLevel_Loop	;If not ready end
	
	;If data ready, fetch the data
	mov dx , 03F8H
	in al , dx 
	
	mov game_level, al
	
	pop ds
	pop dx
	pop cx
	pop bx
	pop ax
	RET
ReceiveLevel ENDP

InvitationModule Proc
push ax
push bx
push cx
push dx
push ds

ClearScreen
   
	;Printing the Menu Selections by setting course and use the Macro
    
    
   mov ax, 0
   mov bx, 0
   mov cx, 0
   mov dx, 0
   Module_Loop:
		PrintText 0,0, GameStart
        Module_Check: 
			call ModuleReceive
			
			cmp chat_host, 0
			jnz END_InvitationModule
			
            cmp game_host, 0
			jnz END_InvitationModule
			
			mov ah, 1
            int 16h
			jz Module_Check
		
		mov ah, 0
		int 16h  
		cmp al, 27		;ESCAPE
		jz ModuleEnd_Program
		
		cmp ah, 3Ch		;F2
		jnz Module_Check_F3
		
		;Chat Invitation
		cmp chat_host, 0
		jnz Module_Loop
		cmp chat_invited, 1
		jz Module_Loop
		mov al, chat_invitation_key
		mov invitation_type, 1
		call ModuleSend
		jmp Module_Loop
		
		Module_Check_F3:
		cmp ah, 3Dh
		jnz Module_Loop
		mov al, game_invitation_key
		mov invitation_type, 2
		call ModuleSend
		
	jmp Module_Loop  
	
	jmp END_InvitationModule
	ModuleEnd_Program:
    
	call Exit

	END_InvitationModule:
	
	pop ds
	pop dx
	pop cx
	pop bx
	pop ax
	RET
InvitationModule ENDP

ModuleSend Proc

	push ax	;To save the character in al to send

;Check that Transmitter Holding Register is Empty
	mov dx , 3FDH		; Line Status Register
	ModuleSend_Loop: 	In al , dx 			;Read Line Status
		test al , 00100000b		;Checking Transmitter Holding Register bit 
	JZ ModuleSend_Loop
			
	;Now the Transmitter Holding Register is Empty and ready to send a character
	
	mov dx , 3F8H		; Transmit data register
	pop ax
  	;Value is already in al
  	out dx , al 
		
	END_ModuleSend:ret    
ModuleSend Endp

ModuleReceive Proc
     ;Check that Data Ready
	mov dx , 3FDH		; Line Status Register
	in al , dx 
  	test al , 1		;Checking Data Ready bit 
  	
	JZ END_ReceiveModule1	;If not ready end

	;If data ready, fetch the data
  		mov dx , 03F8H
  		in al , dx 
		
	cmp chat_host, 0
	jnz END_ReceiveModule1
	
	cmp game_host, 0
	jnz END_ReceiveModule1
	
	cmp al, chat_invitation_key
	jnz Receive_Continue1
	;Chat Invitation Received
	
		mov chat_invited, 1
		Call ReceivedInvitation
	jmp END_ReceiveModule1
	
	Receive_Continue1:
	cmp al, game_invitation_key
	jnz Receive_Continue2
	;Game Invitation Received
	
		mov game_invited, 1
		Call ReceivedInvitation
		
	END_ReceiveModule1:ret
	
	Receive_Continue2:
	cmp al, accept_key
	jnz Receive_Continue3
	;Invitation Accepted
		
		cmp invitation_type, 1
		jnz check_game_invitation
		;Chat Invitation
		
		mov chat_host, 1
		mov chat_invited, 0
		mov invitation_type, 0
		
		check_game_invitation:
		cmp invitation_type, 2
		jnz END_ReceiveModule2
		;Game Invitation
		
		mov game_host, 1
		mov game_invited, 0
		mov invitation_type, 0
		
	jmp END_ReceiveModule2
	Receive_Continue3:
	cmp chat_invited, 0
	jnz END_ReceiveModule2
	cmp al, refuse_key
	jnz END_ReceiveModule2
	;Invitation Refused
	
		mov chat_host, 0
		mov chat_invited, 0
		mov invitation_type, 0
	
	END_ReceiveModule2:ret
ModuleReceive Endp


ReceivedInvitation	PROC
	ClearScreen
	
	cmp game_invited, 1
	jz Game_Invitation_Received
	
	Chat_Invitation_Received:
	PrintStr chat_invitation_msg
	jmp ReceivedInvitation_Continue1
	
	Game_Invitation_Received:
	PrintStr game_invitation_msg
	
	ReceivedInvitation_Continue1:
	
	
	ReceivedInvitation_Again:
	mov ah, 0
	int 16h
	
	ReceivedInvitation_Check_Y:
	cmp al, 'y'
	jnz ReceivedInvitation_Check_N
	;Accept Invitation
	
	mov al, accept_key
	cmp game_invited, 1
	jnz accept_chat
	
	accept_game:
	mov game_host, 2
	jmp ReceivedInvitation_Continue2
	
	accept_chat:
	mov chat_host, 2
	
	ReceivedInvitation_Continue2:
	call ModuleSend
	
	jmp ReceivedInvitation_END
	ReceivedInvitation_Check_N:
	cmp al, 'n'
	jnz ReceivedInvitation_Again
	;Refuse Invitation
	
	mov al, refuse_key
	cmp game_invited, 1
	jnz refuse_chat
	
	refuse_game:
	mov game_invited, 0
	jmp ReceivedInvitation_Continue3
	
	refuse_chat:
	mov chat_invited, 0
	
	ReceivedInvitation_Continue3:
	call ModuleSend
	
	ClearScreen
	;Display the menu again
	PrintText 0,0, GameStart
	
	mov invitation_type, 0
	
	ReceivedInvitation_END:
	RET
ReceivedInvitation	ENDP

;**********************;
;***** Module End *****;							 
;**********************;

;*********************************************************************************************************************************************;           
        
Intialize_Port Proc
	push ax
	push bx
	push cx
	push dx
	push ds

	;Setting the divisor value (To get baud rate 9600 [Value of divisor=00 0ch])
	mov dx,3fbh 			; Line Control Register
	mov al,10000000b		;Set Divisor Latch Access Bit
	out dx,al				;Out it
	
	;Setting the LSB (Least Significant Byte) to 0ch
	mov dx,3f8h			
	mov al,0ch			
	out dx,al
	
	;Setting the MSB (Most Significant Byte) to 00h
	mov dx,3f9h
	mov al,00h
	out dx,al
	
	;Port configuration
	
	mov dx,3fbh
	mov al,00000011b
	;(0xxxxxxx):Access to Receiver buffer, Transmitter buffer
	;(x0xxxxxx):Set Break disabled
	;(xx000xxx):No Parity
	;(xxxxx0xx):One Stop Bit
	;(xxxxxx11):Word size=8bits
	out dx,al

	pop ds
	pop dx
	pop cx
	pop bx
	pop ax
   ret 
Intialize_Port Endp 

;----------------------------
      ColorScreen PROC 
           push ax
           push bx
           push cx
           push dx
           
           mov ah,7h        ;Scroll down
           mov al, 25
		   sub al, intialChatRow	;number of lines to scroll   	
           mov bh,ChatAtt   ;blank lines at bottom of window (Chat Section Color)
           mov ch,intialChatRow        ;row - window's upper left corner
           mov cl,0         ;column - window's upper left corner
           mov dh,25        ;row - window's lower right corner
           mov dl,79        ;column - window's lower right corner
           int 10h
           
           pop dx
           pop cx
           pop bx
           pop ax
            
            ret
        ColorScreen ENDP   
;----------------------------        
        ScrollChatScreen PROC 
           push ax
           push bx
           push cx
           push dx
           
           mov ah,6h                ;Scroll up
           mov al,1                 ;number of lines to scroll
           mov bh,ChatAtt           ;blanck lines at bottom of window
           mov ch,intialChatRow     ;row - window's upper left corner
           mov cl,0                 ;column - window's upper left corner
           mov dh,25                ;row - window's lower right corner
           mov dl,79                ;column - window's lower right corner
           int 10h
           
           pop dx
           pop cx
           pop bx
           pop ax
            
            ret
        ScrollChatScreen ENDP
;---------------------------- 
        SendOperation PROC                       
            
           push ax
           push bx
           push cx
           push dx
          
		   
           mov ch,00h          
           mov cl, BufferSize
           mov bx, 0
           
           loopBuffer:
           
           mov  ah,Buffer[bx]  
           mov  ToSendChar,ah 
           mov  GetChar,ah
           
           mov  al,'$'
           mov  Buffer[bx],al
           
           call  SendOneChar
           call  writeChatScreen
            
           inc bx    
           dec cl
           jnz loopBuffer 
           
           ;resets size of buffer
           mov bl,00h
           mov BufferSize,bl
           
           ;send Enter after string
           mov  al,0Dh
           mov  ToSendChar,al
           call  SendOneChar
           
           mov   GetChar,al
           call  writeChatScreen
           
           pop dx
           pop cx
           pop bx
           pop ax
            
            
            ret
        SendOperation ENDP 
;----------------------------  
 SendOneChar PROC                       
            
           push ax
           push bx
           push cx
           push dx
           
           
           WaitTillReady: 
           mov dx,3fdh
           in al,dx
           test al,00100000b
           JZ WaitTillReady
           
           mov dx, 3f8h
           mov al, ToSendChar
           out dx, al
           
           
           
           pop dx
           pop cx
           pop bx
           pop ax
            
            
            ret
 SendOneChar ENDP 
 ;---------------------------
        writeMyScreen PROC                       
            
           push ax
           push bx
           push cx
           push dx
             
            SetCursor MyRow,MyCol
          
       
             ;;;;; CHECK BACKSPACE ;;;;; 
            mov al, ToSendChar 
            cmp al,08h
            
            jnz  writeMyScreen_Continue2
               
                 push bx
                cmp MyCol,0
                jnz notbeg1
                
                  cmp MyRow, intialMyRow
                  jnz notbeg1
                  jmp endbck1
                  
                notbeg1:
                dec MyCol
                mov al, ' '
				PrintCharAl MyRow, MyCol, myAtt
             push ax
             push bx    
             
             mov bh,00h
             mov bl, BufferSize
             mov  ah,'$'  
             mov  Buffer[bx],ah
             
             dec BufferSize
             pop bx
             pop ax 
                
                endbck1:
                pop bx
                
                
                 Jmp writeMyScreen_End  
            
           writeMyScreen_Continue2: 
            ;;;;; 
             
             cmp BufferSize, 77
			 jz writeMyScreen_End
			 
             PrintCharAl MyRow,MyCol,myAtt
             ShiftCursorMy   
             AddSentToBuffer   
             
        writeMyScreen_End:             
           pop dx
           pop cx
           pop bx
           pop ax
            
            
            ret
        writeMyScreen ENDP
;----------------------------   
		PrintOpponent	Proc
			push ax
			push bx
			push cx
			
			mov bx,2	;In case taking the name from the user, intial value of bx will be 2
			mov ch, 0
			mov cl, OpponentName[1]
			
			PrintOpponent_Loop:
			
			mov al, OpponentName[bx]
			PrintCharAl ChatRow, ChatCol, ChatAtt
			ShiftCursorChat
			
			inc bx
			loop PrintOpponent_Loop
			
			mov al, ':'
			PrintCharAl ChatRow,ChatCol,ChatAtt
			ShiftCursorChat
			
			mov al, ' '
			PrintCharAl ChatRow,ChatCol,ChatAtt
			ShiftCursorChat
			
			pop cx
			pop bx
			pop ax
			RET
		PrintOpponent ENDP
;----------------------------   
		PrintMe	Proc
		
			push ax
			
			mov al, 'M'
			PrintCharAl ChatRow,ChatCol,ChatAtt
			ShiftCursorChat
			
			mov al, 'e'
			PrintCharAl ChatRow,ChatCol,ChatAtt
			ShiftCursorChat
			
			mov al, ':'
			PrintCharAl ChatRow,ChatCol,ChatAtt
			ShiftCursorChat
			
			mov al, ' '
			PrintCharAl ChatRow,ChatCol,ChatAtt
			ShiftCursorChat
			
			pop ax
			RET
		PrintMe ENDP
;---------------------------- 
 

        writeChatScreen PROC                       
            
           push ax
           push bx
           push cx
           push dx
             
            SetCursor ChatRow,ChatCol
			cmp ChatCol, 0
			jnz continue_writeChatScreen
			
			cmp write_status, 1
			jnz Print_P2
			call PrintMe
			jmp continue_writeChatScreen
			Print_P2:
			call PrintOpponent
			
			continue_writeChatScreen:
           ;;;;; CHECK ENTER ;;;;; 
           ;;;;; CHECK ENTER ;;;;; 
            mov al, GetChar 
            cmp al,0Dh
            
            jnz  writeChatScreen_Continue1
               
                WriteEnter ChatRow, ChatCol
                cmp ChatRow, 25
                jnz writeChatScreen_End
                call ScrollChatScreen
                dec ChatRow
                     
             dec ChatCol
                      
           
         writeChatScreen_Continue1: 

             PrintCharAl ChatRow,ChatCol,ChatAtt
             ShiftCursorChat   
           
		writeChatScreen_End:		   
           pop dx
           pop cx
           pop bx
           pop ax

            ret
        writeChatScreen ENDP
;---------------------------- 
 clearMyPart PROC
 
        SetCursor MyRow,MyCol 
        xor cx, cx
		loop3:
		Print intialMyRow, cl, MyAtt, ' '
		inc cl
		cmp cl, 79		
		jnz loop3 
        SetCursor MyRow,MyCol
		ret
 clearMyPart ENDP 

;----------------------------  

Exit PROC                       
	
	ClearScreen
	
	MOV AX,4C00H
	INT 21H
	RET 
Exit ENDP 
	
;----------------------------
	
StartChat PROC

ClearScreen
	
call ColorScreen

	 mov MyRow,intialMyRow
	 mov ChatRow,intialChatRow
	 
	 SetCursor MyRow, MyCol
	 PrintHorizontalLine intialMyRow, MyAtt ;Draws a line on the row defined in al
	  

Again:     
	  
	  get:
	   mov  dx, 3FDH            
	   in   al,dx
	   test al,1
	   JZ send
	   
	   
	   mov dx, 03f8h
	   in  al,dx 
	   mov GetChar,al
	   mov write_status, 0
	   call writeChatScreen          
	  
	  send:
	   mov ah,01h
	   int 16h
	   JZ get
	   
	   mov ah,00
	   int 16h
	   
	   mov ToSendChar, al
	   mov write_status, 1
	   cmp ah,1
	   jz END_Program
		 
	   noEsc:

	   cmp al,0Dh
	   jnz noEnter
		   
		   cmp BufferSize,0
		   jz  Again  
		   
		   call SendOperation
		   mov MyRow, intialMyRow
		   mov MyCol, 0
		   call clearMyPart  
		   
		   jmp Again
	   noEnter:
	   
	   cmp BufferSize, 78
	   jz Again
	   
	   call writeMyScreen  
	  
	 Jmp Again 
	 
	 
	END_Program:
	RET
StartChat ENDP

ChooseLevel Proc

	ClearScreen

	PrintStr LevelMsg

	Level_Check: 
		mov ah, 1
		int 16h
	jz Level_Check
		
	mov ah, 0
	int 16h  
	
	push ax
	FlushKeyboardBuffer
	pop ax
	
	ChooseLevel_Check_1:	
	cmp al, 31h
	jnz ChooseLevel_Check_2
	;Level 1
	mov game_level, 1
	jmp ChooseLevel_End
	
	ChooseLevel_Check_2:
	cmp al, 32h
	jnz Level_Check
	;Level 2
	mov game_level, 2
	
	ChooseLevel_End:
	RET
ChooseLevel ENDP

END MAIN    
