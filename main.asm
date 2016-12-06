 include macros.inc

.MODEL SMALL
.STACK 64    
.DATA 
 
 RBTop       db 6
 RBCenterU   db 7
 RBCenterD   db 8
 RBBottom    db 9
               
 ask_p1_name db 'Player 1 Name: ','$'
 ask_p2_name db 'Player 2 Name: ','$'
 score_msg db "'s score:$"
 p1_name db 30, ?,  30 dup('$')
 p2_name db 30, ?,  30 dup('$')
  
 p1_score db 0  ;Intially player 1 score is 0
 p2_score db 0  ;Intially player 2 score is 0
 
 GoalDim db 71, 3, 75, 11 ;X1,Y1, X2,Y2
 current_player db 1                        
 
 Hr_Line_Color db 0c0h
 Goal_Color db 0c0h
 Goalkeeper_Color db 20h
 Player_Color db 02h
 Ball_Color db 0ch
 
 Coordinate_BallCurve dw  0205h , 6     

 temp dw ? 
 Shoot_Key  db  1Ch; Enter scan code   
 
 Total_Shoots db 1       ;inc until 10 (5 for each player)

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

 
 
.CODE   
MAIN    PROC   
    
    mov ax, @DATA
    mov ds, ax
    mov ax, 0
     
         
    CALL GetNames
    
    ;mov current_player, 2 ;Set player to player 2
        
    CALL DrawInterface   
    
    CALL WriteOneFifthScreen
    
    mov Ah,03h
    int 10h    
    mov Bh, 00
    mov Cx, 01  

    mov ah,1
    mov cx,2b0bh
    int 10h 
   
   Print RBCenterU,70,Goalkeeper_Color     
   Print RBCenterD,70,Goalkeeper_Color 
   
    
    
    

;Wait for key from keyboard to shoot
    
 CHECK: 
    mov ah, 2h              ;Setting the Status Bar
    mov bh, 0      
    mov dl, 0 
    mov dh, 14
    int 10h
     
    mov ah,9                ;Clear The Status Bar
    mov bh,0 
    mov al,32               ;Printing Space
    mov cx,80               ;80 Times to cover all Horizontal Line
    mov bl ,0AFh            ;Set Status Bar Color -> white on Green Background
    int 10h
    
    cmp current_player,1
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
         
       MOV AH,1              ;Get key pressed
       INT 16H  
   
       JNE Key_Pressed1
       JMP NO_Key_Pressed1 
       
CHECK_1:
JMP CHECK       
                     
    Key_Pressed1:   
       MOV AH,0             ;clear used scan code from buffer
       INT 16H  
   
       cmp ah,Shoot_Key   
       JE Shoot     
       cmp ah,1H                  ;Esc
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
       sub dl,bl         ;dec X Coordinate -->left
       dec dl
       add dh,bl         ;inc Y Coordinate -->down
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
        
        MOV AH,1              ;Get key pressed
        INT 16H  
        
        JNE Key_Pressed2
        JMP NO_Key_Pressed2               
        
    Key_Pressed2:   
        MOV AH,0             ;clear used scan code from buffer
        INT 16H  
        cmp ah,Shoot_Key   
        JE Shoot     
        cmp ah,1H                  ;Esc
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
        MOV AH,0             ;clear used scan code from buffer
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
        add dl,bl     ;inc X horizonatally -->right
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
     
        add bl ,6 
        
        ;Fady's Part
		mov ah,3h
		mov bh,0h
		int 10h
         
		cmp dl,69                             ;X is higher than the Goal Line
		JAE GoToCheckScores  
		
    Loop Horizontal         
	
    GoToCheckScores:
		Call CheckScores
    jmp CHECK_1 
        
    
Exit: 
    mov ah,004CH
    int 21H    


MAIN        ENDP    

Move PROC
   RightU:
    CMP Ah,48h
    JNE RightD    
    Call RightUp
   RightD:    
    CMP Ah,50h
    JNE ENDD       
    CALL RightDown
   ENDD:ret
Move ENDP    

RightUp PROC
   
    cmp RBCenterU,0
    JE ENDRU
     
    dec RBCenterU
    print RBCenterU,70,Goalkeeper_Color
    Delete RBCenterD,70
    Delete RBCenterD,70 
    DEC RBCenterD
   
    ENDRU:ret
RightUp ENDP

RightDown PROC
    cmp RBCenterD,13
    JE ENDRD
    
    inc RBCenterD
    print RBCenterD,70,Goalkeeper_Color 
    Delete RBCenterU,70 
    INC RBCenterU
   
    ENDRD:ret
RightDown ENDP

GetNames    PROC  
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
    
    mov ah, 2
    mov dx, 0
    int 10h
    
    mov ah, 9
    mov dx, offset ask_p1_name
    int 21h
    
    mov ah, 0Ah
    mov dx, offset p1_name
    int 21h
    
    mov ah, 2
    mov dl, 10
    int 21h
    
    mov ah, 2
    mov dl, 13
    int 21h
    
    mov ah, 9
    mov dx, offset ask_p2_name
    int 21h
    
    mov ah, 0Ah
    mov dx, offset p2_name
    int 21h
      
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax  
    
    RET
     
GetNames    ENDP

DrawInterface   PROC
    
    push ax
    push bx
    push cx
    push dx
    push ds
    
    mov ax, 0600h
    mov bh, 07
    mov cx, 0
    mov dx, 184fh
    int 10h
    
    
    mov ax, 0
    mov bx, 0
    mov cx, 0
    mov dx, 0
        
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
    mov dh, 8 ;Move to position Y=11
    int 10h 
    
    mov bh, 0
    mov dx, 0
	mov al, 'P'
	mov bl, Player_Color
    mov ah, 9
    mov cx, 1
    int 10h
                            
    cmp current_player, 1  ;Check if the current player is player1
    JE write_1
    
    ;if player 2 => Print 2
	Call Print_P2
    
    mov ax,0
    AND ax,ax
    JE continue1
    
	write_1:
    ;if player 1 => Print 1
    Call Print_P1
    
    continue1:
    
    ;Draw the goal
    
        ;Draw colored spaces indicating the goal beginning
        
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
        
        
        ;Draw the back of the goal (|)
        mov cl, GoalDim[1]
        inc cl 
        loop1:
            push cx   
            Print cl, GoalDim[2], Goal_Color   
            pop cx
            inc cl
            cmp cl, GoalDim[3]
        JNE loop1
        
        
        ;Draw colored spaces indicating the goal ending
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
        
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax 
    
    RET                
                    
DrawInterface   ENDP

WriteOneFifthScreen PROC
    push ax
    push bx
    push cx
    push dx
    push ds
mov ax, 0
mov bx, 0
mov cx, 0
mov dx, 0

;Draw last one fifth section
    
    ;Draw Horizontal Line
    
    loop3:
        push cx
        Print 15, cl, Hr_Line_Color      
        pop cx      
        inc cl
        cmp cl, 80
    JNE loop3
    
    ;Move to the next line
    
    mov ah, 2      
    mov dl, 0 ;Move to position X=0
    mov dh, 16 ;Move to position Y=21
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
       
    mov ah, 2
    mov bx, 0
    mov dl, ':'
    int 21h
    
    
    mov bl, p1_score
    add bl, '0'
    mov ah, 2
    mov dl, bl
    int 21h
    
    ;Move to write player 2 info
    mov ah, 2      
    mov dh, 16 ;Move to position Y=16 
    mov dl, 73 
    sub dl, p2_name[1]
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
    
    mov ah, 2
    mov dl, ':'
    int 21h
           
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
    JNE loop6
    
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax 
    
    RET

WriteOneFifthScreen ENDP 

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

Print_P1 PROC
	push ax
	push bx
	push cx
	push dx
	
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
	
	pop dx
	pop cx
	pop bx
	pop ax
	
	RET
Print_P1 ENDP

Print_P2 PROC
	push ax
	push bx
	push cx
	push dx
	
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
	
	pop dx
	pop cx
	pop bx
	pop ax
	
	RET
Print_P2 ENDP

ChangeScore PROC   ;taken from write in fifth of screen
    
	push ax
	push bx
	push cx
	push dx
	
    mov ah, 2
    mov bh,0      
    mov dl, 0 ;Move to position X=0
    mov dh, 16 ;Move to position Y=21
    int 10h 
    
    
    ;Writing players' names and scores
    
    ;Player 1 Info  
    
    push bx
    push cx
    mov cx, 0 
    mov bx, 2
    mov cl, p1_name[1]
    p1loop:
    mov ah,2
    mov dl, p1_name[bx]
    int 21h
    inc bx
    loop p1loop
    pop cx
    pop bx
       
    mov ah, 2
    mov bx, 0
    mov dl, ':'
    int 21h
    
    
    mov bl, p1_score
    add bl, '0'
    mov ah, 2
    mov dl, bl
    int 21h
    
    ;Move to write player 2 info
    mov ah, 2
    mov bh,0      
    mov dh, 16 ;Move to position Y=16 
    mov dl, 73 
    sub dl, p1_name[1]
    int 10h
    
    ;Player 2 Info
   
    push bx
    push cx 
    mov bx, 2
    mov cx, 0
    mov cl, p2_name[1]
    p2loop:
    mov ah,2
    mov dl, p2_name[bx] 
    int 21h
    inc bx
    loop p2loop
    pop cx
    pop bx
    
    mov ah, 2
    mov dl, ':'
    int 21h
           
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

CheckScores PROC
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
        cmp dh,3
        JBE Outline
        cmp dh,11
        JAE Outline
        
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
      cmp current_player,1
      JE incp1
      
      cmp current_player,2 
      JE incp2
         
                                          ;Incrementing The Scores
    incp1:
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
      
    incp2:
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
      
      
      
    Switch:
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
      call ChangeScore  
      
      cmp Total_Shoots,10
      JE Result
      
      inc Total_Shoots
      
      jmp Exit_CheckScores         
                                          
    
    Result:
        mov ah, 0
        mov al, 3h
        int 10h 
    
        mov dh,p1_score
        mov dl,p2_score
        cmp dl,dh
        JB P1Win
        
        cmp dl,dh
        JA P2Win
        
        
        JMP Draws
    
    
    P1Win:
        mov ah,2
        mov dh,12
        mov dl,32
        int 10h
    
        mov ah, 9
        mov dx, offset p1_win
        int 21h
        jmp Exit_CheckScores
        
    P2Win:
        mov ah,2
        mov dh,12
        mov dl,32
        int 10h
    
        mov ah, 9
        mov dx, offset p2_win
        int 21h 
        jmp Exit_CheckScores         
        
     Draws:
        mov ah,2
        mov dh,12
        mov dl,32
        int 10h
    
        mov ah, 9
        mov dx, offset Draw
        int 21h


	Exit_CheckScores:
	pop dx
	pop cx
	pop bx
	pop ax
	
	RET
CheckScores ENDP


END MAIN                   	
