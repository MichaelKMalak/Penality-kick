 include macros.inc

.MODEL small
.STACK 64    
.DATA 
 
 RBTop       db 6
 RBCenterU   db 7
 RBCenterD   db 8
 RBBottom    db 9
               
 ask_p1_name db 'Player 1 Name: ','$'
 ask_p2_name db 'Player 2 Name: ','$'
 score_msg db "'s score:$"
 p1_name db 30, ?, 30 dup('$')
 p2_name db 30, ?, 30 dup('$')
  
 p1_score db 0  ;Intially player 1 score is 0
 p2_score db 0  ;Intially player 2 score is 0
 
GoalDim db 71, 3, 74, 11 ;X1,Y1, X2,Y2
 current_player db 1                        
 
 Hr_Line_Color db 0c0h
 
 
 Coordinate_BallCurve dw  0205h , 6     

temp dw ? 
cx_value dw ?
Shoot_Key  db  1Ch; Enter scan code

 
.CODE   
MAIN    PROC   
    
    mov ax, @DATA
    mov ds, ax
    mov ax, 0
    
    mov ah, 0
    mov al, 3
    int 10h 
    
    CALL GetNames
    
    mov current_player, 2 ;Set player to player 2
        
    CALL DrawInterface
    
    CALL WriteOneFifthScreen              
         
    
    
   mov Ah,03h
   int 10h    
   mov Bh, 00
   mov Cx, 01  

   mov ah,1
   mov cx,2b0bh
   int 10h 
   
   
   
   Print RBCenterU,70,20h     
   Print RBCenterD,70,20h  
 
   
;   DrawCurve_Only
   
   ; DrawCurve_Shoot  
    
;   activeLoop:
   
;    mov ah, 01h
 ;   int 16h         ;ZF=1 no charachter in buffer
  ;  jz activeLoop
   ;     mov Ah,00
    ;    int 16h     ;waits for buffer till it's not empty
     ;   CALL Move            

  ; JMP activeLoop
; CALL  Active       



;Wait for key from keyboard to shoot
          CHECK:
          
                            ; DrawCurve   macro
                          ;  DrawCurve Macro    
                        ;Get Curve co
                       ; mov bx,Coordinate_BallCurve[0] ;start      
                       mov bl,0 
                       mov cx,Coordinate_BallCurve[2] ; radius
                        FirstHalfCycle:  
                                          
                                           
                                           
                                           mov ah,2
                                           mov dx, Coordinate_BallCurve[0]
                                           add dl,bl                ;inc X Coordinate --> right  
                                           ; add dl,bl                ;inc X Coordinate --> right
                                           ;inc dl
                                           add dh,bl                ;inc Y Coordinate --> down
                                           int 10h      
                                          push dx
                                           
                                           ;Draw 'o'
                                           mov ah,2
                                           mov dl,'o'
                                           int 21h  
                                                pop dx
                                                 mov si,dx    
                                            ;To clear the last 'o'       
                                            mov ah,2
                                             mov dx,si
                                             int 10h      
                                          ;  ;Draw space
                                            mov ah,2
                                           mov dl,' '
                                           int 21h  
                                           
                                           
                                           
                                            
                                           inc bl   
                                             
                                         
                                               MOV AH,1              ;Get key pressed
                                                INT 16H  
                                           JNZ Key_Pressed1
                                           JMP NO_Key_Pressed1               
                                            Key_Pressed1:   
                                                     MOV AH,0             ;clear used scan code from buffer
                                                INT 16H  
                                                cmp ah,Shoot_Key   
                                                JZ Shoot     
                                                pusha
                                                mov cx_value,cx   
                                               Call Move 
                                               mov cx ,cx_value 
                                               popa
                                           NO_Key_Pressed1:    
                                                  
                        loop FirstHalfCycle
                        
                       ; mov bl,0  
                        mov bl,1
                         mov cx,Coordinate_BallCurve[2] ; radius 
                         dec cx 
                         mov temp,si               
                         SecondHalfCycle:   
                                           mov ah,2
                                              mov dx, temp
                                           sub dl,bl         ;dec X Coordinate -->left
                                          ; dec dl
                                           add dh,bl         ;inc Y Coordinate -->down
                                           int 10h    
                                          push dx
                                           ;Draw 'o'
                                           mov ah,2
                                           mov dl,'o'
                                           int 21h  
                                            pop dx
                                             mov si,dx
                                           
                                           ; To clear the last 'o'       
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
                                           JNZ Key_Pressed2
                                           JMP NO_Key_Pressed2               
                                            Key_Pressed2:   
                                                     MOV AH,0             ;clear used scan code from buffer
                                                INT 16H  
                                                cmp ah,Shoot_Key   
                                                JZ Shoot     
                                                pusha
                                               mov cx_value,cx   
                                               Call Move 
                                               mov cx ,cx_value 
                                               popa
                                           NO_Key_Pressed2:    
                                             
                        loop SecondHalfCycle
                              
                        
                       ; ENDM DrawCurve           
                       cmp ah,Shoot_Key 
               
           JNZ CHECK   
           
           
           ;si contains co-ordinate of start of shooting line
           Shoot:    
          
                
                                                       
                  mov cx,12 ;Horizontal shoot  
                  mov bl,1   
                  mov temp,si
                  Horizontal:  
                           
                                               MOV AH,1              ;Get key pressed
                                                INT 16H  
                                           JNZ Key_Pressed3
                                           JMP NO_Key_Pressed3               
                                            Key_Pressed3:   
                                                     MOV AH,0             ;clear used scan code from buffer
                                                INT 16H      
                                                pusha
                                               mov cx_value,cx   
                                               Call Move 
                                               mov cx ,cx_value 
                                               popa
                                           
                            
                       NO_Key_Pressed3:
                             mov ah,2
                             mov dx,temp
                             add dl,bl     ;inc X horizonatally -->right
                             int 10h   
                            push dx
                             ;Draw
                             mov ah,2                                                                                  
                             mov dl,'o'
                             int 21h
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
                             
                             
                                       
                                         
                  Loop Horizontal             





;   mov ah, 4ch
 ;  int 21h      
 hlt
MAIN        ENDP    
 
       
  
  
  
Move PROC
   RightU:
    CMP Ah,48h
    JNZ RightD    
    Call RightUp
   RightD:    
    CMP Ah,50h
    JNZ ENDD       
    CALL RightDown
   ENDD:ret
Move ENDP    

RightUp PROC
   
     cmp RBCenterU,0
      jz ENDRU
    dec RBCenterU
    print RBCenterU,70,20h
    Delete RBCenterD,70 
    DEC RBCenterD
   
    ENDRU:ret
RightUp ENDP

RightDown PROC
    cmp RBCenterD,14
      jz ENDRD
    
    inc RBCenterD
    print RBCenterD,70,20h 
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
    
    mov dh, 0
    mov ah, 2
    mov dl, '+'
    int 21h
    
    ;Write P1 or P2 under the + sign
    mov al, 0
    mov ah, 2
    mov dl, 5 ;Move to position X=5
    mov dh, 8 ;Move to position Y=11
    int 10h 
    
    mov dh, 0
    mov ah, 2
    mov dl, 'P'
    int 21h
                            
    cmp current_player, 1  ;Check if the current player is player1
    jz write_1
    
    ;if player 2 => Print 2
    mov dh, 0
    mov ah, 2
    mov dl, '2'
    int 21h
    
    mov ax,0
    AND ax,ax
    jz continue1
    
    ;if player 1 => Print 1
    write_1:
    mov dh, 0
    mov ah, 2
    mov dl, '1'
    int 21h
    
    continue1:
    
    ;Draw the goal
    
        ;Draw colored spaces indicating the goal beginning
        
        mov cl, GoalDim[0]
        
        
        loop0:
        push cx
        Print GoalDim[1], cl, 0c0h
        pop cx
                
        inc cl  
        mov bl, GoalDim[2]
        inc bl
        cmp cl, bl 
        jnz loop0 
        
        
        ;Draw the back of the goal (|)
        mov cl, GoalDim[1]
        inc cl 
        loop1:
            push cx   
            Print cl, GoalDim[2], 0c0h   
            pop cx
            inc cl
            cmp cl, GoalDim[3]
        jnz loop1
        
        
        ;Draw colored spaces indicating the goal ending
        mov cl, GoalDim[2]
        
        loop2:
        push cx
        Print GoalDim[3], cl, 0c0h
        pop cx
         
        dec cl
        mov bl, GoalDim[0]
        dec bl
        cmp cl, bl        
        jnz loop2 
        
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
    jnz loop3
    
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
    mov dl, 78 
    sub dl, p1_name[1]
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
    jnz loop6
    
    pop ds
    pop dx
    pop cx
    pop bx
    pop ax 
    
    RET

WriteOneFifthScreen ENDP

DelayHalfSecond  PROC
    
    push ax
    push bx
    push cx
    push dx
           
    mov cx, 7h
    mov dx, 0A120h
    mov ah, 86h
    int 15h
    
    pop dx
    pop cx
    pop bx
    pop ax
    
    RET
DelayHalfSecond  ENDP

END MAIN 
