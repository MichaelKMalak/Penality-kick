 include macros.inc

.MODEL large
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
 
 GoalDim db 70, 3, 73, 11 ;X1,Y1, X2,Y2
 current_player db 1   
 
 
 
 Coordinate_BallCurve dw  0205h       ,        6     

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
    
 ;   GetNames
    
    mov current_player, 2 ;Set player to player 2
        
;    DrawInterface
    
 ;   WriteOneFifthScreen              
         
    
    
   mov Ah,03h
   int 10h    
   mov Bh, 00
   mov Cx, 01  

   mov ah,1
   mov cx,2b0bh
   int 10h 
   
   
   Print RBTop,70,20h   
   Print RBCenterU,70,20h     
   Print RBCenterD,70,20h  
   Print RBBottom,70,20h 
   
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





   mov ah, 4ch
   int 21h
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
    cmp RBTop,0
      jz ENDRU
    DEC RBTop
    Print RBTop,70,20h
    Delete RBBottom,70
    DEC RBCenterU
    DEC RBCenterD
    DEC RBBottom
    ENDRU:ret
RightUp ENDP

RightDown PROC
    cmp RBBottom,14
      jz ENDRD
    INC RBBottom
    Print RBBottom,70,20h
    Delete RBTop,70
    INC RBCenterD
    INC RBCenterU
    INC RBTop
    ENDRD:ret
RightDown ENDP
END MAIN 
