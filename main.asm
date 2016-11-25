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
 p1_name db 30, ?, 30 dup('$')
 p2_name db 30, ?, 30 dup('$')
  
 p1_score db 0  ;Intially player 1 score is 0
 p2_score db 0  ;Intially player 2 score is 0
 
 GoalDim db 70, 3, 73, 11 ;X1,Y1, X2,Y2
 current_player db 1  
 
.CODE   
MAIN    PROC   
    
    mov ax, @DATA
    mov ds, ax
    mov ax, 0
    
    mov ah, 0
    mov al, 3
    int 10h 
    
    GetNames
    
    mov current_player, 2 ;Set player to player 2
        
    DrawInterface
    
    WriteOneFifthScreen              
      
  
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
   
   
   activeLoop:
   
    mov ah, 01h
    int 16h         ;ZF=1 no charachter in buffer
    jz activeLoop
        mov Ah,00
        int 16h     ;waits for buffer till it's not empty
        CALL Move            

   JMP activeLoop

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
