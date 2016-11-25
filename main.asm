 include macros.inc

 org 100h
   
   jmp start
   
   RBTop       equ 0905h
   RBCenterU   equ 0906h
   RBCenter    equ 0907h
   RBCenterD   equ 0908h
   RBBottom    equ 0909h
   
   
start:
   mov ax,@data	
   mov ds,ax              
      
  
   mov Ah,03h
   int 10h    
   mov Bh, 00
   mov Cx, 01  

   mov ah,1
   mov cx,2b0bh
   int 10h 
        
        
   mov [RBTop],10
   mov [RBCenterU],11 
   mov [RBCenterD],12
   mov [RBBottom],13  
   
   Print [RBTop],70,0c0h   
   Print [RBCenterU],70,0c0h     
   Print [RBCenterD],70,0c0h  
   Print [RBBottom],70,0c0h 
   
   
   activeLoop:
   
    mov ah, 01h
    int 16h         ;ZF=1 no charachter in buffer
    jz activeLoop
        mov Ah,00
        int 16h     ;waits for buffer till it's not empty
        CALL Move            

   JMP activeLoop
ret    

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
    cmp [RBTop],0
      jz ENDRU
    DEC [RBTop]
    Print [RBTop],70,0c0h
    Delete [RBBottom],70
    DEC [RBCenterU]
    DEC [RBCenterD]
    DEC [RBBottom]
    ENDRU:ret
RightUp ENDP

RightDown PROC
    cmp [RBBottom],18
      jz ENDRD
    INC [RBBottom]
    Print [RBBottom],70,0c0h
    Delete [RBTop],70
    INC [RBCenterD]
    INC [RBCenterU]
    INC [RBTop]
    ENDRD:ret
RightDown ENDP
                       	
  
  

  
	
