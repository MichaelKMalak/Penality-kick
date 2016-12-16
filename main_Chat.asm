PrintStr MACRO Str
   push ax
   push dx
           
        mov ah,9
        mov dx, offset str
        int 21h
		
	pop dx
	pop ax           
ENDM PrintStr 
;----------------------------
PrintChar MACRO 
   push ax
           
        mov ah,2        ;charachter in Dl
        int 21h
		
	pop ax             
ENDM PrintChar 
;----------------------------
ReadStr MACRO Read
   push ax
   push dx
           
        mov ah,0Ah
        mov dx, offset Read
        int 21h
		
  pop dx
  pop ax          
ENDM ReadStr   
;----------------------------
ClearScreen MACRO    
   push ax
                              
           mov ax,0003
           int 10h 
		   
	pop ax		   
ENDM ClearScreen   
;----------------------------
SetCursor MACRO Row, Col
   push ax
   push bx
   push dx
           
            mov dh,Row
            mov dl,Col 
            mov bh,00
            mov ah,02
            int 10h
			
	pop dx
	pop bx
	pop ax
ENDM SetCursor  
;----------------------------
ShiftCursorMy MACRO 
   push ax
           
          inc MyCol
          
          cmp MyCol, 79
          jnz getOutMy
          
          mov al,0
          mov MyCol,al
          inc MyRow
          
          cmp MyRow, intialChatRow 
		  
          getOutMy:

	pop ax
ENDM ShiftCursorMy    
;----------------------------
ShiftCursorChat MACRO 
   push ax
             
          inc ChatCol
          
          cmp ChatCol, 79
          jnz getOutChat
          
          mov al,0
          mov ChatCol,al
          inc ChatRow
          
          cmp ChatRow, 26				;CHECK THAT
          jnz getOutChat
          
          call ScrollChatScreen
          dec ChatRow

          getOutChat:
	pop ax
ENDM ShiftCursorChat
;---------------------------- 
PrintHorizontalLine MACRO row, color
   push cx
                  
    mov cl, 0
    
    loop1:
        Print row, cl, color , ' '      
        inc cl
        cmp cl, 80
    jnz loop1           

	pop cx
ENDM PrintHorizontalLine 
;---------------------------- 
Print MACRO row, column, color, char    
  push ax
  push bx
  push cx

  
	SetCursor row, column	

	mov Ah, 09
	mov Al, char
	mov Bl, color
	mov Cx, 1h
	INT 10h  
   

  pop cx
  pop bx
  pop ax
 ENDM Print 
;----------------------------
PrintCharAl MACRO row, column ,color

	Print row, column, color, al 

ENDM PrintCharAl
;----------------------------

WriteEnter  MACRO row,column
   push ax
         
                inc row
                mov al, 00h
                mov column,al

  pop ax
ENDM WriteEnter 
;----------------------------  
AddSentToBuffer  MACRO
   push ax
   push bx
              
             
             mov bh,00h
             mov bl, BufferSize
             mov  ah,ToSendChar  
             mov  Buffer[bx],ah
             inc bl
             mov BufferSize,bl  
			 
	pop bx
	pop ax                   
ENDM AddSentToBuffer

;---------------------------- 
 
        .MODEL SMALL
        .STACK 64
        .DATA	  
	
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
    
	MyName 				db  'Player 1: ','$'
	OpponentName 		db  'Player 1: ','$'
	
	ComputerNumber		db  1				;always equal 1 (only equals two when: FirstTimeToSend=0 && FirstTimeToRecieve=1)	
	FirstTimeToSend		db	0	
	FirstTimeToRecieve	db	0		
	
        .CODE
    MAIN PROC 
         MOV AX,@DATA
         MOV DS,AX
         
         ClearScreen 		 
         call ColorScreen

         mov MyRow,intialMyRow
         mov ChatRow,intialChatRow
         
         SetCursor MyRow, MyCol
         PrintHorizontalLine intialMyRow, MyAtt ;Draws a line on the row defined in al
         call IntializeCOM 

    Again:     
          
          get:
           mov  dx, 3FDH            
           in   al,dx
           test al,1
           JZ send
           
		   cmp FirstTimeToRecieve, 00
		   jnz Chatting
		   mov al, 1
		   mov FirstTimeToRecieve, al
				cmp FirstTimeToSend, 00
				jnz Chatting
					mov al, 2
					mov ComputerNumber, al
		   Chatting:
           mov dx, 03f8h
           in  al,dx 
           mov GetChar,al
           
           call writeChatScreen          
          
          send:
           mov ah,01h
           int 16h
           JZ get
           
           mov ah,00
           int 16h
           
           mov ToSendChar, al
           
           cmp ah,1
           jnz noEsc
             
           call Exit
           noEsc:

           cmp al,0Dh
           jnz noEnter
               
               cmp BufferSize,0
               jz  Again  
               
               call SendOperation
			   mov	FirstTimeToSend, 1
               mov MyRow, intialMyRow
               mov MyCol, 0
               call clearMyPart  
               
               jmp Again
           noEnter:
           
           cmp BufferSize, 78
           jz Again
           
           call writeMyScreen 
           
                               
          
         Jmp Again 
         
         
         call Exit
         
         
         mov ah, 4ch
		 int 21h
         
MAIN    ENDP   
        
;----------------------------             
        
        IntializeCOM PROC 
            
            mov dx,3fbh
            mov al,10000000b
            out dx,al  
            
            mov al,0ch
            mov dx,3f8h
            out dx,al
            
            mov al,00h
            mov dx,3f9h
            out dx,al
            
            mov dx,3fbh
            mov al,00011011b
            out dx,al
            
            ret
        IntializeCOM ENDP 

;----------------------------
      ColorScreen PROC 
           push ax
           push bx
           push cx
           push dx
           
           mov ah,7h        ;Scroll down
           mov al, 25
		   sub al, intialChatRow	;number of lines to scroll   	
           mov bh,ChatAtt   ;blanck lines at bottom of window
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
           
           ;send arrow after string
           ;mov  al,16
           ;mov  ToSendChar,al
           ;call  SendOneChar
           
           ;mov  GetChar,al
           ;call  writeChatScreen
           cmp ComputerNumber, 1
		   jnz FromComputer2
		   
		   mov  al,'1'
           mov  ToSendChar,al
		   mov  GetChar,al
           call  SendOneChar
		   
		   mov  al,'1'
		   mov  GetChar,al
           call  writeChatScreen
		   jmp TheMessage
           FromComputer2:
		   
		   mov  al,'2'
           mov  ToSendChar,al
		   mov  GetChar,al
           call  SendOneChar
		   
		   mov  al,'2'
		   mov  GetChar,al
           call  writeChatScreen
		   
		   TheMessage:
		   mov  al,' '
           mov  ToSendChar,al
		   mov  GetChar,al
           call  SendOneChar
		   
		   mov  al,' '
		   mov  GetChar,al
           call  writeChatScreen
		   
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
                
                  cmp MyRow, 0
                  jnz notbeg1
                   jmp writeMyScreen_End
                  
                notbeg1:
                dec MyCol
                
                ;Print space 
                     push ax                       
                     mov al,' '
                     PrintCharAl MyRow,MyCol,myAtt   
                     pop ax 
                
                
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
        writeChatScreen PROC                       
            
           push ax
           push bx
           push cx
           push dx
             
            SetCursor ChatRow,ChatCol
           
            
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
                    ;Print space 
                     push ax                       
                     mov al,' '
                     PrintCharAl ChatRow,ChatCol,ChatAtt   
                     pop ax    
                      
           writeChatScreen_End:
           pop dx
           pop cx
           pop bx
           pop ax

            ret
         writeChatScreen_Continue1: 

             PrintCharAl ChatRow,ChatCol,ChatAtt
             ShiftCursorChat   
            
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
           MOV Ah,4CH
           INT 21H
           ret 
         Exit ENDP 

         
END MAIN
        
        