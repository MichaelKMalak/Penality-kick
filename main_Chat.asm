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
	LOCAL getOutChat
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
	OpponentName 		db  'Player 2: ','$'
	
	write_status		db	?	;if 1=> I'm sending text, if 0=> I'm receiving text
	
											;****************************;
											;***** Module Variables *****;							 
											;****************************;
								;*****************************************************;
	invited db 0	;if there is a pending invitation value will be 1, otherwise it's 0
	chat_host db 0	;if Player 1 is the host it will be 1, if player 2 is the host it will be 2, otherwise it is 0
  
	invitation_key db 88h	;the key sent to the other player indicating a chat invitation
	accept_key db 11h		;the key if sent, the invitation is accepted
	refuse_key db 00h		;the key if sent, the invitation is rejected/refused
	
	invitation_msg db 'You have a pending chat invitation. Do you want to accept it? y or n$'
	instruction_msg db 'To send a chat invitation click on F2.$'
								;*****************************************************;
	
        .CODE
    MAIN PROC 
         MOV AX,@DATA
         MOV DS,AX
         MOV AX,0
		 
         ClearScreen 		 
         call Intialize_Port
		 
		 call InvitationModule	;Before starting the main program, we will handle the invitations first to be like a separate module from the program itself.
		 
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
           jnz noEsc
             
           call Exit
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
         
         
         call Exit
         
         
         mov ah, 4ch
		 int 21h
         
MAIN    ENDP   
;*********************************************************************************************************************************************;

												 ;**************************;
												 ;****Invitations Module****;
												 ;**************************;
;**********************;
;**** Module Start ****;							 
;**********************;

InvitationModule Proc
push ax
push bx
push cx
push dx
push ds

ClearScreen
   
   mov ah, 9
   LEA dx, instruction_msg
   int 21h
   
   mov ax, 0
   mov bx, 0
   mov cx, 0
   mov dx, 0
   Module_Loop:
    
	
        Module_Check: 
			call ModuleReceive
			
			cmp chat_host, 0
			jnz END_InvitationModule
            
			mov ah, 1
            int 16h
			jz Module_Check
		
		mov ah, 0
		int 16h  
		cmp al, 27		;ESCAPE
		jz ModuleEnd_Program
		
		cmp ah, 3Ch		;F2
		jnz Module_Loop
		
		cmp chat_host, 0
		jnz Module_Loop
		cmp invited, 1
		jz Module_Loop
		
		mov al, invitation_key
		call ModuleSend
		
	jmp Module_Loop  
	
	jmp END_InvitationModule
	ModuleEnd_Program:
    
	call Exit

	END_InvitationModule:
	ClearScreen
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
  	
	JZ END_ReceiveModule	;If not ready end

	;If data ready, fetch the data
  		mov dx , 03F8H
  		in al , dx 
		
	cmp chat_host, 0
	jnz END_ReceiveModule
	
	cmp al, invitation_key
	jnz Receive_Continue1
	;Invitation Received
	
		mov invited, 1
		Call ReceivedInvitation
	jmp END_ReceiveModule
	
	Receive_Continue1:
	cmp invited, 0
	jnz END_ReceiveModule
	cmp al, accept_key
	jnz Receive_Continue2
	;Invitation Accepted
	
		mov chat_host, 1
		mov invited, 0
		
	jmp END_ReceiveModule
	Receive_Continue2:
	cmp invited, 0
	jnz END_ReceiveModule
	cmp al, refuse_key
	jnz END_ReceiveModule
	;Invitation Refused
	
		mov chat_host, 0
		mov invited, 0
	
	END_ReceiveModule:ret
ModuleReceive Endp


ReceivedInvitation	PROC
	ClearScreen
	mov ah, 9
	LEA dx, invitation_msg
	int 21h
	
	ReceivedInvitation_Again:
	mov ah, 0
	int 16h
	
	ReceivedInvitation_Check_Y:
	cmp al, 'y'
	jnz ReceivedInvitation_Check_N
	;Accept Invitation
	
	mov al, accept_key
	mov chat_host, 2
	call ModuleSend
	
	jmp ReceivedInvitation_END
	ReceivedInvitation_Check_N:
	cmp al, 'n'
	jnz ReceivedInvitation_Again
	;Refuse Invitation
	
	mov al, refuse_key
	call ModuleSend

	mov invited, 0
	ClearScreen
	
	ReceivedInvitation_END:
	RET
ReceivedInvitation	ENDP

;**********************;
;***** Module End *****;							 
;**********************;

;*********************************************************************************************************************************************;





        
;----------------------------             
        
Intialize_Port Proc
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
                
                  cmp MyRow, 0
                  jnz notbeg1
                   jmp writeMyScreen_End
                  
                notbeg1:
                dec MyCol
                
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
		PrintP2	Proc
		
			push ax
			
			mov al, 'P'
			PrintCharAl ChatRow,ChatCol,ChatAtt
			ShiftCursorChat
			
			mov al, '2'
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
		PrintP2 ENDP
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
			call PrintP2
			
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
           MOV Ah,4CH
           INT 21H
           ret 
         Exit ENDP 

         
END MAIN
